;;; supersonic-local.el --- Local music files provider for supersonic.el -*- lexical-binding: t; -*-

;; Author: justchokingaround
;; Assisted-by: Claude:claude-opus-5-5
;; URL: https://github.com/systemfreund/supersonic.el
;; Keywords: multimedia

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The `local' library provider: the music files under
;; `supersonic-local-directories', browsed and played with no server,
;; e.g. while it is unreachable.  Played through mpv, which plays the
;; files directly.
;;
;; Tags are read with ffprobe, once per file: the index is cached in
;; `supersonic-cache-path' and a rescan only probes files that are new
;; or changed.  Missing tags are guessed from an Artist/Album/Track
;; directory layout.  Cover art is a cover.jpg-like file next to the
;; tracks, or else the art embedded in them; lyrics are an .lrc file
;; next to the track, or else its lyrics tag.  The `recent' album list
;; is what was played from this library.
;;
;; `supersonic-local-toggle' switches between this provider and the one
;; active before.  With `supersonic-local-offer-fallback', a request to
;; the server failing for want of a connection offers that switch.
;;
;; Ids are file names for tracks, and strings of their own for albums
;; and artists; like every provider's ids, nothing else reads them.

;;; Code:

(require 'aio)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(require 'supersonic-custom)
(require 'supersonic-provider)
(require 'supersonic-playback)

(declare-function supersonic-provider-switch "supersonic")
(declare-function supersonic-lyrics-parse-lrc "supersonic-lyrics")

;;;
;;; Reading tags
;;;

(defun supersonic-local--normalize-key (key)
  "Return tag KEY lowercased with all but letters and digits dropped.
Tag names vary with the format and the tagger: \"Album Artist\",
\"album_artist\" and \"ALBUMARTIST\" all become \"albumartist\"."
  (replace-regexp-in-string "[^[:alnum:]]" "" (downcase (if (symbolp key) (symbol-name key) key))))

(defun supersonic-local--tags (probe)
  "Return the tags in PROBE, ffprobe's parsed JSON, as an alist of strings.
Keys are normalized by `supersonic-local--normalize-key'.  The
container's tags win over the audio stream's, which some formats, such
as Ogg, keep theirs in."
  (let ((tags nil))
    (dolist (source (cons (alist-get 'tags (alist-get 'format probe))
                          (mapcar (lambda (stream) (alist-get 'tags stream))
                                  (seq-filter (lambda (stream) (equal (alist-get 'codec_type stream) "audio"))
                                              (alist-get 'streams probe)))))
      (dolist (pair source)
        (let ((key (supersonic-local--normalize-key (car pair))))
          (when (and (stringp (cdr pair)) (not (assoc key tags)))
            (push (cons key (cdr pair)) tags)))))
    (nreverse tags)))

(defun supersonic-local--number (string)
  "Return the leading number of STRING, such as 7 for \"7/10\", or nil."
  (and string (string-match "\\`[[:space:]]*\\([0-9]+\\)" string)
       (string-to-number (match-string 1 string))))

(defun supersonic-local--guess (file root)
  "Guess the artist, album, track number and title of FILE from its name.
ROOT is the library directory FILE is under; an Artist/Album/Track
layout below it is assumed.  Returns a plist of what could be guessed."
  (let* ((parts (split-string (file-relative-name file root) "/" t))
         (base (file-name-sans-extension (car (last parts))))
         (number (and (string-match "\\`\\([0-9]+\\)[[:space:]]*[-.][[:space:]]*" base)
                      (string-to-number (match-string 1 base))))
         (title (if number (substring base (match-end 0)) base))
         (clean (lambda (dir) (string-trim (replace-regexp-in-string "([0-9]\\{4\\})" "" dir)))))
    (list :title title
          :track number
          :artist (and (>= (length parts) 2) (car parts))
          :album (and (>= (length parts) 3) (funcall clean (car (last parts 2)))))))

(defun supersonic-local--folder-art (directory)
  "Return the cover art image file in DIRECTORY, or nil."
  (let ((case-fold-search t))
    (and (file-directory-p directory)
         (car (directory-files directory t "\\`\\(cover\\|folder\\|front\\|album\\)\\.\\(jpe?g\\|png\\|webp\\)\\'")))))

(defun supersonic-local--track (file root probe attributes)
  "Return the track plist of FILE under ROOT, from PROBE and ATTRIBUTES.
PROBE is ffprobe's parsed JSON for FILE, nil if it could not be read;
ATTRIBUTES are FILE's `file-attributes'."
  (let* ((tags (supersonic-local--tags probe))
         (tag (lambda (&rest keys)
                (seq-some (lambda (key)
                            (let ((value (cdr (assoc key tags))))
                              (and value (not (string-empty-p (string-trim value))) (string-trim value))))
                          keys)))
         (guess (supersonic-local--guess file root))
         (duration (let ((d (alist-get 'duration (alist-get 'format probe))))
                     (and d (round (string-to-number d)))))
         (embedded (seq-some (lambda (stream)
                               (eql 1 (alist-get 'attached_pic (alist-get 'disposition stream))))
                             (alist-get 'streams probe)))
         (folder-art (supersonic-local--folder-art (file-name-directory file)))
         (artist (or (funcall tag "artist") (plist-get guess :artist)))
         (year (supersonic-local--number (funcall tag "date" "year" "originaldate"))))
    (list :id file
          :title (or (funcall tag "title") (plist-get guess :title))
          :artist artist
          :album-artist (or (funcall tag "albumartist") (plist-get guess :artist) artist)
          :album (or (funcall tag "album") (plist-get guess :album))
          :duration duration
          :track (or (supersonic-local--number (funcall tag "track" "tracknumber")) (plist-get guess :track))
          :disc (supersonic-local--number (funcall tag "disc" "discnumber"))
          :year year
          :genre (funcall tag "genre")
          :art (cond (folder-art (concat "file:" folder-art))
                     (embedded (concat "embedded:" file)))
          :suffix (downcase (or (file-name-extension file) ""))
          :size (file-attribute-size attributes)
          :mtime (float-time (file-attribute-modification-time attributes))
          :lyrics-tag (seq-some (lambda (pair)
                                  (and (string-match-p "lyrics" (car pair))
                                       (not (string-empty-p (string-trim (cdr pair))))
                                       (cdr pair)))
                                tags))))

(defun supersonic-local--probe (file)
  "Return a promise resolving to ffprobe's parsed JSON for FILE, or nil."
  (let ((promise (aio-promise))
        (buffer (generate-new-buffer " *supersonic-ffprobe*")))
    (condition-case err
        (make-process
         :name "supersonic-ffprobe"
         :buffer buffer
         :command (list supersonic-local-ffprobe "-v" "quiet" "-print_format" "json"
                        "-show_format" "-show_streams" file)
         :coding 'utf-8
         :connection-type 'pipe
         :noquery t
         :stderr nil
         :sentinel
         (lambda (process _event)
           (unless (process-live-p process)
             (let ((json (and (eq (process-status process) 'exit)
                              (zerop (process-exit-status process))
                              (with-current-buffer buffer
                                (goto-char (point-min))
                                (condition-case nil
                                    (json-parse-buffer :object-type 'alist :array-type 'list
                                                       :null-object nil :false-object nil)
                                  (error nil))))))
               (kill-buffer buffer)
               (aio-resolve promise (lambda () json))))))
      (error
       (kill-buffer buffer)
       (aio-resolve promise (lambda () (signal (car err) (cdr err))))))
    promise))

;;;
;;; The index
;;;

(defvar supersonic-local--tracks nil
  "Hash table of file name to track plist, nil until the index is loaded.")
(defvar supersonic-local--albums nil "Hash table of album id to album plist.")
(defvar supersonic-local--artists nil "Hash table of artist id to artist plist.")
(defvar supersonic-local--scan nil "Promise of the scan running, or nil.")
(defvar supersonic-local--history nil "Album ids played, most recent first.")
(defvar supersonic-local--counts nil "Hash table of album id to times played.")

(defconst supersonic-local--index-version 1
  "Version of the index cache file's format; a different one is rescanned.")

(defun supersonic-local--index-file ()
  "Return the file the local index is cached in."
  (expand-file-name "local-index.eld" supersonic-cache-path))

(defun supersonic-local--history-file ()
  "Return the file the local play history is kept in."
  (expand-file-name "local-history.eld" supersonic-cache-path))

(defun supersonic-local--counts-file ()
  "Return the file the local play counts are kept in."
  (expand-file-name "local-counts.eld" supersonic-cache-path))

(defun supersonic-local--load-counts ()
  "Load the play counts, an alist on disk, into `supersonic-local--counts'."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (pair (supersonic-local--read-file (supersonic-local--counts-file)))
      (puthash (car pair) (cdr pair) table))
    (setq supersonic-local--counts table)))

(defun supersonic-local--read-file (file)
  "Return the Lisp object in FILE, or nil if it cannot be read."
  (when (file-readable-p file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file)
          (read (current-buffer)))
      (error nil))))

(defun supersonic-local--write-file (file object)
  "Write OBJECT to FILE, logging rather than signalling a failure."
  (condition-case err
      (progn
        (make-directory (file-name-directory file) t)
        (with-temp-file file
          (let ((print-length nil)
                (print-level nil)
                (coding-system-for-write 'utf-8))
            (insert ";; -*- coding: utf-8 -*-\n")
            (prin1 object (current-buffer)))))
    (error (supersonic--log "could not write %s: %s" file (error-message-string err)))))

(defun supersonic-local--album-key (track)
  "Return the id of TRACK's album.
Case is ignored, so that \"MIKA\" and \"Mika\" tags make one album."
  (downcase (concat "album:" (or (plist-get track :album-artist) "") "\x1f" (or (plist-get track :album) ""))))

(defun supersonic-local--artist-key (name)
  "Return the id of the artist called NAME, ignoring case."
  (downcase (concat "artist:" (or name ""))))

(defun supersonic-local--most-common (strings)
  "Return the string occurring most often in STRINGS, the first on a tie."
  (let ((counts nil))
    (dolist (string strings)
      (when string
        (cl-incf (alist-get string counts 0 nil #'equal))))
    (car (seq-reduce (lambda (best pair) (if (> (cdr pair) (cdr best)) pair best))
                     (nreverse counts) (cons nil 0)))))

(defun supersonic-local--rebuild ()
  "Rebuild the album and artist tables from the track index."
  (let ((albums (make-hash-table :test #'equal))
        (artists (make-hash-table :test #'equal)))
    (maphash
     (lambda (_file track)
       (let* ((id (supersonic-local--album-key track))
              (album (or (gethash id albums)
                         (puthash id (list :id id
                                           :name (or (plist-get track :album) "(no album)")
                                           :artist (plist-get track :album-artist)
                                           :year (plist-get track :year)
                                           :art (plist-get track :art)
                                           :mtime 0
                                           :tracks nil)
                                  albums))))
         (setq album (plist-put album :tracks (cons track (plist-get album :tracks))))
         (setq album (plist-put album :mtime (max (plist-get album :mtime) (or (plist-get track :mtime) 0))))
         (unless (plist-get album :art)
           (setq album (plist-put album :art (plist-get track :art))))
         (puthash id album albums)))
     supersonic-local--tracks)
    (maphash
     (lambda (id album)
       (let ((tracks (plist-get album :tracks)))
         (setq album (plist-put album :tracks (sort tracks #'supersonic-local--track<)))
         (setq album (plist-put album :name (or (supersonic-local--most-common
                                                  (mapcar (lambda (track) (plist-get track :album)) tracks))
                                                 "(no album)")))
         (setq album (plist-put album :artist (supersonic-local--most-common
                                               (mapcar (lambda (track) (plist-get track :album-artist)) tracks))))
         (setq album (plist-put album :song-count (length tracks)))
         (setq album (plist-put album :duration (apply #'+ (mapcar (lambda (track) (or (plist-get track :duration) 0))
                                                                    tracks))))
         (setq album (plist-put album :artist-id (supersonic-local--artist-key (plist-get album :artist))))
         (puthash id album albums))
       (let* ((name (plist-get album :artist))
              (artist-id (supersonic-local--artist-key name))
              (artist (or (gethash artist-id artists)
                          (list :id artist-id :name (or name "(unknown artist)") :art nil :albums nil))))
         (setq artist (plist-put artist :albums (cons id (plist-get artist :albums))))
         (setq artist (plist-put artist :album-count (length (plist-get artist :albums))))
         (setq artist (plist-put artist :spellings
                                 (append (mapcar (lambda (track) (plist-get track :album-artist))
                                                 (plist-get album :tracks))
                                         (plist-get artist :spellings))))
         (setq artist (plist-put artist :name (or (supersonic-local--most-common (plist-get artist :spellings))
                                                  "(unknown artist)")))
         (unless (plist-get artist :art)
           (setq artist (plist-put artist :art (plist-get album :art))))
         (puthash artist-id artist artists)))
     albums)
    (setq supersonic-local--albums albums
          supersonic-local--artists artists)))

(defun supersonic-local--track< (a b)
  "Return non-nil if track A comes before track B on their album."
  (let ((da (or (plist-get a :disc) 1)) (db (or (plist-get b :disc) 1))
        (ta (or (plist-get a :track) most-positive-fixnum)) (tb (or (plist-get b :track) most-positive-fixnum)))
    (cond ((/= da db) (< da db))
          ((/= ta tb) (< ta tb))
          (t (string< (plist-get a :id) (plist-get b :id))))))

(defun supersonic-local--files ()
  "Return every music file under `supersonic-local-directories', with its root.
Each element is (FILE . ROOT).  Hidden files and directories are
skipped, and so is every file `supersonic-local-ignore-regexp' matches."
  (let ((case-fold-search t)
        (regexp (concat "\\.\\(" (regexp-opt supersonic-local-extensions) "\\)\\'"))
        (files nil))
    (dolist (directory supersonic-local-directories)
      (let ((root (file-name-as-directory (file-truename (expand-file-name directory)))))
        (when (file-directory-p root)
          (dolist (file (directory-files-recursively
                         root regexp nil
                         (lambda (dir) (not (string-prefix-p "." (file-name-nondirectory dir))))
                         t))
            (unless (or (string-prefix-p "." (file-name-nondirectory file))
                        (and supersonic-local-ignore-regexp
                             (string-match-p supersonic-local-ignore-regexp file)))
              (push (cons file root) files))))))
    (nreverse files)))

(aio-defun
 supersonic-local--rescan ()
 "Bring the index up to date with the files on disk, probing only changed ones."
 (unless (and supersonic-local-ffprobe (file-executable-p supersonic-local-ffprobe))
   (user-error "Reading local tags needs ffprobe; see `supersonic-local-ffprobe'"))
 (let* ((old (or supersonic-local--tracks (make-hash-table :test #'equal)))
        (new (make-hash-table :test #'equal))
        (pending nil)
        (probed 0))
   (dolist (entry (supersonic-local--files))
     (let* ((file (car entry))
            (attributes (file-attributes file))
            (known (gethash file old)))
       (if (and known attributes
                (equal (plist-get known :size) (file-attribute-size attributes))
                (equal (plist-get known :mtime) (float-time (file-attribute-modification-time attributes))))
           (puthash file known new)
         (when attributes
           (push (list file (cdr entry) attributes) pending)))))
   (when pending
     (message "[Supersonic] Reading tags of %d local file(s)..." (length pending)))
   ;; A pool of workers, each probing one file at a time off the shared list.
   (let ((workers nil))
     (dotimes (_ (max 1 supersonic-local-scan-concurrency))
       (push (funcall (aio-lambda ()
                        (while pending
                          (pcase-let ((`(,file ,root ,attributes) (pop pending)))
                            (let ((probe (condition-case nil
                                             (aio-await (supersonic-local--probe file))
                                           (error nil))))
                              (puthash file (supersonic-local--track file root probe attributes) new)
                              (setq probed (1+ probed)))))))
             workers))
     (dolist (worker workers)
       (aio-await worker)))
   (setq supersonic-local--tracks new)
   (supersonic-local--rebuild)
   (supersonic-local--write-file (supersonic-local--index-file)
                                 (list :version supersonic-local--index-version
                                       :directories (supersonic-local--roots)
                                       :tracks (hash-table-values new)))
   (message "[Supersonic] Local library: %d track(s)%s" (hash-table-count new)
            (if (> probed 0) (format ", %d read anew" probed) ""))
   new))

(defun supersonic-local--roots ()
  "Return `supersonic-local-directories' as absolute directory names."
  (mapcar (lambda (directory) (file-name-as-directory (expand-file-name directory)))
          supersonic-local-directories))

(defun supersonic-local--load-index ()
  "Load the cached index, returning non-nil if there was a usable one.
One built for other `supersonic-local-directories' is not."
  (let ((data (supersonic-local--read-file (supersonic-local--index-file))))
    (when (and (eql (plist-get data :version) supersonic-local--index-version)
               (equal (plist-get data :directories) (supersonic-local--roots)))
      (let ((tracks (make-hash-table :test #'equal)))
        (dolist (track (plist-get data :tracks))
          (puthash (plist-get track :id) track tracks))
        (setq supersonic-local--tracks tracks)
        (supersonic-local--rebuild)
        t))))

(defun supersonic-local--start-scan ()
  "Start a rescan unless one is running, and return its promise."
  (or supersonic-local--scan
      (setq supersonic-local--scan
            (funcall (aio-lambda ()
                       (condition-case err
                           (prog1 (aio-await (supersonic-local--rescan))
                             (setq supersonic-local--scan nil))
                         (error
                          (setq supersonic-local--scan nil)
                          (signal (car err) (cdr err)))))))))

(aio-defun
 supersonic-local--ensure-index ()
 "Make sure the index is loaded, scanning only if there is no cached one.
A cached index is used right away, and refreshed in the background
once per session."
 (unless supersonic-local--tracks
   (unless supersonic-local--history
     (setq supersonic-local--history (supersonic-local--read-file (supersonic-local--history-file))))
   (if (supersonic-local--load-index)
       (ignore (supersonic-local--start-scan))
     (aio-await (supersonic-local--start-scan)))))

;;;###autoload
(defun supersonic-local-rescan ()
  "Rescan `supersonic-local-directories' for new, changed and removed files."
  (interactive)
  (ignore (supersonic-local--start-scan)))

;;;
;;; Operations
;;;

(defun supersonic-local--public (item)
  "Return ITEM, a track, album or artist plist, without the index's own keys.
A track gets the ids of its album and album artist added."
  (let ((public (cl-loop for (key value) on item by #'cddr
                         unless (memq key '(:tracks :albums :spellings :lyrics-tag :mtime :starred))
                         append (list key value))))
    (when (gethash (plist-get item :id) (supersonic-local--favourites))
      (setq public (append public (list :starred t))))
    (if (and (plist-get item :title) (string-prefix-p "/" (or (plist-get item :id) "")))
        (append public (list :album-id (supersonic-local--album-key item)
                             :artist-id (supersonic-local--artist-key (plist-get item :album-artist))))
      public)))

(defun supersonic-local--name< (a b)
  "Return non-nil if plist A's name sorts before B's, ignoring case."
  (string-collate-lessp (or (plist-get a :name) "") (or (plist-get b :name) "") nil t))

(aio-defun
 supersonic-local--artists-op () "Resolve to every local artist, by name."
 (aio-await (supersonic-local--ensure-index))
 (mapcar #'supersonic-local--public (sort (hash-table-values supersonic-local--artists) #'supersonic-local--name<)))

(aio-defun
 supersonic-local--artist-albums (id) "Resolve to the albums of local artist ID, oldest first."
 (aio-await (supersonic-local--ensure-index))
 (let ((albums (mapcar (lambda (album-id) (gethash album-id supersonic-local--albums))
                       (plist-get (gethash id supersonic-local--artists) :albums))))
   (mapcar #'supersonic-local--public
           (sort (delq nil albums)
                 (lambda (a b)
                   (let ((ya (or (plist-get a :year) 0)) (yb (or (plist-get b :year) 0)))
                     (if (/= ya yb) (< ya yb) (supersonic-local--name< a b))))))))

(aio-defun
 supersonic-local--album-list (type count &optional offset)
 "Resolve to at most COUNT local albums of list TYPE, after OFFSET."
 (aio-await (supersonic-local--ensure-index))
 (let* ((all (hash-table-values supersonic-local--albums))
        (albums
         (pcase type
           ('newest (sort all (lambda (a b) (> (plist-get a :mtime) (plist-get b :mtime)))))
           ('random
            (let ((vector (vconcat all)))
              (cl-loop for i from (1- (length vector)) downto 1
                       do (cl-rotatef (aref vector i) (aref vector (random (1+ i)))))
              (append vector nil)))
           ('recent (delq nil (mapcar (lambda (id) (gethash id supersonic-local--albums))
                                      supersonic-local--history)))
           ('frequent
            (unless supersonic-local--counts
              (supersonic-local--load-counts))
            (let (played)
              (maphash (lambda (id count)
                         (let ((album (gethash id supersonic-local--albums)))
                           (when album (push (cons count album) played))))
                       supersonic-local--counts)
              (mapcar #'cdr (sort played (lambda (a b) (> (car a) (car b)))))))
           (_ (user-error "The local files have no `%s' album list" type)))))
   (mapcar #'supersonic-local--public (seq-take (nthcdr (or offset 0) albums) count))))

(aio-defun
 supersonic-local--album-tracks (id) "Resolve to the tracks of local album ID, in album order."
 (aio-await (supersonic-local--ensure-index))
 (mapcar #'supersonic-local--public (plist-get (gethash id supersonic-local--albums) :tracks)))

(aio-defun
 supersonic-local--track-op (id) "Resolve to local track ID."
 (aio-await (supersonic-local--ensure-index))
 (let ((track (gethash id supersonic-local--tracks)))
   (unless track
     (error "No local track %s" id))
   (supersonic-local--public track)))

(defun supersonic-local--matches-p (words &rest fields)
  "Return non-nil if every one of WORDS occurs in one of FIELDS, ignoring case."
  (let ((haystack (downcase (string-join (delq nil fields) " "))))
    (seq-every-p (lambda (word) (string-search word haystack)) words)))

(aio-defun
 supersonic-local--search (query)
 "Resolve to the local artists, albums and tracks matching every word of QUERY."
 (aio-await (supersonic-local--ensure-index))
 (let ((words (split-string (downcase query) nil t))
       (limit 50))
   (list
    :artists (mapcar #'supersonic-local--public
                     (seq-take (sort (seq-filter (lambda (a) (supersonic-local--matches-p words (plist-get a :name)))
                                                 (hash-table-values supersonic-local--artists))
                                     #'supersonic-local--name<)
                               limit))
    :albums (mapcar #'supersonic-local--public
                    (seq-take (sort (seq-filter (lambda (a) (supersonic-local--matches-p
                                                             words (plist-get a :name) (plist-get a :artist)))
                                                (hash-table-values supersonic-local--albums))
                                    #'supersonic-local--name<)
                              limit))
    :tracks (mapcar #'supersonic-local--public
                    (seq-take (seq-filter (lambda (track)
                                            (supersonic-local--matches-p
                                             words (plist-get track :title) (plist-get track :artist)
                                             (plist-get track :album)))
                                          (sort (hash-table-values supersonic-local--tracks)
                                                (lambda (a b) (string< (plist-get a :id) (plist-get b :id)))))
                              limit)))))

(aio-defun
 supersonic-local--stream-url (id &optional _format)
 "Resolve to the file local track ID is played from: its own name."
 (unless (file-readable-p id)
   (error "Local file %s is gone" id))
 id)

(defun supersonic-local--scrobble (id now-playing)
  "Remember the album of local track ID as played, when it starts.
NOW-PLAYING is non-nil when it has just started; that is when it counts."
  (when (and now-playing supersonic-local--tracks)
    (let ((track (gethash id supersonic-local--tracks)))
      (when track
        (let ((album (supersonic-local--album-key track)))
          (setq supersonic-local--history
                (seq-take (cons album (delete album supersonic-local--history)) 100))
          (supersonic-local--write-file (supersonic-local--history-file) supersonic-local--history)
          (unless supersonic-local--counts
            (supersonic-local--load-counts))
          (puthash album (1+ (gethash album supersonic-local--counts 0)) supersonic-local--counts)
          (let (pairs)
            (maphash (lambda (key count) (push (cons key count) pairs)) supersonic-local--counts)
            (supersonic-local--write-file (supersonic-local--counts-file) pairs)))))))

(defun supersonic-local--embedded-art (file)
  "Return a promise resolving to the bytes of the art embedded in FILE."
  (let ((promise (aio-promise))
        (buffer (generate-new-buffer " *supersonic-art*")))
    (with-current-buffer buffer
      (set-buffer-multibyte nil))
    (make-process
     :name "supersonic-embedded-art"
     :buffer buffer
     :command (list supersonic-local-ffmpeg "-v" "quiet" "-i" file
                    "-an" "-map" "0:v:0" "-c:v" "copy" "-f" "image2pipe" "-")
     :coding 'binary
     :connection-type 'pipe
     :noquery t
     :stderr nil
     :sentinel
     (lambda (process _event)
       (unless (process-live-p process)
         (let ((data (with-current-buffer buffer (buffer-string)))
               (ok (and (eq (process-status process) 'exit) (zerop (process-exit-status process)))))
           (kill-buffer buffer)
           (aio-resolve promise
                        (lambda ()
                          (if (and ok (> (length data) 0))
                              data
                            (error "No art embedded in %s" file))))))))
    promise))

(aio-defun
 supersonic-local--cover-art (art _size)
 "Resolve to the bytes of local cover art ART, a file or a track's embedded art."
 (cond
  ((string-prefix-p "file:" art)
   (with-temp-buffer
     (set-buffer-multibyte nil)
     (insert-file-contents-literally (substring art 5))
     (buffer-string)))
  ((string-prefix-p "embedded:" art)
   (aio-await (supersonic-local--embedded-art (substring art 9))))
  (t (error "Unknown local art %s" art))))

(aio-defun
 supersonic-local--lyrics (id)
 "Resolve to the lyrics of local track ID: an .lrc file next to it, else its tag."
 (require 'supersonic-lyrics)
 (let* ((case-fold-search t)
        (base (file-name-sans-extension (file-name-nondirectory id)))
        (lrc (seq-find (lambda (file) (string-equal (file-name-sans-extension (file-name-nondirectory file)) base))
                       (directory-files (file-name-directory id) t "\\.lrc\\'"))))
   (or (and lrc
            (let ((lyrics (supersonic-local--read-lrc lrc "lrc file"))
                  ;; TRACK.en.lrc and the like: a translation.
                  (translations
                   (delq nil (mapcar (lambda (code)
                                       (let ((file (seq-find (lambda (f) (string-equal
                                                                          (file-name-nondirectory f)
                                                                          (concat base "." code ".lrc")))
                                                             (directory-files (file-name-directory id) t "\\.lrc\\'"))))
                                         (and file (append (list :lang code)
                                                           (supersonic-local--read-lrc file "lrc file")))))
                                     supersonic-lyrics-translation-codes))))
              (if (and lyrics translations)
                  (append lyrics (list :alternatives translations))
                lyrics)))
       (progn
         (aio-await (supersonic-local--ensure-index))
         (supersonic-lyrics-parse-lrc (plist-get (gethash id supersonic-local--tracks) :lyrics-tag) "tags")))))

(defun supersonic-local--read-lrc (file source)
  "Return the lyrics in FILE, an .lrc or plain text file, from SOURCE."
  (supersonic-lyrics-parse-lrc
   (with-temp-buffer
     (let ((coding-system-for-read 'utf-8))
       (insert-file-contents file))
     (buffer-string))
   source))

;;;; Playlists, as .m3u files

(defun supersonic-local--playlist-directory ()
  "Return the directory local playlists are kept in."
  (file-name-as-directory
   (or supersonic-local-playlist-directory
       (expand-file-name "Playlists" (or (car supersonic-local-directories)
                                         (user-error "Set `supersonic-local-directories' first"))))))

(defun supersonic-local--playlist-file (id)
  "Return the .m3u file of playlist ID."
  (unless (string-prefix-p "playlist:" id)
    (error "Not a local playlist: %s" id))
  (substring id 9))

(defun supersonic-local--read-playlist (file)
  "Return the track files listed in the .m3u FILE, in order."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents file))
    (let ((directory (file-name-directory file))
          (files nil))
      (dolist (line (split-string (buffer-string) "\r?\n" t "[[:space:]]+"))
        (unless (string-prefix-p "#" line)
          (push (expand-file-name line directory) files)))
      (nreverse files))))

(defun supersonic-local--write-playlist (file track-files)
  "Write TRACK-FILES to the .m3u FILE, in order."
  (make-directory (file-name-directory file) t)
  (let ((coding-system-for-write 'utf-8))
    (with-temp-file file
      (insert "#EXTM3U\n")
      (dolist (track track-files)
        (insert track "\n")))))

(defun supersonic-local--playlist-plist (file)
  "Return the playlist plist of the .m3u FILE."
  (let* ((tracks (supersonic-local--read-playlist file))
         (known (delq nil (mapcar (lambda (track) (and supersonic-local--tracks (gethash track supersonic-local--tracks)))
                                  tracks))))
    (list :id (concat "playlist:" file)
          :name (file-name-sans-extension (file-name-nondirectory file))
          :song-count (length tracks)
          :duration (apply #'+ (mapcar (lambda (track) (or (plist-get track :duration) 0)) known))
          :art (plist-get (car known) :art))))

(aio-defun
 supersonic-local--playlists () "Resolve to the playlists in the playlist directory."
 (aio-await (supersonic-local--ensure-index))
 (let ((directory (supersonic-local--playlist-directory)))
   (and (file-directory-p directory)
        (mapcar #'supersonic-local--playlist-plist
                (directory-files directory t "\\.m3u8?\\'")))))

(aio-defun
 supersonic-local--playlist-tracks (id) "Resolve to the tracks of local playlist ID still found."
 (aio-await (supersonic-local--ensure-index))
 (delq nil (mapcar (lambda (file)
                     (let ((track (gethash file supersonic-local--tracks)))
                       (and track (supersonic-local--public track))))
                   (supersonic-local--read-playlist (supersonic-local--playlist-file id)))))

(defun supersonic-local--playlist-name-file (name)
  "Return the .m3u file a playlist called NAME goes in."
  (expand-file-name (concat (replace-regexp-in-string "[/\\:*?\"<>|]" "_" name) ".m3u")
                    (supersonic-local--playlist-directory)))

(aio-defun
 supersonic-local--create-playlist (name ids) "Write a new playlist NAME of track IDS."
 (let ((file (supersonic-local--playlist-name-file name)))
   (when (file-exists-p file)
     (user-error "There is a playlist called %s already" name))
   (supersonic-local--write-playlist file ids)
   (supersonic-local--playlist-plist file)))

(aio-defun
 supersonic-local--edit-playlist (id changes) "Make CHANGES to local playlist ID."
 (let* ((file (supersonic-local--playlist-file id))
        (remove (plist-get changes :remove))
        (tracks (append (cl-loop for track in (supersonic-local--read-playlist file) for i from 0
                                 unless (memq i remove) collect track)
                        (plist-get changes :add))))
   (supersonic-local--write-playlist file tracks)
   (when (plist-get changes :name)
     (rename-file file (supersonic-local--playlist-name-file (plist-get changes :name))))
   nil))

(aio-defun
 supersonic-local--set-playlist-tracks (id ids) "Make track IDS local playlist ID's tracks."
 (supersonic-local--write-playlist (supersonic-local--playlist-file id) ids)
 nil)

(aio-defun
 supersonic-local--delete-playlist (id) "Move local playlist ID to the trash."
 (let ((delete-by-moving-to-trash t))
   (delete-file (supersonic-local--playlist-file id) t))
 nil)

(aio-defun
 supersonic-local--play-queue () "Resolve to the local play queue saved last, or nil."
 (supersonic-local--read-file (expand-file-name "local-queue.eld" supersonic-cache-path)))

(aio-defun
 supersonic-local--save-play-queue (ids index position) "Save the local play queue IDS."
 (supersonic-local--write-file (expand-file-name "local-queue.eld" supersonic-cache-path)
                               (list :ids ids :current index :position position))
 nil)

(defun supersonic-local--shuffled (list)
  "Return a shuffled copy of LIST."
  (let ((vector (vconcat list)))
    (cl-loop for i from (1- (length vector)) downto 1
             do (cl-rotatef (aref vector i) (aref vector (random (1+ i)))))
    (append vector nil)))

(aio-defun
 supersonic-local--similar-tracks (id count)
 "Resolve to at most COUNT local tracks like ID's: the same artist or genre."
 (aio-await (supersonic-local--ensure-index))
 (let* ((seed (or (gethash id supersonic-local--tracks)
                  (car (plist-get (gethash id supersonic-local--albums) :tracks))
                  (let ((artist (gethash id supersonic-local--artists)))
                    (car (plist-get (gethash (car (plist-get artist :albums)) supersonic-local--albums) :tracks)))))
        (artist (plist-get seed :album-artist))
        (genre (plist-get seed :genre)))
   (mapcar #'supersonic-local--public
           (seq-take (supersonic-local--shuffled
                      (seq-filter (lambda (track)
                                    (and (not (equal (plist-get track :id) id))
                                         (or (and artist (equal (plist-get track :album-artist) artist))
                                             (and genre (equal (plist-get track :genre) genre)))))
                                  (hash-table-values supersonic-local--tracks)))
                     count))))

(aio-defun
 supersonic-local--random-tracks (count &optional genre)
 "Resolve to at most COUNT random local tracks, of GENRE if given."
 (aio-await (supersonic-local--ensure-index))
 (mapcar #'supersonic-local--public
         (seq-take (supersonic-local--shuffled
                    (seq-filter (lambda (track) (or (not genre) (equal (plist-get track :genre) genre)))
                                (hash-table-values supersonic-local--tracks)))
                   count)))

;;;; Favourites

(defvar supersonic-local--favourites nil "Hash table of favourite ids, or nil until read.")

(defun supersonic-local--favourites-file ()
  "Return the file local favourites are kept in."
  (expand-file-name "local-favourites.eld" supersonic-cache-path))

(defun supersonic-local--favourites ()
  "Return the table of favourite ids, reading it if need be."
  (or supersonic-local--favourites
      (let ((table (make-hash-table :test #'equal)))
        (dolist (id (supersonic-local--read-file (supersonic-local--favourites-file)))
          (puthash id t table))
        (setq supersonic-local--favourites table))))

(aio-defun
 supersonic-local--star (id _kind flag) "Make local ID a favourite if FLAG, else not."
 (let ((table (supersonic-local--favourites)))
   (if flag (puthash id t table) (remhash id table))
   (supersonic-local--write-file (supersonic-local--favourites-file) (hash-table-keys table)))
 nil)

(aio-defun
 supersonic-local--starred () "Resolve to the local favourites."
 (aio-await (supersonic-local--ensure-index))
 (let ((table (supersonic-local--favourites)))
   (cl-flet ((pick (items) (mapcar #'supersonic-local--public
                                   (seq-filter (lambda (item) (gethash (plist-get item :id) table)) items))))
     (list :tracks (pick (hash-table-values supersonic-local--tracks))
           :albums (pick (hash-table-values supersonic-local--albums))
           :artists (pick (hash-table-values supersonic-local--artists))))))

(aio-defun
 supersonic-local--top-tracks (artist count)
 "Resolve to at most COUNT tracks of local ARTIST from its most played albums."
 (aio-await (supersonic-local--ensure-index))
 (unless supersonic-local--counts
   (supersonic-local--load-counts))
 (let* ((entry (gethash (plist-get artist :id) supersonic-local--artists))
        (albums (sort (delq nil (mapcar (lambda (id) (gethash id supersonic-local--albums)) (plist-get entry :albums)))
                      (lambda (a b) (> (gethash (plist-get a :id) supersonic-local--counts 0)
                                       (gethash (plist-get b :id) supersonic-local--counts 0)))))
        (played (seq-filter (lambda (album) (> (gethash (plist-get album :id) supersonic-local--counts 0) 0)) albums)))
   (mapcar #'supersonic-local--public
           (seq-take (seq-mapcat (lambda (album) (plist-get album :tracks)) played) count))))

(aio-defun
 supersonic-local--genres () "Resolve to the genres of the local tracks, most albums first."
 (aio-await (supersonic-local--ensure-index))
 (let ((albums (make-hash-table :test #'equal))
       (songs (make-hash-table :test #'equal)))
   (maphash (lambda (_file track)
              (let ((genre (plist-get track :genre)))
                (when genre
                  (cl-incf (gethash genre songs 0))
                  (puthash genre (cons (supersonic-local--album-key track) (gethash genre albums)) albums))))
            supersonic-local--tracks)
   (let (genres)
     (maphash (lambda (genre count)
                (push (list :id genre :name genre :song-count count
                            :album-count (length (delete-dups (gethash genre albums))))
                      genres))
              songs)
     (sort genres (lambda (a b) (> (plist-get a :album-count) (plist-get b :album-count)))))))

(aio-defun
 supersonic-local--genre-albums (genre count) "Resolve to at most COUNT local albums of GENRE."
 (aio-await (supersonic-local--ensure-index))
 (mapcar #'supersonic-local--public
         (seq-take (seq-filter (lambda (album)
                                 (seq-some (lambda (track) (equal (plist-get track :genre) genre))
                                           (plist-get album :tracks)))
                               (hash-table-values supersonic-local--albums))
                   count)))

(defun supersonic-local--cache-namespace ()
  "Return the namespace of local ids, which are file names already."
  "local")

(defun supersonic-local--config-hints ()
  "Return what to check when the local library fails."
  (list "Check that supersonic-local-directories names your music directories"
        "Check that ffprobe (from FFmpeg) is installed, see supersonic-local-ffprobe"))

(supersonic-provider-register
 'local
 `((artists . supersonic-local--artists-op)
   (artist-albums . supersonic-local--artist-albums)
   (album-list . supersonic-local--album-list)
   (album-tracks . supersonic-local--album-tracks)
   (track . supersonic-local--track-op)
   (search . supersonic-local--search)
   (stream-url . supersonic-local--stream-url)
   (scrobble . supersonic-local--scrobble)
   (cover-art . supersonic-local--cover-art)
   (lyrics . supersonic-local--lyrics)
   (playlists . supersonic-local--playlists)
   (playlist-tracks . supersonic-local--playlist-tracks)
   (create-playlist . supersonic-local--create-playlist)
   (edit-playlist . supersonic-local--edit-playlist)
   (set-playlist-tracks . supersonic-local--set-playlist-tracks)
   (delete-playlist . supersonic-local--delete-playlist)
   (play-queue . supersonic-local--play-queue)
   (save-play-queue . supersonic-local--save-play-queue)
   (similar-tracks . supersonic-local--similar-tracks)
   (random-tracks . supersonic-local--random-tracks)
   (star . supersonic-local--star)
   (starred . supersonic-local--starred)
   (top-tracks . supersonic-local--top-tracks)
   (genres . supersonic-local--genres)
   (genre-albums . supersonic-local--genre-albums)
   (cache-namespace . supersonic-local--cache-namespace)
   (config-hints . supersonic-local--config-hints)))

;;;
;;; Switching to and from local files
;;;

(defvar supersonic-local--previous-provider nil
  "The provider active before `supersonic-local-toggle' switched to `local'.")

;;;###autoload
(defun supersonic-local-toggle ()
  "Switch to the local files, or back to the provider active before them.
Like every provider switch, this stops playback."
  (interactive)
  (require 'supersonic)
  (if (eq supersonic-provider 'local)
      (supersonic-provider-switch (or supersonic-local--previous-provider
                                      (car (remq 'local (supersonic-provider-names)))))
    (setq supersonic-local--previous-provider supersonic-provider)
    (supersonic-provider-switch 'local)))

(defconst supersonic-local--offline-regexp
  (regexp-opt '("No answer from" "connection" "Connection" "refused" "unreachable" "Name or service"
                "Network is" "network" "host" "timed out" "open-network-stream" "Failed to fetch"))
  "Matches the error messages of a request that never reached the server.")

(defvar supersonic-local--offered-at nil
  "When the fallback to local files was last offered, as a float time.")

(defun supersonic-local--maybe-offer-fallback (_description err)
  "Offer switching to local files if ERR says the server is unreachable.
On `supersonic-async-error-functions'.  Offered at most once a minute,
as one unreachable server fails several requests at once."
  (when (and supersonic-local-offer-fallback
             supersonic-local-directories
             (not (eq supersonic-provider 'local))
             (string-match-p supersonic-local--offline-regexp (error-message-string err))
             (or (not supersonic-local--offered-at)
                 (> (- (float-time) supersonic-local--offered-at) 60)))
    (setq supersonic-local--offered-at (float-time))
    ;; Out of the process callback the error was reported from.
    (run-at-time 0 nil
                 (lambda ()
                   (when (y-or-n-p "[Supersonic] Server unreachable.  Switch to local files? ")
                     (supersonic-local-toggle))))))

(add-hook 'supersonic-async-error-functions #'supersonic-local--maybe-offer-fallback)

(defun supersonic-local-unload-function ()
  "Remove the fallback offer from `supersonic-async-error-functions'."
  (remove-hook 'supersonic-async-error-functions #'supersonic-local--maybe-offer-fallback)
  nil)

(provide 'supersonic-local)

;;; supersonic-local.el ends here
