;;; supersonic-lyrics.el --- Synced lyrics for supersonic.el -*- lexical-binding: t; -*-

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

;; Lyrics for the track playing, highlighted line by line as it plays
;; when they are synced.
;;
;; `supersonic-lyrics' opens the *supersonic-lyrics* buffer.  It turns
;; on `supersonic-lyrics-follow-mode', which looks up the lyrics of
;; each track as it starts and keeps track of the line being sung:
;;
;; - the lyrics buffer highlights it and keeps it centred;
;; - the now-playing buffer shows it, see `supersonic-lyrics-in-now-playing';
;; - the echo area shows it, see `supersonic-lyrics-echo-area';
;; - `supersonic-lyrics-line' holds it, for a mode line of your own,
;;   and `supersonic-lyrics-line-change-hook' runs as it changes.
;;
;; Lyrics come from the provider first -- for Subsonic, OpenSubsonic's
;; getLyricsBySongId, which serves the lyrics embedded in the files or
;; in .lrc files next to them -- and, when the provider has none that
;; are synced and `supersonic-lyrics-lrclib' is non-nil, from LRCLIB
;; (https://lrclib.net).  Found lyrics are cached on disk.
;;
;; The line being sung is worked out from the playback position, read
;; from the backend whenever playback is paused, resumed or sought and
;; every few seconds besides, and extrapolated in between.

;;; Code:

(require 'aio)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'url-util)

(require 'supersonic-custom)
(require 'supersonic-api)
(require 'supersonic-provider)
(require 'supersonic-playback)
(require 'supersonic)

;;;
;;; Parsing
;;;

(defconst supersonic-lyrics--lrc-time-regexp
  "\\[\\([0-9]+\\):\\([0-9]+\\)\\(?:[.:]\\([0-9]+\\)\\)?\\]"
  "Regexp matching one LRC time tag, [mm:ss], [mm:ss.xx] or [mm:ss:xx].")

(defun supersonic-lyrics--lrc-ms (minutes seconds fraction)
  "Return the milliseconds of an LRC time tag's MINUTES, SECONDS and FRACTION.
Each is a string; FRACTION, the digits after the separator, may be nil."
  (+ (* 60000 (string-to-number minutes))
     (* 1000 (string-to-number seconds))
     (if fraction
         (round (* 1000 (/ (string-to-number fraction) (expt 10.0 (length fraction)))))
       0)))

(defun supersonic-lyrics-parse-lrc (text &optional source)
  "Parse TEXT, lyrics in LRC format or plain, into a lyrics plist.
The plist is as the `lyrics' provider operation resolves to, SOURCE
being its `:source'.  A line with several time tags is repeated at each
of them, an [offset:] tag is applied, and enhanced LRC's word tags are
dropped.  TEXT without any time tag is taken as plain lyrics.  Returns
nil for blank TEXT."
  (when (and text (not (string-empty-p (string-trim text))))
    (let ((offset 0)
          (synced nil)
          (plain nil))
      (dolist (line (split-string text "\r?\n"))
        (cond
         ((string-match "\\`\\[offset:[[:space:]]*\\([-+]?[0-9]+\\)\\][[:space:]]*\\'" line)
          (setq offset (string-to-number (match-string 1 line))))
         ;; Other ID tags: [ar:...], [ti:...], [length:...] and the like.
         ((string-match-p "\\`\\[[[:alpha:]]+:.*\\][[:space:]]*\\'" line))
         ((string-match (concat "\\`\\(?:" supersonic-lyrics--lrc-time-regexp "\\)+") line)
          (let ((tags (match-string 0 line))
                (lyric (string-trim
                        (replace-regexp-in-string "<[0-9]+:[0-9]+\\(?:[.:][0-9]+\\)?>" ""
                                                  (substring line (match-end 0)))))
                (start 0))
            (while (string-match supersonic-lyrics--lrc-time-regexp tags start)
              (push (cons (supersonic-lyrics--lrc-ms (match-string 1 tags) (match-string 2 tags)
                                                     (match-string 3 tags))
                          lyric)
                    synced)
              (setq start (match-end 0)))))
         (t (push (cons nil (string-trim-right line)) plain))))
      (if synced
          (supersonic-lyrics--fold-duplicates
           (list :synced t
                 :lines (mapcar (lambda (line) (cons (max 0 (- (car line) offset)) (cdr line)))
                                ;; `sort' is stable: lines sharing a time keep file order.
                                (sort (nreverse synced) (lambda (a b) (< (car a) (car b)))))
                 :source source))
        (let ((lines (nreverse plain)))
          ;; Keep blank lines between verses, not around the whole text.
          (while (and lines (string-empty-p (string-trim (cdar lines))))
            (setq lines (cdr lines)))
          (setq lines (nreverse lines))
          (while (and lines (string-empty-p (string-trim (cdar lines))))
            (setq lines (cdr lines)))
          (list :synced nil :lines (nreverse lines) :source source))))))

(defun supersonic-lyrics--fold-duplicates (lyrics)
  "Read the second of two LYRICS lines sharing a time as the first's translation.
The usual way LRC files carry a translation: each line twice, original
first.  Lines are left alone unless `supersonic-lyrics-lrc-translations'
is non-nil and at least a third of them come in such pairs."
  (let* ((lines (plist-get lyrics :lines))
         (pairs (cl-loop for (a b) on lines
                         count (and b (eql (car a) (car b))))))
    (if (not (and supersonic-lyrics-lrc-translations lines
                  (>= (* 3 pairs) (length lines))
                  (> pairs 0)))
        lyrics
      (let ((kept nil)
            (translation nil))
        (while lines
          (let ((line (pop lines)))
            (push line kept)
            (push (and lines (eql (car line) (caar lines)) (cdr (pop lines))) translation)
            ;; A third line at the same time is no translation of ours.
            (while (and lines (eql (car line) (caar lines)))
              (pop lines))))
        (list :synced t :lines (nreverse kept) :source (plist-get lyrics :source)
              :translation (nreverse translation) :translation-source "lrc")))))

(defun supersonic-lyrics-align-translation (lines translated)
  "Return TRANSLATED, lyrics lines, as a translation aligned with LINES.
Both are lists of (MS . TEXT).  Synced lines are matched by start time
within half a second, others by position when the counts agree.
Returns a list of strings or nils as long as LINES, or nil if nothing
lines up."
  (let ((result
         (cond
          ((and (caar lines) (caar translated))
           (mapcar (lambda (line)
                     (let ((match (seq-find (lambda (other) (<= (abs (- (car other) (car line))) 500))
                                            translated)))
                       (and match (cdr match))))
                   lines))
          ((= (length lines) (length translated))
           (mapcar #'cdr translated)))))
    (and (seq-some (lambda (text) (and text (not (string-empty-p text)))) result)
         result)))

(defun supersonic-lyrics--line-index (lyrics ms)
  "Return the index of the line of LYRICS being sung MS milliseconds in.
That is the last line starting at or before MS, nil before the first or
for lyrics that are not synced."
  (when (plist-get lyrics :synced)
    (let ((index nil)
          (i 0))
      (cl-loop for line in (plist-get lyrics :lines)
               while (<= (car line) ms)
               do (setq index i
                        i (1+ i)))
      index)))

;;;
;;; LRCLIB
;;;

(defun supersonic-lyrics--lrclib-lyrics (record)
  "Turn RECORD, an LRCLIB lyrics record as an alist, into lyrics, or nil."
  (cond
   ((alist-get 'instrumental record)
    (list :synced nil :lines '((nil . "♪ Instrumental ♪")) :source "lrclib"))
   ((alist-get 'syncedLyrics record)
    (supersonic-lyrics-parse-lrc (alist-get 'syncedLyrics record) "lrclib"))
   ((alist-get 'plainLyrics record)
    (supersonic-lyrics-parse-lrc (alist-get 'plainLyrics record) "lrclib"))))

(defun supersonic-lyrics--lrclib-best (records duration)
  "Pick the best of LRCLIB search RECORDS for a track DURATION seconds long.
Synced lyrics win over plain, then the closest duration.  When
DURATION is known, a record more than five seconds off it, or of no
known duration, is no match at all."
  (let ((candidates
         (seq-filter (lambda (record)
                       (and (or (alist-get 'syncedLyrics record) (alist-get 'plainLyrics record))
                            (or (not duration)
                                (and (alist-get 'duration record)
                                     (<= (abs (- (alist-get 'duration record) duration)) 5)))))
                     records)))
    (car (sort candidates
               (lambda (a b)
                 (let ((sa (and (alist-get 'syncedLyrics a) t))
                       (sb (and (alist-get 'syncedLyrics b) t)))
                   (if (eq sa sb)
                       (< (abs (- (or (alist-get 'duration a) 0) (or duration 0)))
                          (abs (- (or (alist-get 'duration b) 0) (or duration 0))))
                     sa)))))))

(aio-defun
 supersonic-lyrics--lrclib-request (endpoint query)
 "Return a promise resolving to LRCLIB's parsed JSON for ENDPOINT with QUERY.
QUERY is an alist of strings.  Resolves to nil when LRCLIB answers with
an error, such as 404 for a track it does not know."
 (let* ((url (concat (string-remove-suffix "/" supersonic-lyrics-lrclib-url) endpoint "?"
                     (url-build-query-string (mapcar (lambda (pair) (list (car pair) (cdr pair))) query))))
        ;; Bound only while the request is sent, not across the await.
        (request (let ((url-user-agent "supersonic.el (https://github.com/systemfreund/supersonic.el)"))
                   (supersonic-get-text url)))
        (body (condition-case nil
                  (aio-await request)
                (error nil))))
   (and body
        (condition-case nil
            (json-parse-string body :object-type 'alist :array-type 'list
                               :null-object nil :false-object nil)
          (error nil)))))

(defun supersonic-lyrics--clean-title (title)
  "Return TITLE without the parts lyrics databases often leave out of theirs.
Such as \"(feat. X)\", \"[Live]\" or \" - 2011 Remaster\"."
  (let ((case-fold-search t))
    (string-trim
     (replace-regexp-in-string
      "[[:space:]]+-[[:space:]]+.*\\(remaster\\|version\\|edit\\|mix\\|live\\|mono\\|stereo\\).*\\'" ""
      (replace-regexp-in-string "[[:space:]]*[([][^])]*[])]" "" title)))))

(aio-defun
 supersonic-lyrics-lrclib (track)
 "Return a promise resolving to LRCLIB's lyrics for TRACK, or nil.
TRACK is a track plist.  Tries, until one has synced lyrics: an exact
match on artist, title, album and duration; the same without the album,
as LRCLIB often has several records of a song, not all synced; a
search by artist and title; and a search by artist and the title
stripped by `supersonic-lyrics--clean-title'.  Falls back on the first
plain lyrics found."
 (let* ((artist (plist-get track :artist))
        (title (plist-get track :title))
        (album (plist-get track :album))
        (duration (plist-get track :duration))
        (clean (and title (supersonic-lyrics--clean-title title)))
        (base (and artist title
                   (append `(("artist_name" . ,artist) ("track_name" . ,title))
                           (and duration `(("duration" . ,(number-to-string duration)))))))
        (attempts
         (and base
              (delq nil
                    (list (and album (list "/api/get" (append base `(("album_name" . ,album)))))
                          (list "/api/get" base)
                          (list "/api/search" `(("artist_name" . ,artist) ("track_name" . ,title)))
                          (and clean (not (string-empty-p clean)) (not (equal clean title))
                               (list "/api/search" `(("artist_name" . ,artist) ("track_name" . ,clean))))))))
        (found nil)
        (plain nil))
   (while (and attempts (not found))
     (pcase-let* ((`(,endpoint ,query) (pop attempts))
                  (answer (aio-await (supersonic-lyrics--lrclib-request endpoint query)))
                  (lyrics (supersonic-lyrics--lrclib-lyrics
                           (if (equal endpoint "/api/search")
                               (and (listp answer) (supersonic-lyrics--lrclib-best answer duration))
                             answer))))
       (if (plist-get lyrics :synced)
           (setq found lyrics)
         (setq plain (or plain lyrics)))))
   (or found plain)))

;;;
;;; Looking lyrics up
;;;

(defvar supersonic-lyrics--cache (make-hash-table :test #'equal)
  "Map of track id to its lyrics, or to `none' if none were found.")

(defun supersonic-lyrics--cache-file (track-id)
  "Return the file the lyrics of TRACK-ID are cached in."
  (expand-file-name (concat (supersonic-provider-cache-name track-id) ".eld")
                    (expand-file-name "lyrics" supersonic-cache-path)))

(defconst supersonic-lyrics--cache-version 2
  "Version of the lookup that wrote a cached entry; older ones are looked up anew.
Bumped whenever the lookup got better at finding synced lyrics.")

(defun supersonic-lyrics--read-cache (track-id)
  "Return the lyrics of TRACK-ID cached on disk, or nil.
An entry written by an older lookup, see `supersonic-lyrics--cache-version',
counts as none."
  (let ((file (supersonic-lyrics--cache-file track-id)))
    (when (file-readable-p file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents file)
            (let ((lyrics (read (current-buffer))))
              (and (eql (plist-get lyrics :cache-version) supersonic-lyrics--cache-version)
                   (plist-get lyrics :lines)
                   lyrics)))
        (error nil)))))

(defun supersonic-lyrics--write-cache (track-id lyrics)
  "Cache LYRICS of TRACK-ID on disk; a failure is only logged."
  (condition-case err
      (let ((file (supersonic-lyrics--cache-file track-id)))
        (make-directory (file-name-directory file) t)
        (with-temp-file file
          (let ((print-length nil)
                (print-level nil)
                (coding-system-for-write 'utf-8))
            (insert ";; -*- coding: utf-8 -*-\n")
            (prin1 (append (list :cache-version supersonic-lyrics--cache-version) lyrics)
                   (current-buffer)))))
    (error (supersonic--log "lyrics cache write failed: %s" (error-message-string err)))))

(aio-defun
 supersonic-lyrics--fetch (track-id track)
 "Return a promise resolving to the best lyrics found for TRACK-ID, or nil.
TRACK is its track plist.  Synced lyrics win: the provider's, else
LRCLIB's; plain lyrics are only taken when neither has synced ones."
 (let* ((provided (and (supersonic-provider-supports-p 'lyrics)
                       (condition-case err
                           (aio-await (supersonic-provider-lyrics track-id))
                         (error
                          (supersonic--log "provider lyrics failed: %s" (error-message-string err))
                          nil))))
        (online (and supersonic-lyrics-lrclib
                     (not (plist-get provided :synced))
                     (aio-await (supersonic-lyrics-lrclib track)))))
   (cond
    ((plist-get provided :synced) provided)
    ((plist-get online :synced) online)
    (t (or provided online)))))

(aio-defun
 supersonic-lyrics-lookup (track-id track &optional refresh)
 "Return a promise resolving to the lyrics of TRACK-ID, or nil if none are found.
TRACK is its track plist.  Answers from the cache unless REFRESH."
 (let ((cached (and (not refresh)
                    (or (gethash track-id supersonic-lyrics--cache)
                        (supersonic-lyrics--read-cache track-id)))))
   (if cached
       (progn
         (puthash track-id cached supersonic-lyrics--cache)
         (and (not (eq cached 'none)) cached))
     (let ((lyrics (supersonic-lyrics--resolve-alternatives
                    (aio-await (supersonic-lyrics--fetch track-id track)))))
       ;; A miss is only remembered for the session: lyrics may turn
       ;; up later, on the server or on LRCLIB.
       (puthash track-id (or lyrics 'none) supersonic-lyrics--cache)
       (when lyrics
         (supersonic-lyrics--write-cache track-id lyrics))
       lyrics))))

;;;
;;; State
;;;

(defvar supersonic-lyrics-follow-mode)

(defvar supersonic-lyrics-line nil
  "The line of the current track's synced lyrics being sung, or nil.")

(defvar supersonic-lyrics-line-change-hook nil
  "Hook run whenever `supersonic-lyrics-line' may have changed.")

(defvar supersonic-lyrics--track-id nil "Id of the track the lyrics state is for.")
(defvar supersonic-lyrics--track nil "Track plist of `supersonic-lyrics--track-id'.")
(defvar supersonic-lyrics--lyrics nil "Lyrics of the current track, or nil.")
(defvar supersonic-lyrics--status nil
  "Where the lyrics of the current track stand: `loading', `found' or `none'.")
(defvar supersonic-lyrics--index nil "Index of the line being sung, or nil.")
(defvar supersonic-lyrics--anchor nil
  "The last position read, as (SECONDS FLOAT-TIME PAUSED).")
(defvar supersonic-lyrics--timer nil "Timer extrapolating the position while playing.")
(defvar supersonic-lyrics--ticks 0 "Ticks since the position was last read.")
(defvar supersonic-lyrics--generation 0
  "Bumped on every track change, so a slow lookup cannot clobber a newer one.")
(defvar supersonic-lyrics--last-echo nil "The line last put in the echo area.")

;;;
;;; Translations
;;;

(defun supersonic-lyrics--target-language-p (lang)
  "Return non-nil if `supersonic-lyrics-translation-codes' has language code LANG."
  (and lang (member (downcase lang) supersonic-lyrics-translation-codes)))

(defun supersonic-lyrics--translated-by (lyrics other source)
  "Return LYRICS with OTHER, lyrics of the same song, aligned as its translation.
SOURCE names where OTHER came from.  LYRICS is returned as is if the
two do not line up."
  (let ((translation (supersonic-lyrics-align-translation (plist-get lyrics :lines) (plist-get other :lines))))
    (if translation
        (append (list :translation translation :translation-source source)
                (supersonic-lyrics--without lyrics '(:translation :translation-source :translation-none)))
      lyrics)))

(defun supersonic-lyrics--without (plist keys)
  "Return PLIST without KEYS."
  (cl-loop for (key value) on plist by #'cddr
           unless (memq key keys) append (list key value)))

(defun supersonic-lyrics--resolve-alternatives (lyrics)
  "Pick a translation for LYRICS out of its `:alternatives', and drop them.
An alternative in a language of `supersonic-lyrics-translation-codes'
becomes the translation.  Should LYRICS themselves be in that language
and an alternative not, they swap places, as long as no sync is lost."
  (let ((alternatives (plist-get lyrics :alternatives))
        (lyrics (supersonic-lyrics--without lyrics '(:alternatives))))
    (cond
     ((or (null alternatives) (plist-get lyrics :translation)) lyrics)
     ((and (supersonic-lyrics--target-language-p (plist-get lyrics :lang))
           (seq-find (lambda (other)
                       (and (not (supersonic-lyrics--target-language-p (plist-get other :lang)))
                            (or (plist-get other :synced) (not (plist-get lyrics :synced)))))
                     alternatives))
      (let ((original (seq-find (lambda (other)
                                  (and (not (supersonic-lyrics--target-language-p (plist-get other :lang)))
                                       (or (plist-get other :synced) (not (plist-get lyrics :synced)))))
                                alternatives)))
        (supersonic-lyrics--translated-by original lyrics (plist-get lyrics :source))))
     (t
      (let ((other (seq-find (lambda (other) (supersonic-lyrics--target-language-p (plist-get other :lang)))
                             alternatives)))
        (if other
            (supersonic-lyrics--translated-by lyrics other (plist-get other :source))
          lyrics))))))

(defun supersonic-lyrics--translation-directory ()
  "Return the directory translations of your own are looked for in."
  (or supersonic-lyrics-translation-directory
      (expand-file-name "lyrics-translations" supersonic-cache-path)))

(defun supersonic-lyrics--file-name-part (string)
  "Return STRING made safe for use in a file name."
  (string-trim (replace-regexp-in-string "[/\\:*?\"<>|]" "_" (or string ""))))

(defun supersonic-lyrics-translation-file (track)
  "Return the file a translation of TRACK of your own is looked for in, if any.
That is \"ARTIST - TITLE.lrc\" or \".txt\" in
`supersonic-lyrics-translation-directory', compared ignoring case.  A
.txt file has one line per lyrics line; an .lrc file is matched to the
lyrics by time."
  (let ((directory (supersonic-lyrics--translation-directory))
        (base (downcase (concat (supersonic-lyrics--file-name-part (plist-get track :artist)) " - "
                                (supersonic-lyrics--file-name-part (plist-get track :title))))))
    (when (file-directory-p directory)
      (seq-find (lambda (file) (equal (downcase (file-name-sans-extension (file-name-nondirectory file))) base))
                (directory-files directory t "\\.\\(lrc\\|txt\\)\\'")))))

(defun supersonic-lyrics--with-user-translation (lyrics track)
  "Return LYRICS translated by your own file for TRACK, if there is one.
Translations that came with the lyrics themselves win; one of your own
wins over a machine translation."
  (let ((file (and lyrics
                   (not (member (plist-get lyrics :translation-source) '("lrc" "server" "lrclib")))
                   (supersonic-lyrics-translation-file track))))
    (if (not file)
        lyrics
      (let ((other (supersonic-lyrics-parse-lrc
                    (with-temp-buffer
                      (let ((coding-system-for-read 'utf-8))
                        (insert-file-contents file))
                      (buffer-string))
                    "file")))
        (if (and other (plist-get lyrics :synced) (not (plist-get other :synced)))
            ;; Plain text against synced lyrics: by position, blank lines and all.
            (let ((lines (plist-get lyrics :lines))
                  (texts (mapcar #'cdr (plist-get other :lines))))
              (if (= (length lines) (length texts))
                  (append (list :translation texts :translation-source "your file")
                          (supersonic-lyrics--without lyrics '(:translation :translation-source)))
                (message "[Supersonic] %s has %d lines, the lyrics %d; not used"
                         (file-name-nondirectory file) (length texts) (length lines))
                lyrics))
          (supersonic-lyrics--translated-by lyrics other "your file"))))))

(defun supersonic-lyrics--machine-translate (interactive)
  "Have `supersonic-lyrics-translate-function' translate the current lyrics.
Skipped for lyrics already translated, or found to need none, unless
INTERACTIVE, which also reports what happened."
  (let ((lyrics supersonic-lyrics--lyrics)
        (track supersonic-lyrics--track)
        (track-id supersonic-lyrics--track-id)
        (generation supersonic-lyrics--generation))
    (cond
     ((not supersonic-lyrics-translate-function)
      (when interactive
        (user-error "No `supersonic-lyrics-translate-function' to translate with")))
     ((not lyrics)
      (when interactive (user-error "No lyrics to translate")))
     ((and (not interactive)
           (or (plist-get lyrics :translation) (plist-get lyrics :translation-none))))
     (t
      (when interactive
        (message "[Supersonic] Translating lyrics..."))
      (funcall
       supersonic-lyrics-translate-function track (mapcar #'cdr (plist-get lyrics :lines))
       supersonic-lyrics-translation-language
       (lambda (translation &optional err)
         (cond
          (err (message "[Supersonic] Translating lyrics failed: %s" err))
          ((not (equal track-id supersonic-lyrics--track-id)))
          (t
           (let* ((lines (plist-get lyrics :lines))
                  (fixed (and translation
                              (if (= (length translation) (length lines))
                                  translation
                                (supersonic--log "machine translation has %d lines for %d"
                                                 (length translation) (length lines))
                                nil)))
                  (updated (append (if fixed
                                       (list :translation fixed :translation-source "machine")
                                     (list :translation-none t))
                                   (supersonic-lyrics--without lyrics '(:translation :translation-source
                                                                         :translation-none)))))
             (puthash track-id updated supersonic-lyrics--cache)
             (supersonic-lyrics--write-cache track-id updated)
             (when (= generation supersonic-lyrics--generation)
               (setq supersonic-lyrics--lyrics updated)
               (supersonic-lyrics--refresh-views))
             (when interactive
               (message "[Supersonic] %s" (if fixed "Lyrics translated" "No translation needed"))))))))))))

(defun supersonic-lyrics-translate ()
  "Translate the lyrics playing now with `supersonic-lyrics-translate-function'."
  (interactive)
  (supersonic-lyrics--machine-translate t))

;;;###autoload
(defun supersonic-lyrics-toggle-translation ()
  "Show or hide translations under the lyrics lines."
  (interactive)
  (setq supersonic-lyrics-show-translation (not supersonic-lyrics-show-translation))
  (supersonic-lyrics--refresh-views)
  (message "Lyrics translations %s" (if supersonic-lyrics-show-translation "shown" "hidden")))

;;;###autoload
(defun supersonic-lyrics-toggle-echo-area ()
  "Show or stop showing the line being sung in the echo area.
Turns on `supersonic-lyrics-follow-mode' if need be."
  (interactive)
  (setq supersonic-lyrics-echo-area (not supersonic-lyrics-echo-area))
  (unless (or supersonic-lyrics-echo-area (not (equal (current-message) supersonic-lyrics--last-echo)))
    (message nil))
  (when (and supersonic-lyrics-echo-area (not supersonic-lyrics-follow-mode))
    (supersonic-lyrics-follow-mode 1))
  (message "Lyrics in the echo area %s" (if supersonic-lyrics-echo-area "on" "off")))

(defun supersonic-lyrics--translation-text (index)
  "Return the shown translation of lyrics line INDEX, or nil."
  (and index supersonic-lyrics-show-translation
       (let ((text (nth index (plist-get supersonic-lyrics--lyrics :translation))))
         (and text (not (string-empty-p (string-trim text))) text))))

;;;
;;; Following playback
;;;

(defun supersonic-lyrics--position-ms ()
  "Return the extrapolated playback position in milliseconds, or nil."
  (pcase supersonic-lyrics--anchor
    (`(,seconds ,time ,paused)
     (+ (* 1000 (+ seconds (if paused 0 (- (float-time) time))))
        supersonic-lyrics-offset))))

(defun supersonic-lyrics--tick ()
  "Move to the line being sung now, reading the position anew now and then."
  (when (>= (setq supersonic-lyrics--ticks (1+ supersonic-lyrics--ticks))
            (max 1 (round (/ 5.0 supersonic-lyrics-interval))))
    (setq supersonic-lyrics--ticks 0)
    (supersonic-lyrics--resync))
  (let* ((ms (supersonic-lyrics--position-ms))
         (index (and ms (supersonic-lyrics--line-index supersonic-lyrics--lyrics ms))))
    (unless (eql index supersonic-lyrics--index)
      (setq supersonic-lyrics--index index)
      (supersonic-lyrics--line-changed))))

(defun supersonic-lyrics--update-timer ()
  "Run the tick timer exactly while synced lyrics are being sung."
  (let ((wanted (and supersonic-lyrics-follow-mode
                     (plist-get supersonic-lyrics--lyrics :synced)
                     supersonic-lyrics--anchor
                     (not (nth 2 supersonic-lyrics--anchor)))))
    (cond
     ((and wanted (not supersonic-lyrics--timer))
      (setq supersonic-lyrics--timer
            (run-at-time 0 supersonic-lyrics-interval #'supersonic-lyrics--tick)))
     ((and (not wanted) supersonic-lyrics--timer)
      (cancel-timer supersonic-lyrics--timer)
      (setq supersonic-lyrics--timer nil)))))

(aio-defun
 supersonic-lyrics--resync ()
 "Read the playback position and pause state from the backend afresh."
 (condition-case err
     (let* ((position (aio-await (supersonic-playback-status 'position)))
            (paused (aio-await (supersonic-playback-status 'paused))))
       (setq supersonic-lyrics--anchor (and (numberp position) (list position (float-time) paused)))
       (supersonic-lyrics--update-timer)
       (supersonic-lyrics--tick-once))
   (error (supersonic--log "lyrics resync failed: %s" (error-message-string err)))))

(defun supersonic-lyrics--tick-once ()
  "Recompute the line being sung right away, without counting a tick."
  (let* ((ms (supersonic-lyrics--position-ms))
         (index (and ms (supersonic-lyrics--line-index supersonic-lyrics--lyrics ms))))
    (unless (eql index supersonic-lyrics--index)
      (setq supersonic-lyrics--index index)
      (supersonic-lyrics--line-changed))))

(aio-defun
 supersonic-lyrics--track-changed (&optional refresh)
 "Look up the lyrics of the track now playing, unless they are current.
With REFRESH, look them up again even for the same track, bypassing the
cache."
 (condition-case err
     (let ((track-id (aio-await (supersonic-playback-status 'track-id))))
       (when (or refresh (not (equal track-id supersonic-lyrics--track-id)))
         (let ((generation (setq supersonic-lyrics--generation (1+ supersonic-lyrics--generation))))
           (setq supersonic-lyrics--track-id track-id
                 supersonic-lyrics--track nil
                 supersonic-lyrics--lyrics nil
                 supersonic-lyrics--index nil
                 supersonic-lyrics--status (and track-id 'loading))
           (supersonic-lyrics--update-timer)
           (supersonic-lyrics--refresh-views)
           (when track-id
             (let* ((track (aio-await (supersonic-provider-track track-id)))
                    (lyrics (aio-await (supersonic-lyrics-lookup track-id track refresh))))
               (when (= generation supersonic-lyrics--generation)
                 (setq supersonic-lyrics--track track
                       supersonic-lyrics--lyrics (supersonic-lyrics--with-user-translation lyrics track)
                       supersonic-lyrics--status (if lyrics 'found 'none))
                 (supersonic-lyrics--refresh-views)
                 (when (and supersonic-lyrics-auto-translate supersonic-lyrics-translate-function)
                   (supersonic-lyrics--machine-translate nil)))))))
       (aio-await (supersonic-lyrics--resync)))
   (error
    (setq supersonic-lyrics--status 'none)
    (supersonic-lyrics--refresh-views)
    (supersonic--log "lyrics lookup failed: %s" (error-message-string err)))))

(defun supersonic-lyrics--on-track-change ()
  "Follow a track change, not awaiting it."
  (ignore (supersonic-lyrics--track-changed)))

(defun supersonic-lyrics--on-position-change ()
  "Follow a pause, resume or seek, not awaiting it."
  (ignore (supersonic-lyrics--resync)))

(defconst supersonic-lyrics--hooks
  '((supersonic-playback-track-change-hook . supersonic-lyrics--on-track-change)
    (supersonic-playback-state-change-hook . supersonic-lyrics--on-position-change)
    (supersonic-playback-position-change-hook . supersonic-lyrics--on-position-change))
  "The playback hooks `supersonic-lyrics-follow-mode' follows, with its handlers.")

;;;###autoload
(define-minor-mode supersonic-lyrics-follow-mode
  "Look up the lyrics of every track played and follow the line being sung.
See the Commentary of `supersonic-lyrics.el' for where it shows."
  :global t
  :group 'supersonic-lyrics
  (dolist (hook supersonic-lyrics--hooks)
    (remove-hook (car hook) (cdr hook)))
  (setq supersonic-now-playing-render-functions
        (delq #'supersonic-lyrics-now-playing-render supersonic-now-playing-render-functions))
  (if supersonic-lyrics-follow-mode
      (progn
        (dolist (hook supersonic-lyrics--hooks)
          (add-hook (car hook) (cdr hook)))
        (when supersonic-lyrics-in-now-playing
          (setq supersonic-now-playing-render-functions
                (append supersonic-now-playing-render-functions
                        (list #'supersonic-lyrics-now-playing-render))))
        (setq supersonic-lyrics--track-id nil)
        (supersonic-lyrics--on-track-change))
    (setq supersonic-lyrics--anchor nil
          supersonic-lyrics-line nil)
    (supersonic-lyrics--update-timer)))

;;;
;;; Showing the line being sung
;;;

(defun supersonic-lyrics--line-text (index)
  "Return the text of line INDEX of the current lyrics, or nil."
  (and index (cdr (nth index (plist-get supersonic-lyrics--lyrics :lines)))))

(defun supersonic-lyrics--line-changed ()
  "Show the line now being sung everywhere it is shown."
  (setq supersonic-lyrics-line (supersonic-lyrics--line-text supersonic-lyrics--index))
  (supersonic-lyrics--highlight)
  (supersonic-lyrics--update-now-playing)
  (supersonic-lyrics--echo)
  (run-hooks 'supersonic-lyrics-line-change-hook)
  (force-mode-line-update t))

(defun supersonic-lyrics--echo ()
  "Show the line being sung in the echo area, if wanted and not in the way.
It never covers the minibuffer, nor a message of someone else's."
  (when (and supersonic-lyrics-echo-area
             supersonic-lyrics-line
             (not (string-empty-p supersonic-lyrics-line))
             (not (active-minibuffer-window))
             (let ((shown (current-message)))
               (or (not shown) (equal shown supersonic-lyrics--last-echo))))
    (let ((message-log-max nil)
          (translation (supersonic-lyrics--translation-text supersonic-lyrics--index)))
      (setq supersonic-lyrics--last-echo
            (message "%s" (concat (propertize (concat "♪ " supersonic-lyrics-line) 'face 'supersonic-lyrics-current)
                                  (and translation
                                       (concat "\n  " (propertize translation 'face 'supersonic-lyrics-translation)))))))))

;;;; Now playing

(defun supersonic-lyrics--now-playing-text ()
  "Return the now-playing \"Lyrics:\" row's text: the line sung and the next."
  (cond
   ((eq supersonic-lyrics--status 'loading) (propertize "…" 'face 'shadow))
   ((not (plist-get supersonic-lyrics--lyrics :synced))
    (propertize (if supersonic-lyrics--lyrics "(not synced, see `supersonic-lyrics')" "—") 'face 'shadow))
   (t
    (let ((current (supersonic-lyrics--line-text supersonic-lyrics--index))
          (next (supersonic-lyrics--line-text (if supersonic-lyrics--index (1+ supersonic-lyrics--index) 0))))
      (concat (propertize (if (and current (not (string-empty-p current))) current "♪")
                          'face 'supersonic-lyrics-current)
              (let ((translation (supersonic-lyrics--translation-text supersonic-lyrics--index)))
                (and translation
                     (concat "\n" (make-string 10 ?\s) (propertize translation 'face 'supersonic-lyrics-translation))))
              (and next (concat "\n" (make-string 10 ?\s) (propertize next 'face 'shadow))))))))

(defun supersonic-lyrics-now-playing-render (_buff _song)
  "Insert the now-playing buffer's \"Lyrics:\" row.
Put into `supersonic-now-playing-render-functions' by
`supersonic-lyrics-follow-mode' when `supersonic-lyrics-in-now-playing'
is non-nil."
  (supersonic-now-playing--insert-field "Lyrics" (supersonic-lyrics--now-playing-text) 'lyrics))

(defun supersonic-lyrics--update-now-playing ()
  "Redraw the now-playing buffer's \"Lyrics:\" row, if it has one."
  (let ((buff (supersonic-now-playing-buffer)))
    (when (and buff supersonic-lyrics-in-now-playing)
      (supersonic-now-playing--update-field buff 'lyrics (supersonic-lyrics--now-playing-text)))))

;;;; The lyrics buffer

(defconst supersonic-lyrics-buffer-name "*supersonic-lyrics*"
  "Name of the buffer `supersonic-lyrics' shows.")

(defvar-local supersonic-lyrics--follow t
  "Non-nil while the lyrics buffer keeps the line being sung in view.")

(defvar-local supersonic-lyrics--current-overlay nil "Overlay on the line being sung.")
(defvar-local supersonic-lyrics--past-overlay nil "Overlay on the lines already sung.")
(defvar-local supersonic-lyrics--set-point nil "Where following last put point.")

(defun supersonic-lyrics--buffer ()
  "Return the lyrics buffer if it is live, else nil."
  (let ((buff (get-buffer supersonic-lyrics-buffer-name)))
    (and buff (buffer-live-p buff) buff)))

(defun supersonic-lyrics--line-position (index)
  "Return the buffer position of lyrics line INDEX, or nil."
  (and index
       (save-excursion
         (goto-char (point-min))
         (let ((match (text-property-search-forward 'supersonic-lyrics-index index #'eql)))
           (and match (prop-match-beginning match))))))

(defun supersonic-lyrics--render-buffer ()
  "Redraw the lyrics buffer from the current lyrics state."
  (let ((buff (supersonic-lyrics--buffer)))
    (when buff
      (with-current-buffer buff
        (let ((inhibit-read-only t)
              (track supersonic-lyrics--track)
              (lyrics supersonic-lyrics--lyrics))
          (erase-buffer)
          (when track
            (insert (propertize (or (plist-get track :title) "") 'face 'bold) "\n")
            (insert (propertize (string-join (delq nil (list (plist-get track :artist) (plist-get track :album)))
                                             " — ")
                                'face 'shadow)
                    "\n"))
          (when lyrics
            (insert (propertize (format "%s · %s%s\n" (if (plist-get lyrics :synced) "synced" "not synced")
                                        (or (plist-get lyrics :source) "?")
                                        (cond ((plist-get lyrics :translation)
                                               (format " · translation: %s%s"
                                                       (plist-get lyrics :translation-source)
                                                       (if supersonic-lyrics-show-translation "" " (hidden)")))
                                              (t "")))
                                'face 'shadow)))
          (insert "\n")
          (pcase supersonic-lyrics--status
            ('nil (insert (propertize "Nothing playing." 'face 'shadow)))
            ('loading (insert (propertize "Looking for lyrics…" 'face 'shadow)))
            ('none (insert (propertize "No lyrics found." 'face 'shadow)))
            (_
             (let ((index 0))
               (dolist (line (plist-get lyrics :lines))
                 (let ((translation (supersonic-lyrics--translation-text index))
                       (properties (list 'supersonic-lyrics-index index
                                         'supersonic-lyrics-ms (car line)
                                         'mouse-face (and (car line) 'highlight)
                                         'help-echo (and (car line) "mouse-1: play from this line"))))
                   ;; Newlines included, so a line and its translation
                   ;; make one stretch of the same index.
                   (insert (apply #'propertize
                                  (concat (if (and (car line) (string-empty-p (cdr line))) "♪" (cdr line)) "\n")
                                  properties))
                   (when translation
                     (insert (apply #'propertize (concat "  " translation "\n")
                                    'face 'supersonic-lyrics-translation properties))))
                 (setq index (1+ index))))))
          (setq supersonic-lyrics--follow t)
          (goto-char (point-min))
          (setq supersonic-lyrics--set-point (point))
          (supersonic-lyrics--highlight))))))

(defun supersonic-lyrics--highlight ()
  "Highlight the line being sung in the lyrics buffer and scroll it into view."
  (let ((buff (supersonic-lyrics--buffer)))
    (when buff
      (with-current-buffer buff
        (unless supersonic-lyrics--current-overlay
          (setq supersonic-lyrics--current-overlay (make-overlay 1 1))
          (overlay-put supersonic-lyrics--current-overlay 'face 'supersonic-lyrics-current)
          (setq supersonic-lyrics--past-overlay (make-overlay 1 1))
          (overlay-put supersonic-lyrics--past-overlay 'face 'supersonic-lyrics-past))
        (let ((start (supersonic-lyrics--line-position supersonic-lyrics--index))
              (first (supersonic-lyrics--line-position 0)))
          (if (not start)
              (progn
                (delete-overlay supersonic-lyrics--current-overlay)
                (delete-overlay supersonic-lyrics--past-overlay))
            (let ((end (1- (or (next-single-property-change start 'supersonic-lyrics-index) (point-max)))))
              (move-overlay supersonic-lyrics--current-overlay start end)
              (if (and first (< first start))
                  (move-overlay supersonic-lyrics--past-overlay first start)
                (delete-overlay supersonic-lyrics--past-overlay))
              (when supersonic-lyrics--follow
                (setq supersonic-lyrics--set-point start)
                (goto-char start)
                (dolist (window (get-buffer-window-list buff nil t))
                  (set-window-point window start)
                  (with-selected-window window
                    (recenter)))))))))))

(defun supersonic-lyrics--refresh-views ()
  "Redraw every view of the lyrics after they changed."
  (supersonic-lyrics--render-buffer)
  (setq supersonic-lyrics-line (supersonic-lyrics--line-text supersonic-lyrics--index))
  (supersonic-lyrics--update-now-playing)
  (run-hooks 'supersonic-lyrics-line-change-hook)
  (force-mode-line-update t))

(defun supersonic-lyrics--notice-motion ()
  "Stop following once point is moved by hand, in `post-command-hook'."
  (when (and supersonic-lyrics--follow
             supersonic-lyrics--set-point
             (/= (point) supersonic-lyrics--set-point))
    (setq supersonic-lyrics--follow nil)
    (message "Lyrics no longer follow playback; %s resumes"
             (substitute-command-keys "\\<supersonic-lyrics-mode-map>\\[supersonic-lyrics-toggle-follow]"))))

(defun supersonic-lyrics-toggle-follow ()
  "Toggle whether the lyrics buffer keeps the line being sung in view."
  (interactive)
  (setq supersonic-lyrics--follow (not supersonic-lyrics--follow))
  (supersonic-lyrics--highlight)
  (setq supersonic-lyrics--set-point (point))
  (message "Lyrics %s playback" (if supersonic-lyrics--follow "follow" "no longer follow")))

(defun supersonic-lyrics-seek-to-line (&optional event)
  "Play from the start of the lyrics line at point, or clicked with EVENT."
  (interactive (list last-nonmenu-event))
  (when (mouse-event-p event)
    (mouse-set-point event))
  (let ((ms (get-text-property (line-beginning-position) 'supersonic-lyrics-ms)))
    (unless ms
      (user-error "No synced lyrics line here"))
    (let ((position (aio-wait-for (supersonic-playback-status 'position))))
      (unless position
        (user-error "Nothing playing"))
      (supersonic-playback-seek (- (/ (- ms supersonic-lyrics-offset) 1000.0) position))
      (setq supersonic-lyrics--follow t
            supersonic-lyrics--set-point (point)))))

(defun supersonic-lyrics--nudge (ms)
  "Shift the lyrics MS milliseconds earlier, and say by how much they are now."
  (setq supersonic-lyrics-offset (+ supersonic-lyrics-offset ms))
  (supersonic-lyrics--tick-once)
  (message "Lyrics offset %+.2fs" (/ supersonic-lyrics-offset 1000.0)))

(defun supersonic-lyrics-earlier ()
  "Show lyrics lines a quarter second sooner, for lyrics lagging behind."
  (interactive)
  (supersonic-lyrics--nudge 250))

(defun supersonic-lyrics-later ()
  "Show lyrics lines a quarter second later, for lyrics running ahead."
  (interactive)
  (supersonic-lyrics--nudge -250))

(defun supersonic-lyrics-refetch ()
  "Look the lyrics of the track playing up again, bypassing the cache."
  (interactive)
  (ignore (supersonic-lyrics--track-changed t)))

(defvar supersonic-lyrics-mode-map
  (let ((map (supersonic--define-common-keys (make-sparse-keymap))))
    (define-key map (kbd "RET") #'supersonic-lyrics-seek-to-line)
    (define-key map [mouse-1] #'supersonic-lyrics-seek-to-line)
    (define-key map (kbd "f") #'supersonic-lyrics-toggle-follow)
    (define-key map (kbd "(") #'supersonic-lyrics-later)
    (define-key map (kbd ")") #'supersonic-lyrics-earlier)
    (define-key map (kbd "g") #'supersonic-lyrics-refetch)
    (define-key map (kbd "t") #'supersonic-lyrics-toggle-translation)
    (define-key map (kbd "T") #'supersonic-lyrics-translate)
    (define-key map (kbd "e") #'supersonic-lyrics-toggle-echo-area)
    map)
  "Keymap for `supersonic-lyrics-mode'.")

(define-derived-mode supersonic-lyrics-mode special-mode "Supersonic Lyrics"
  "Major mode for the lyrics buffer opened by `supersonic-lyrics'.
\\{supersonic-lyrics-mode-map}"
  (buffer-disable-undo)
  (setq-local cursor-type nil)
  (add-hook 'post-command-hook #'supersonic-lyrics--notice-motion nil t))

;;;###autoload
(defun supersonic-lyrics ()
  "Show the lyrics of the track playing, following along as it plays."
  (interactive)
  (let ((buff (get-buffer-create supersonic-lyrics-buffer-name)))
    (with-current-buffer buff
      (unless (derived-mode-p 'supersonic-lyrics-mode)
        (supersonic-lyrics-mode)))
    (if supersonic-lyrics-follow-mode
        (supersonic-lyrics--render-buffer)
      (supersonic-lyrics-follow-mode 1))
    (pop-to-buffer-same-window buff)
    (supersonic-lyrics--highlight)))

(provide 'supersonic-lyrics)

;;; supersonic-lyrics.el ends here
