;;; supersonic-subsonic.el --- Subsonic library provider for supersonic.el -*- lexical-binding: t; -*-

;; Author: systemfreund <github@o9z.de>
;; Assisted-by: Claude:claude-opus-5
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

;; The Subsonic library provider: implements the operations of the
;; facade in `supersonic-provider.el' against any server speaking the
;; Subsonic REST API, and registers them under the name `subsonic'.
;;
;; Everything Subsonic-shaped stops here: requests go out through
;; `supersonic-api.el', and what comes back is turned into the facade's
;; plists before anyone else sees it.  Subsonic ids are handed out
;; verbatim, and `coverArt' becomes `:art'.  `supersonic-browse-by-tags'
;; picks between the ID3 tag view (getAlbum) and the folder view
;; (getMusicDirectory) here too.

;;; Code:

(require 'aio)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url)

(require 'supersonic-custom)
(require 'supersonic-api)
(require 'supersonic-provider)

(defvar url-http-end-of-headers)

;;;
;;; Subsonic JSON -> facade plists
;;;

(defun supersonic-subsonic--plist (data mapping)
  "Build a facade plist from Subsonic alist DATA according to MAPPING.
MAPPING is a list of (KEY . FIELD) pairs: each KEY of the result is
set to DATA's FIELD.  Fields DATA does not have are left out entirely
instead of being set to nil, so a missing value reads the same as a
key the provider never heard of.  `:id' always comes first, taken from
DATA's \"id\" as a string."
  (let ((plist (list :id (supersonic-get-id-as-string data))))
    (dolist (pair mapping)
      (let ((value (assoc-default (cdr pair) data)))
        (when value
          (setq plist (append plist (list (car pair) value))))))
    plist))

(defun supersonic-subsonic--artist (data)
  "Turn a Subsonic artist alist DATA into a facade artist plist."
  (supersonic-subsonic--plist data '((:name . "name") (:art . "coverArt") (:album-count . "albumCount")
                                     (:starred . "starred"))))

(defun supersonic-subsonic--album (data)
  "Turn a Subsonic album alist DATA into a facade album plist."
  (supersonic-subsonic--plist
   data '((:name . "name") (:artist . "artist") (:year . "year") (:art . "coverArt")
          (:artist-id . "artistId") (:song-count . "songCount") (:duration . "duration")
          (:genre . "genre") (:starred . "starred"))))

(defun supersonic-subsonic--track (data)
  "Turn a Subsonic song/child alist DATA into a facade track plist.
On top of the facade's own track keys, carries Subsonic's \"genre\"
as `:genre', for a now-playing row of the user's own to read."
  (supersonic-subsonic--plist
   data
   '((:title . "title")
     (:artist . "artist")
     (:album . "album")
     (:duration . "duration")
     (:track . "track")
     (:art . "coverArt")
     (:suffix . "suffix")
     (:content-type . "contentType")
     (:size . "size")
     (:genre . "genre")
     (:year . "year")
     (:disc . "discNumber")
     (:album-id . "albumId")
     (:artist-id . "artistId")
     (:starred . "starred"))))

(defun supersonic-subsonic--playlist (data)
  "Turn a Subsonic playlist alist DATA into a facade playlist plist."
  (supersonic-subsonic--plist
   data '((:name . "name") (:song-count . "songCount") (:duration . "duration")
          (:art . "coverArt") (:owner . "owner") (:comment . "comment"))))

(defun supersonic-subsonic--podcast (data)
  "Turn a Subsonic podcast channel alist DATA into a facade podcast plist."
  (supersonic-subsonic--plist data '((:title . "title") (:art . "coverArt"))))

(defun supersonic-subsonic--episode (data)
  "Turn a Subsonic podcast episode alist DATA into a facade episode plist."
  (supersonic-subsonic--plist data '((:title . "title") (:duration . "duration") (:status . "status"))))

(defun supersonic-subsonic--artists-from (data)
  "Return the facade artists in a parsed getArtists response DATA.
Flattens every letter bucket of the \"index\" array, in bucket order."
  (mapcan
   (lambda (bucket) (mapcar #'supersonic-subsonic--artist (assoc-default "artist" bucket)))
   (supersonic-recursive-assoc data '("subsonic-response" "artists" "index"))))

(defun supersonic-subsonic--artist-albums-from (data)
  "Return the facade albums in a parsed getArtist response DATA."
  (mapcar #'supersonic-subsonic--album (supersonic-recursive-assoc data '("subsonic-response" "artist" "album"))))

(defun supersonic-subsonic--album-list-from (data)
  "Return the facade albums in a parsed getAlbumList2 response DATA."
  (mapcar #'supersonic-subsonic--album (supersonic-recursive-assoc data '("subsonic-response" "albumList2" "album"))))

(defun supersonic-subsonic--tracks-path ()
  "Return the json path to a track list, per current `supersonic-browse-by-tags'.
Read live, as `supersonic-subsonic--album-tracks' picks the endpoint
by it too."
  (if supersonic-browse-by-tags
      '("subsonic-response" "album" "song")
    '("subsonic-response" "directory" "child")))

(defun supersonic-subsonic--album-tracks-from (data)
  "Return the facade tracks in a parsed getAlbum/getMusicDirectory response DATA."
  (mapcar #'supersonic-subsonic--track (supersonic-recursive-assoc data (supersonic-subsonic--tracks-path))))

(defun supersonic-subsonic--song-from (data)
  "Return the facade track in a parsed getSong response DATA.
Signals an error when DATA has no song in it, so a caller that looks a
track up never mistakes an empty answer for a track with no metadata."
  (let ((song (supersonic-recursive-assoc data '("subsonic-response" "song"))))
    (unless song
      (error "No song in getSong response"))
    (supersonic-subsonic--track song)))

(defun supersonic-subsonic--search-from (data)
  "Return the facade search result in a parsed search3 response DATA."
  (let ((results (supersonic-recursive-assoc data '("subsonic-response" "searchResult3"))))
    (list
     :artists (mapcar #'supersonic-subsonic--artist (assoc-default "artist" results))
     :albums (mapcar #'supersonic-subsonic--album (assoc-default "album" results))
     :tracks (mapcar #'supersonic-subsonic--track (assoc-default "song" results)))))

(defun supersonic-subsonic--podcasts-from (data)
  "Return the facade podcast channels in a parsed getPodcasts response DATA."
  (mapcar #'supersonic-subsonic--podcast (supersonic-recursive-assoc data '("subsonic-response" "podcasts" "channel"))))

(defun supersonic-subsonic--podcast-episodes-from (data)
  "Return the facade episodes in a parsed getPodcasts response DATA.
DATA is the answer to a request for one channel with its episodes, so
only the first channel in it is read."
  (mapcar
   #'supersonic-subsonic--episode
   (assoc-default "episode" (car (supersonic-recursive-assoc data '("subsonic-response" "podcasts" "channel"))))))

;;;
;;; Operations
;;;

(aio-defun
 supersonic-subsonic--artists () "Return a promise resolving to every artist, via getArtists."
 (supersonic-subsonic--artists-from (aio-await (supersonic-get-json (supersonic-build-url "/getArtists.view" '())))))

(aio-defun
 supersonic-subsonic--artist-albums
 (id)
 "Return a promise resolving to the albums of artist ID, via getArtist."
 (supersonic-subsonic--artist-albums-from
  (aio-await (supersonic-get-json (supersonic-build-url "/getArtist.view" `(("id" . ,id)))))))

(aio-defun
 supersonic-subsonic--album-list
 (type count)
 "Return a promise resolving to at most COUNT albums of list TYPE.
Asks getAlbumList2.  TYPE is one of
`supersonic-provider-album-list-types', each of which is also the
name getAlbumList2 knows that list under."
 (supersonic-subsonic--album-list-from
  (aio-await
   (supersonic-get-json
    (supersonic-build-url
     "/getAlbumList2.view" `(("type" . ,(symbol-name type)) ("size" . ,(number-to-string count))))))))

(aio-defun
 supersonic-subsonic--album-tracks
 (id)
 "Return a promise resolving to the tracks of album ID.
Asks getAlbum when `supersonic-browse-by-tags' is non-nil, and
getMusicDirectory -- ID then being a directory -- otherwise."
 (supersonic-subsonic--album-tracks-from
  (aio-await
   (supersonic-get-json
    (if supersonic-browse-by-tags
        (supersonic-build-url "/getAlbum.view" `(("id" . ,id)))
      (supersonic-build-url "/getMusicDirectory.view" `(("id" . ,id))))))))

(aio-defun
 supersonic-subsonic--song (id) "Return a promise resolving to track ID, via getSong."
 (supersonic-subsonic--song-from (aio-await (supersonic-get-json (supersonic-build-url "/getSong.view" `(("id" . ,id)))))))

(aio-defun
 supersonic-subsonic--search (query) "Return a promise resolving to the search3 results for QUERY."
 (supersonic-subsonic--search-from
  (aio-await (supersonic-get-json (supersonic-build-url "/search3.view" `(("query" . ,query)))))))

(aio-defun
 supersonic-subsonic--podcasts () "Return a promise resolving to every channel, via getPodcasts."
 (supersonic-subsonic--podcasts-from
  (aio-await (supersonic-get-json (supersonic-build-url "/getPodcasts.view" '(("includeEpisodes" . "false")))))))

(aio-defun
 supersonic-subsonic--podcast-episodes
 (id)
 "Return a promise resolving to the episodes of podcast channel ID.
Asks getPodcasts for that one channel, episodes included."
 (supersonic-subsonic--podcast-episodes-from
  (aio-await
   (supersonic-get-json (supersonic-build-url "/getPodcasts.view" `(("id" . ,id) ("includeEpisodes" . "true")))))))

(aio-defun
 supersonic-subsonic--add-podcast (url) "Subscribe to the podcast feed at URL, via createPodcastChannel."
 (aio-await (supersonic-get-json (supersonic-build-url "/createPodcastChannel.view" `(("url" . ,url)))))
 nil)

(aio-defun
 supersonic-subsonic--download-podcast-episode
 (id)
 "Have the server download podcast episode ID, via downloadPodcastEpisode."
 (aio-await (supersonic-get-json (supersonic-build-url "/downloadPodcastEpisode.view" `(("id" . ,id)))))
 nil)

(aio-defun
 supersonic-subsonic--stream-url (id &optional format)
 "Return a promise resolving to the stream.view URL for track ID.
Built locally.  It carries the token-auth triple, which never expires.
FORMAT's `:format' becomes stream.view's `format' (EXT, or \"raw\" for
no transcoding) and `:max-bit-rate' its `maxBitRate'; a key left out
leaves the choice to the server."
 (supersonic-build-url
  "/stream.view"
  (append
   `(("id" . ,id))
   (and (plist-get format :format) `(("format" . ,(plist-get format :format))))
   (and (plist-get format :max-bit-rate)
        `(("maxBitRate" . ,(number-to-string (plist-get format :max-bit-rate))))))))

(defun supersonic-subsonic--scrobble (id now-playing)
  "Scrobble track ID via scrobble.view, as now playing if NOW-PLAYING.
Silently: it happens at every track change, and url-http would say
\"Contacting host\" each time."
  (let ((inhibit-message t))
    (url-retrieve
   (supersonic-build-url
    "/scrobble.view"
    `(("id" . ,id)
      ("submission" .
       ,(if now-playing
            "false"
          "true"))))
     ;; Kill the unread response buffer, which would otherwise stay.  The
     ;; connection is back in its pool by now (`url-http-activate-callback').
     (lambda (_status) (kill-buffer (current-buffer)))
     nil t)))

(defun supersonic-subsonic--cover-art-request-url (art size)
  "Return the getCoverArt URL for cover art ART, scaled to SIZE."
  (supersonic-build-url "/getCoverArt.view" `(("id" . ,art) ("size" . ,(int-to-string size)))))

(defun supersonic-subsonic--image-size (data)
  "Return (WIDTH . HEIGHT) of the JPEG, PNG or WebP image in DATA, or nil.
DATA is a unibyte string; only the header is read."
  (cl-flet ((u8 (i) (aref data i))
            (be16 (i) (+ (* 256 (aref data i)) (aref data (1+ i))))
            (le16 (i) (+ (aref data i) (* 256 (aref data (1+ i)))))
            (le24 (i) (+ (aref data i) (* 256 (aref data (1+ i))) (* 65536 (aref data (+ i 2))))))
    (condition-case nil
        (cond
         ((string-prefix-p "\x89PNG" data)
          (cons (+ (* 65536 (be16 16)) (be16 18)) (+ (* 65536 (be16 20)) (be16 22))))
         ((and (string-prefix-p "RIFF" data) (equal (substring data 8 12) "WEBP"))
          (pcase (substring data 12 16)
            ("VP8X" (cons (1+ (le24 24)) (1+ (le24 27))))
            ("VP8 " (cons (logand (le16 26) #x3fff) (logand (le16 28) #x3fff)))
            ("VP8L" (let ((b (+ (u8 21) (* 256 (u8 22)) (* 65536 (u8 23)) (* 16777216 (u8 24)))))
                      (cons (1+ (logand b #x3fff)) (1+ (logand (ash b -14) #x3fff)))))))
         ((string-prefix-p "\xff\xd8" data)
          (let ((i 2) (found nil))
            (while (and (not found) (< (+ i 9) (length data)))
              (if (/= (u8 i) #xff)
                  (setq i (length data))
                (let ((marker (u8 (1+ i))))
                  (if (memq marker '(#xc0 #xc1 #xc2 #xc3 #xc5 #xc6 #xc7 #xc9 #xca #xcb #xcd #xce #xcf))
                      (setq found (cons (be16 (+ i 7)) (be16 (+ i 5))))
                    (setq i (+ i 2 (be16 (+ i 2))))))))
            found)))
      (error nil))))

(defvar supersonic-subsonic--server-scales nil
  "Non-nil once the server has sent cover art scaled to the size asked for.")

(defun supersonic-subsonic--placeholder-p (data size)
  "Return non-nil if DATA, asked for at SIZE, is a placeholder, not the art.
Navidrome answers with an unscaled placeholder when it cannot get at
an album's art in time, as under many requests at once.  Art much
bigger than asked for is taken for that, but only once the server has
been seen to scale art at all."
  (let ((dimensions (supersonic-subsonic--image-size data)))
    (when dimensions
      (let ((largest (max (car dimensions) (cdr dimensions))))
        (when (<= largest (* 1.1 size))
          (setq supersonic-subsonic--server-scales t))
        (and supersonic-subsonic--server-scales (> largest (* 1.5 size)))))))

(aio-defun
 supersonic-subsonic--cover-art (art size)
 "Return a promise resolving to the bytes of cover art ART at SIZE.
Asks getCoverArt, which scales the image to SIZE on the server."
 (pcase-let ((`(,status . ,buffer)
              (aio-await (supersonic-url-retrieve (supersonic-subsonic--cover-art-request-url art size)))))
   (unwind-protect
       (let ((err (plist-get status :error)))
         (when err
           (error "Failed to fetch cover art %s: %s" art (error-message-string err)))
         (let ((data (with-current-buffer buffer
                       (buffer-substring-no-properties (1+ url-http-end-of-headers) (point-max)))))
           ;; Not to be cached as the art: it would stick for good.
           (when (supersonic-subsonic--placeholder-p data size)
             (error "Cover art %s is not available yet" art))
           data))
     (kill-buffer buffer))))

(aio-defun
 supersonic-subsonic--cover-art-url (art size)
 "Return a promise resolving to the getCoverArt URL for ART at SIZE.
Built locally, and carrying the same token-auth triple as
`supersonic-subsonic--stream-url'."
 (supersonic-subsonic--cover-art-request-url art size))

(defun supersonic-subsonic--structured-lyrics (entry)
  "Turn ENTRY, one OpenSubsonic structuredLyrics object, into lyrics.
A synced ENTRY's `offset' is applied to every line, LRC style: a
positive offset makes lines show sooner.  Its `lang' becomes `:lang'."
  (let ((synced (eq (assoc-default "synced" entry) t))
        (offset (or (assoc-default "offset" entry) 0)))
    (list :synced synced
          :lines (mapcar (lambda (line)
                           (cons (and synced (max 0 (- (or (assoc-default "start" line) 0) offset)))
                                 (or (assoc-default "value" line) "")))
                         (assoc-default "line" entry))
          :lang (assoc-default "lang" entry)
          :source "server")))

(defun supersonic-subsonic--lyrics-from (data)
  "Return the best lyrics in getLyricsBySongId DATA: synced over unsynced.
The other entries, typically the same lyrics in other languages, come
along as `:alternatives'."
  (let* ((entries (seq-filter (lambda (e) (assoc-default "line" e))
                              (supersonic-recursive-assoc data '("subsonic-response" "lyricsList" "structuredLyrics"))))
         (entry (or (seq-find (lambda (e) (eq (assoc-default "synced" e) t)) entries)
                    (car entries))))
    (and entry
         (let ((lyrics (supersonic-subsonic--structured-lyrics entry))
               (others (mapcar #'supersonic-subsonic--structured-lyrics (remq entry entries))))
           (if others
               (append lyrics (list :alternatives others))
             lyrics)))))

(aio-defun
 supersonic-subsonic--lyrics (id)
 "Return a promise resolving to the lyrics of track ID, or nil.
Asks the OpenSubsonic getLyricsBySongId, which can hand out synced
lyrics -- from embedded tags or .lrc files next to the music -- and
falls back on the plain text of the original getLyrics for servers
without it."
 (let ((structured (condition-case nil
                       (list (aio-await
                              (supersonic-get-json
                               (supersonic-build-url "/getLyricsBySongId.view" `(("id" . ,id))))))
                     (error nil))))
   (if structured
       (supersonic-subsonic--lyrics-from (car structured))
     (let* ((track (aio-await (supersonic-subsonic--song id)))
            (data (aio-await
                   (supersonic-get-json
                    (supersonic-build-url
                     "/getLyrics.view"
                     `(("artist" . ,(or (plist-get track :artist) ""))
                       ("title" . ,(or (plist-get track :title) "")))))))
            (text (assoc-default "value" (supersonic-recursive-assoc data '("subsonic-response" "lyrics")))))
       (and text (not (string-empty-p (string-trim text)))
            (list :synced nil
                  :lines (mapcar (lambda (line) (cons nil line)) (split-string (string-trim text) "\\r?\\n"))
                  :source "server"))))))

(aio-defun
 supersonic-subsonic--playlists () "Return a promise resolving to every playlist, via getPlaylists."
 (mapcar #'supersonic-subsonic--playlist
         (supersonic-recursive-assoc
          (aio-await (supersonic-get-json (supersonic-build-url "/getPlaylists.view" nil)))
          '("subsonic-response" "playlists" "playlist"))))

(aio-defun
 supersonic-subsonic--playlist-tracks (id) "Return a promise resolving to playlist ID's tracks, via getPlaylist."
 (mapcar #'supersonic-subsonic--track
         (supersonic-recursive-assoc
          (aio-await (supersonic-get-json (supersonic-build-url "/getPlaylist.view" `(("id" . ,id)))))
          '("subsonic-response" "playlist" "entry"))))

(defun supersonic-subsonic--song-params (key ids)
  "Return the query pairs passing IDS, each under KEY."
  (mapcar (lambda (id) (cons key id)) ids))

(aio-defun
 supersonic-subsonic--create-playlist (name ids)
 "Create playlist NAME of track IDS via createPlaylist; resolve to it.
Tracks past the first hundred are added after, keeping URLs short."
 (let* ((data (aio-await (supersonic-get-json
                          (supersonic-build-url
                           "/createPlaylist.view"
                           (cons (cons "name" name) (supersonic-subsonic--song-params "songId" (seq-take ids 100)))))))
        (playlist (supersonic-recursive-assoc data '("subsonic-response" "playlist")))
        (made (and playlist (supersonic-subsonic--playlist playlist))))
   (when (and made (nthcdr 100 ids))
     (aio-await (supersonic-subsonic--edit-playlist (plist-get made :id) (list :add (nthcdr 100 ids)))))
   made))

(aio-defun
 supersonic-subsonic--edit-playlist (id changes)
 "Make CHANGES to playlist ID via updatePlaylist, a hundred tracks at a time."
 (let ((remove (sort (copy-sequence (plist-get changes :remove)) #'<))
       (add (plist-get changes :add))
       (first t))
   (while (or first add)
     (aio-await
      (supersonic-get-json
       (supersonic-build-url
        "/updatePlaylist.view"
        (append `(("playlistId" . ,id))
                (and first (plist-get changes :name) `(("name" . ,(plist-get changes :name))))
                (and first (mapcar (lambda (i) (cons "songIndexToRemove" (number-to-string i))) remove))
                (supersonic-subsonic--song-params "songIdToAdd" (seq-take add 100))))))
     (setq first nil
           add (nthcdr 100 add)))
   nil))

(aio-defun
 supersonic-subsonic--set-playlist-tracks (id ids)
 "Make track IDS playlist ID's tracks, in order.
createPlaylist with a playlistId replaces the tracks; past the first
hundred, the rest are appended."
 (aio-await (supersonic-get-json
             (supersonic-build-url
              "/createPlaylist.view"
              (cons (cons "playlistId" id) (supersonic-subsonic--song-params "songId" (seq-take ids 100))))))
 (when (nthcdr 100 ids)
   (aio-await (supersonic-subsonic--edit-playlist id (list :add (nthcdr 100 ids)))))
 nil)

(aio-defun
 supersonic-subsonic--delete-playlist (id) "Delete playlist ID via deletePlaylist."
 (aio-await (supersonic-get-json (supersonic-build-url "/deletePlaylist.view" `(("id" . ,id)))))
 nil)

(aio-defun
 supersonic-subsonic--play-queue ()
 "Return a promise resolving to the queue saved on the server, via getPlayQueue."
 (let* ((data (aio-await (supersonic-get-json (supersonic-build-url "/getPlayQueue.view" nil))))
        (queue (supersonic-recursive-assoc data '("subsonic-response" "playQueue")))
        (ids (mapcar #'supersonic-get-id-as-string (assoc-default "entry" queue)))
        (current (assoc-default "current" queue)))
   (and ids
        (list :ids ids
              :current (or (and current (seq-position ids (format "%s" current))) 0)
              :position (/ (or (assoc-default "position" queue) 0) 1000.0)))))

(aio-defun
 supersonic-subsonic--save-play-queue (ids index position)
 "Save IDS, playing INDEX at POSITION seconds, via savePlayQueue.
At most `supersonic-resume-max-tracks' around the one playing, keeping
the URL short."
 (let* ((start (max 0 (- index 20)))
        (kept (seq-take (nthcdr start ids) supersonic-resume-max-tracks))
        (current (nth index ids)))
   (aio-await
    (supersonic-get-json
     (supersonic-build-url
      "/savePlayQueue.view"
      (append (supersonic-subsonic--song-params "id" kept)
              (and current `(("current" . ,current)))
              `(("position" . ,(number-to-string (round (* 1000 (or position 0))))))))))
   nil))

(aio-defun
 supersonic-subsonic--similar-tracks (id count)
 "Return a promise resolving to at most COUNT tracks like ID's.
Asks getSimilarSongs.
ID may be a song's, an album's or an artist's; the server asks its
external agents, such as Last.fm, which may know nothing."
 (mapcar #'supersonic-subsonic--track
         (supersonic-recursive-assoc
          (aio-await (supersonic-get-json (supersonic-build-url "/getSimilarSongs.view"
                                                                `(("id" . ,id) ("count" . ,(number-to-string count))))))
          '("subsonic-response" "similarSongs" "song"))))

(aio-defun
 supersonic-subsonic--random-tracks (count &optional genre)
 "Return a promise resolving to at most COUNT random tracks, via getRandomSongs."
 (mapcar #'supersonic-subsonic--track
         (supersonic-recursive-assoc
          (aio-await (supersonic-get-json (supersonic-build-url "/getRandomSongs.view"
                                                                (append `(("size" . ,(number-to-string count)))
                                                                        (and genre `(("genre" . ,genre)))))))
          '("subsonic-response" "randomSongs" "song"))))

(aio-defun
 supersonic-subsonic--star (id kind flag)
 "Make ID, a KIND, a favourite if FLAG, else not, via star or unstar."
 (aio-await (supersonic-get-json
             (supersonic-build-url (if flag "/star.view" "/unstar.view")
                                   (list (cons (pcase kind ('album "albumId") ('artist "artistId") (_ "id")) id)))))
 nil)

(aio-defun
 supersonic-subsonic--starred () "Return a promise resolving to the favourites, via getStarred2."
 (let ((starred (supersonic-recursive-assoc
                 (aio-await (supersonic-get-json (supersonic-build-url "/getStarred2.view" nil)))
                 '("subsonic-response" "starred2"))))
   (list :tracks (mapcar #'supersonic-subsonic--track (assoc-default "song" starred))
         :albums (mapcar #'supersonic-subsonic--album (assoc-default "album" starred))
         :artists (mapcar #'supersonic-subsonic--artist (assoc-default "artist" starred)))))

(defun supersonic-subsonic--cache-namespace ()
  "Return the Subsonic server's address, `supersonic-host'.
Ids are only unique per server, so this is what keeps one server's
cached art and waveforms apart from another's."
  (or supersonic-host ""))

(defun supersonic-subsonic--config-hints ()
  "Return what to check when a request to the Subsonic server fails."
  '("Check that supersonic-host is configured correctly"
    "Ensure the scheme (http:// or https://) matches your server"
    "Verify .authinfo has the correct host (must match supersonic-host exactly), or what supersonic-auth-function returns"))

(supersonic-provider-register
 'subsonic
 `((artists . supersonic-subsonic--artists)
   (artist-albums . supersonic-subsonic--artist-albums)
   (album-list . supersonic-subsonic--album-list)
   (album-tracks . supersonic-subsonic--album-tracks)
   (track . supersonic-subsonic--song)
   (search . supersonic-subsonic--search)
   (podcasts . supersonic-subsonic--podcasts)
   (podcast-episodes . supersonic-subsonic--podcast-episodes)
   (add-podcast . supersonic-subsonic--add-podcast)
   (download-podcast-episode . supersonic-subsonic--download-podcast-episode)
   (stream-url . supersonic-subsonic--stream-url)
   (scrobble . supersonic-subsonic--scrobble)
   (cover-art . supersonic-subsonic--cover-art)
   (cover-art-url . supersonic-subsonic--cover-art-url)
   (lyrics . supersonic-subsonic--lyrics)
   (playlists . supersonic-subsonic--playlists)
   (playlist-tracks . supersonic-subsonic--playlist-tracks)
   (create-playlist . supersonic-subsonic--create-playlist)
   (edit-playlist . supersonic-subsonic--edit-playlist)
   (set-playlist-tracks . supersonic-subsonic--set-playlist-tracks)
   (delete-playlist . supersonic-subsonic--delete-playlist)
   (play-queue . supersonic-subsonic--play-queue)
   (save-play-queue . supersonic-subsonic--save-play-queue)
   (similar-tracks . supersonic-subsonic--similar-tracks)
   (random-tracks . supersonic-subsonic--random-tracks)
   (star . supersonic-subsonic--star)
   (starred . supersonic-subsonic--starred)
   (cache-namespace . supersonic-subsonic--cache-namespace)
   (config-hints . supersonic-subsonic--config-hints)))

(provide 'supersonic-subsonic)
;;; supersonic-subsonic.el ends here
