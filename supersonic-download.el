;;; supersonic-download.el --- Save music from the server, for offline use -*- lexical-binding: t; -*-

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

;; `supersonic-download-tracks' saves songs as the server has them --
;; the original files, tags and all -- under
;; `supersonic-download-directory', as Artist/Album (Year)/NN - Title,
;; with the album's cover.jpg and an .lrc file of the lyrics next to
;; them.  Songs already there are skipped.  With that directory among
;; `supersonic-local-directories', the `local' provider plays them
;; without the server, and is rescanned once a download is done.
;;
;; Files are fetched one after another by curl, which is handed its
;; URLs on standard input: they carry the server's credentials, which
;; must not show in a process listing.

;;; Code:

(require 'aio)
(require 'cl-lib)
(require 'subr-x)

(require 'supersonic-custom)
(require 'supersonic-provider)

(declare-function supersonic-lyrics-lookup "supersonic-lyrics")
(declare-function supersonic-local-rescan "supersonic-local")

(defvar supersonic-download--running nil
  "Non-nil while songs are being downloaded.")

(defvar supersonic-download--pending nil
  "Tracks waiting their turn to be downloaded.")

(defun supersonic-download--part (string)
  "Return STRING made safe as one part of a file name."
  (let ((part (string-trim (replace-regexp-in-string "[/\\\\:*?\"<>|[:cntrl:]]" "_" (or string "")))))
    (if (member part '("" "." "..")) "_" part)))

(defun supersonic-download--album-directory (track)
  "Return the directory TRACK's album is saved to."
  (expand-file-name
   (concat (supersonic-download--part (or (plist-get track :artist) "Unknown artist")) "/"
           (supersonic-download--part (concat (or (plist-get track :album) "Unknown album")
                                              (and (plist-get track :year) (format " (%s)" (plist-get track :year)))))
           "/")
   supersonic-download-directory))

(defun supersonic-download--file (track)
  "Return the file name TRACK is saved under."
  (let ((number (plist-get track :track))
        (disc (plist-get track :disc)))
    (expand-file-name
     (concat (cond ((and number disc (> disc 1)) (format "%d-%02d - " disc number))
                   (number (format "%02d - " number)))
             (supersonic-download--part (or (plist-get track :title) (plist-get track :id)))
             "." (or (plist-get track :suffix) "mp3"))
     (supersonic-download--album-directory track))))

(defun supersonic-download--lrc (lyrics)
  "Return LYRICS as the text of an .lrc file."
  (mapconcat (lambda (line)
               (let ((ms (car line)))
                 (concat (and ms (format "[%02d:%05.2f]" (/ ms 60000) (/ (mod ms 60000) 1000.0)))
                         (cdr line))))
             (plist-get lyrics :lines)
             "\n"))

(defun supersonic-download--config-string (string)
  "Return STRING quoted for a curl config file."
  (concat "\"" (replace-regexp-in-string "[\"\\\\]" "\\\\\\&" string) "\"\n"))

(defun supersonic-download--fetch (url file)
  "Return a promise of fetching URL into FILE, resolving to non-nil on success.
The file appears only once it is complete."
  (let ((promise (aio-promise))
        (partial (concat file ".part")))
    (make-directory (file-name-directory file) t)
    (let ((process
           (make-process
            :name "supersonic-download"
            :command '("curl" "--silent" "--show-error" "--fail" "--location" "--config" "-")
            :connection-type 'pipe
            :noquery t
            :buffer nil
            :sentinel
            (lambda (process _event)
              (unless (process-live-p process)
                (let ((ok (and (eq (process-status process) 'exit) (zerop (process-exit-status process)))))
                  (if ok
                      (rename-file partial file t)
                    (when (file-exists-p partial) (delete-file partial)))
                  (aio-resolve promise (lambda () ok))))))))
      (process-send-string process (concat "url = " (supersonic-download--config-string url)
                                           "output = " (supersonic-download--config-string partial)))
      (process-send-eof process))
    promise))

(aio-defun
 supersonic-download--track (track)
 "Download TRACK, its album's cover and its lyrics, unless already there.
Resolves to non-nil if the song was fetched now."
 (let ((file (supersonic-download--file track)))
   (unless (file-exists-p file)
     (unless (aio-await (supersonic-download--fetch
                         (aio-await (supersonic-provider-stream-url (plist-get track :id) '(:format "raw")))
                         file))
       (error "Could not download %s" (or (plist-get track :title) (plist-get track :id))))
     (let ((cover (expand-file-name "cover.jpg" (file-name-directory file))))
       (when (and (plist-get track :art) (not (file-exists-p cover)))
         (aio-await (supersonic-download--fetch
                     (aio-await (supersonic-provider-cover-art-url (plist-get track :art) nil))
                     cover))))
     (when (require 'supersonic-lyrics nil t)
       (let ((outcome (aio-await (aio-catch (supersonic-lyrics-lookup (plist-get track :id) track)))))
         (when (and (eq (car outcome) :success) (plist-get (cdr outcome) :lines))
           (let ((coding-system-for-write 'utf-8))
             (write-region (supersonic-download--lrc (cdr outcome)) nil
                           (concat (file-name-sans-extension file) ".lrc") nil 'silent)))))
     t)))

(aio-defun
 supersonic-download--run ()
 "Download the pending tracks one after another, saying how it goes."
 (let ((fetched 0) (failed 0) (done 0))
   (while supersonic-download--pending
     (let* ((track (pop supersonic-download--pending))
            (outcome (aio-await (aio-catch (supersonic-download--track track)))))
       (setq done (1+ done))
       (if (eq (car outcome) :success)
           (when (cdr outcome) (setq fetched (1+ fetched)))
         (setq failed (1+ failed))
         (supersonic--log "download failed: %s" (error-message-string (cdr outcome))))
       (message "[Supersonic] Downloading: %d done, %d to go" done (length supersonic-download--pending))))
   (setq supersonic-download--running nil)
   (message "[Supersonic] Downloaded %d song(s)%s to %s" fetched
            (if (> failed 0) (format ", %d failed" failed) "")
            (abbreviate-file-name (expand-file-name supersonic-download-directory)))
   (when (and (> fetched 0) (featurep 'supersonic-local))
     (supersonic-local-rescan))))

(defun supersonic-download-tracks (tracks)
  "Save TRACKS, track plists, under `supersonic-download-directory'.
They join any download running."
  (when (eq supersonic-provider 'local)
    (user-error "These are local files already"))
  (unless (executable-find "curl")
    (user-error "Downloading needs curl"))
  (setq supersonic-download--pending (append supersonic-download--pending tracks))
  (unless supersonic-download--running
    (setq supersonic-download--running t)
    (ignore (supersonic-download--run))))

(provide 'supersonic-download)

;;; supersonic-download.el ends here
