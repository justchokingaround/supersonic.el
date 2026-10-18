;;; supersonic-mode-line.el --- Show what supersonic plays in the mode line -*- lexical-binding: t; -*-

;; Author: justchokingaround
;; Assisted-by: Claude:claude-opus-5-5
;; URL: https://github.com/systemfreund/supersonic.el
;; Keywords: multimedia

;; This program is free software; you can redistribute it and/or modify
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

;; `supersonic-mode-line-mode' puts the track playing -- its title and
;; artist, with a pause marker -- into `global-mode-string', the way
;; `emms-mode-line-mode' does.  It follows the playback facade's hooks,
;; so it works with every backend.  Clicking it opens the now-playing
;; buffer.

;;; Code:

(require 'aio)

(require 'supersonic-custom)
(require 'supersonic-provider)
(require 'supersonic-playback)

(declare-function supersonic-show-now-playing "supersonic")

(defvar supersonic-mode-line-string ""
  "What `supersonic-mode-line-mode' shows, kept current by its hooks.")
;;;###autoload
(put 'supersonic-mode-line-string 'risky-local-variable t)

(defvar supersonic-mode-line--track nil
  "The track last shown, as a plist from `supersonic-provider-track'.")

(defvar supersonic-mode-line--generation 0
  "Bumped on every update, so a slow track lookup cannot overwrite a newer one.")

(defvar supersonic-mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'supersonic-show-now-playing)
    (define-key map [mode-line mouse-3] #'supersonic-toggle-playing)
    ;; For a header line showing `supersonic-mode-line-string', as some
    ;; mode-line packages draw their bar there.
    (define-key map [header-line mouse-1] #'supersonic-show-now-playing)
    (define-key map [header-line mouse-3] #'supersonic-toggle-playing)
    map)
  "Keymap active on the mode-line indicator.")

(defun supersonic-mode-line--format (track paused)
  "Return the indicator for TRACK, PAUSED or not; \"\" if TRACK is nil."
  (if (not track)
      ""
    (let* ((label (string-join
                   (delq nil (list (plist-get track :title) (plist-get track :artist)))
                   " — "))
           (label (truncate-string-to-width label supersonic-mode-line-max-width nil nil "…")))
      (concat
       " "
       (propertize
        (concat (if paused supersonic-mode-line-paused-icon supersonic-mode-line-playing-icon) " " label)
        'face 'supersonic-mode-line
        'mouse-face 'mode-line-highlight
        'help-echo "mouse-1: now playing, mouse-3: play/pause"
        'local-map supersonic-mode-line-map)
       " "))))

(aio-defun
 supersonic-mode-line-update ()
 "Bring the mode-line indicator up to date with the active backend."
 (let ((generation (setq supersonic-mode-line--generation (1+ supersonic-mode-line--generation))))
   (condition-case err
       (let* ((track-id (aio-await (supersonic-playback-status 'track-id)))
              (paused (and track-id (aio-await (supersonic-playback-status 'paused))))
              (track (cond
                      ((not track-id) nil)
                      ((equal track-id (plist-get supersonic-mode-line--track :id))
                       supersonic-mode-line--track)
                      (t (aio-await (supersonic-provider-track track-id))))))
         (when (= generation supersonic-mode-line--generation)
           (setq supersonic-mode-line--track track)
           (setq supersonic-mode-line-string (supersonic-mode-line--format track paused))
           (force-mode-line-update t)))
     (error
      (supersonic--log "mode-line update failed: %s" (error-message-string err))))))

(defun supersonic-mode-line--update-soon ()
  "Run `supersonic-mode-line-update', not awaiting it."
  (ignore (supersonic-mode-line-update)))

;;;###autoload
(define-minor-mode supersonic-mode-line-mode
  "Show the track supersonic is playing in the mode line."
  :global t
  :group 'supersonic-mode-line
  (setq global-mode-string (delq 'supersonic-mode-line-string (or global-mode-string '(""))))
  (dolist (hook '(supersonic-playback-track-change-hook supersonic-playback-state-change-hook))
    (remove-hook hook #'supersonic-mode-line--update-soon))
  (setq supersonic-mode-line-string "")
  (when supersonic-mode-line-mode
    (setq global-mode-string (append global-mode-string '(supersonic-mode-line-string)))
    (dolist (hook '(supersonic-playback-track-change-hook supersonic-playback-state-change-hook))
      (add-hook hook #'supersonic-mode-line--update-soon))
    (supersonic-mode-line--update-soon))
  (force-mode-line-update t))

(provide 'supersonic-mode-line)

;;; supersonic-mode-line.el ends here
