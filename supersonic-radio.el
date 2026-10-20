;;; supersonic-radio.el --- Radio and autoplay: never run out of music -*- lexical-binding: t; -*-

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

;; `supersonic-autoplay-mode' keeps the music going: when the last track
;; of the queue starts, tracks like it are added after it -- from the
;; provider's `similar-tracks', which on a Subsonic server asks its
;; external agents, such as Last.fm -- or, failing those, random ones of
;; its genre, or random ones at all.  Tracks already queued are left out.
;;
;; `supersonic-radio' starts that from a track, an album or an artist:
;; it first, then what is like it, carried on by autoplay.

;;; Code:

(require 'aio)
(require 'seq)

(require 'supersonic-custom)
(require 'supersonic-provider)
(require 'supersonic-playback)

(defvar supersonic-radio--extending nil
  "Non-nil while tracks are being added, so a burst of hooks adds once.")

(aio-defun
 supersonic-radio--more (seed exclude count)
 "Return a promise resolving to at most COUNT track ids to play after SEED.
SEED is a track plist; ids in EXCLUDE are left out.  Similar tracks
first, then random ones of SEED's genre, then random ones."
 (let ((found nil))
   (cl-flet ((take (tracks)
               (dolist (track tracks)
                 (let ((id (plist-get track :id)))
                   (unless (or (member id exclude) (member id found) (>= (length found) count))
                     (push id found))))))
     (when (supersonic-provider-supports-p 'similar-tracks)
       (take (condition-case nil
                 (aio-await (supersonic-provider-similar-tracks (plist-get seed :id) (* 2 count)))
               (error nil))))
     (when (and (< (length found) count) (supersonic-provider-supports-p 'random-tracks))
       (when (plist-get seed :genre)
         (take (condition-case nil
                   (aio-await (supersonic-provider-random-tracks (* 2 count) (plist-get seed :genre)))
                 (error nil))))
       (when (< (length found) count)
         (take (condition-case nil (aio-await (supersonic-provider-random-tracks (* 2 count))) (error nil))))))
   (nreverse found)))

(aio-defun
 supersonic-radio--extend ()
 "Add tracks after the queue's last one, if it is the one playing."
 (when (and (not supersonic-radio--extending) (supersonic-playback-live-p))
   (setq supersonic-radio--extending t)
   (unwind-protect
       (condition-case err
           (let* ((queue (aio-await (supersonic-playback-queue)))
                  (current (seq-position queue t (lambda (entry _) (plist-get entry :current)))))
             (when (and current (= current (1- (length queue))))
               (let* ((seed (aio-await (supersonic-provider-track (plist-get (nth current queue) :track-id))))
                      (ids (aio-await (supersonic-radio--more
                                       seed (mapcar (lambda (entry) (plist-get entry :track-id)) queue)
                                       supersonic-autoplay-count))))
                 (when ids
                   (supersonic-playback-enqueue ids)
                   (message "[Supersonic] Autoplay: %d more like %s" (length ids) (or (plist-get seed :title) "this"))))))
         (error (supersonic--log "autoplay failed: %s" (error-message-string err))))
     (setq supersonic-radio--extending nil))))

(defun supersonic-radio--on-track-change ()
  "Extend the queue if its last track has started.  On the track hook."
  (ignore (supersonic-radio--extend)))

;;;###autoload
(define-minor-mode supersonic-autoplay-mode
  "When the last track of the queue starts, add tracks like it after it."
  :global t
  :group 'supersonic
  (if supersonic-autoplay-mode
      (add-hook 'supersonic-playback-track-change-hook #'supersonic-radio--on-track-change)
    (remove-hook 'supersonic-playback-track-change-hook #'supersonic-radio--on-track-change)))

;;;###autoload (autoload 'supersonic-radio "supersonic-radio" nil t)
(aio-defun
 supersonic-radio (id &optional first-ids)
 "Play a radio from ID: FIRST-IDS, then tracks like ID.
ID is a track's, an album's or an artist's; FIRST-IDS default to ID
itself, taken as a track.  Interactively, from the track playing.
With `supersonic-autoplay-mode' on, it plays on and on."
 (interactive (list (or (aio-wait-for (supersonic-playback-status 'track-id))
                        (user-error "Nothing playing to start a radio from"))))
 (let* ((first-ids (or first-ids (list id)))
        (seed (list :id id :genre (and (= (length first-ids) 1)
                                       (condition-case nil
                                           (plist-get (aio-await (supersonic-provider-track (car first-ids))) :genre)
                                         (error nil)))))
        (more (aio-await (supersonic-radio--more seed first-ids supersonic-autoplay-count))))
   (supersonic-playback-start (append first-ids more))
   (message "[Supersonic] Radio: %d track(s) to start with%s" (+ (length first-ids) (length more))
            (if supersonic-autoplay-mode ", and more as it goes" ""))))

(provide 'supersonic-radio)

;;; supersonic-radio.el ends here
