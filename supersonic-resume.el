;;; supersonic-resume.el --- Pick up where you left off -*- lexical-binding: t; -*-

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

;; `supersonic-resume-mode' saves the play queue, the track playing and
;; how far into it, through the provider -- on a Subsonic server with
;; savePlayQueue, so other clients can pick it up too -- whenever the
;; track or the queue changes, every `supersonic-resume-interval'
;; seconds while playing, and when Emacs exits.  Turned on with nothing
;; playing, it puts that queue back, paused where it was, if
;; `supersonic-resume-on-start' says so; `supersonic-resume' does the
;; same on demand.  Nothing plays until you say so.

;;; Code:

(require 'aio)
(require 'seq)

(require 'supersonic-custom)
(require 'supersonic-provider)
(require 'supersonic-playback)

(defvar supersonic-resume--save-timer nil "Timer saving a moment after a change.")
(defvar supersonic-resume--interval-timer nil "Timer saving every so often while playing.")
(defvar supersonic-resume--restoring nil "Non-nil while a restore runs, so it is not saved over.")

(aio-defun
 supersonic-resume--state ()
 "Return a promise resolving to (IDS INDEX POSITION) of playback now, or nil."
 (when (supersonic-playback-live-p)
   (let* ((queue (aio-await (supersonic-playback-queue)))
          (ids (mapcar (lambda (entry) (plist-get entry :track-id)) queue))
          (index (seq-position queue t (lambda (entry _) (plist-get entry :current))))
          (position (aio-await (supersonic-playback-status 'position))))
     (and ids (not (memq nil ids)) index
          (list ids index (if (numberp position) position 0))))))

(aio-defun
 supersonic-resume-save ()
 "Save the play queue and where in it playback is."
 (interactive)
 (when (and (not supersonic-resume--restoring) (supersonic-provider-supports-p 'save-play-queue))
   (condition-case err
       (let ((state (aio-await (supersonic-resume--state))))
         (when state
           (aio-await (apply #'supersonic-provider-save-play-queue state))
           (when (called-interactively-p 'any)
             (message "[Supersonic] Saved the queue, %d track(s)" (length (car state))))))
     (error (supersonic--log "saving the queue failed: %s" (error-message-string err))))))

(defun supersonic-resume--save-soon ()
  "Save a moment from now, once a burst of changes has settled."
  (when supersonic-resume--save-timer
    (cancel-timer supersonic-resume--save-timer))
  (setq supersonic-resume--save-timer
        (run-at-time 3 nil (lambda ()
                             (setq supersonic-resume--save-timer nil)
                             (ignore (supersonic-resume-save))))))

(defun supersonic-resume--save-on-exit ()
  "Save before Emacs exits, waiting a little for it."
  (when (supersonic-playback-live-p)
    (with-timeout (3 nil)
      (condition-case nil
          (aio-wait-for (supersonic-resume-save))
        (error nil)))))

;;;###autoload (autoload 'supersonic-resume "supersonic-resume" nil t)
(aio-defun
 supersonic-resume ()
 "Put back the play queue saved last, paused where it was."
 (interactive)
 (supersonic-provider-require 'play-queue)
 (let ((saved (aio-await (supersonic-provider-play-queue))))
   (if (not saved)
       (message "[Supersonic] No saved queue to pick up")
     (setq supersonic-resume--restoring t)
     (unwind-protect
         (supersonic-playback-restore (plist-get saved :ids) (plist-get saved :current) (plist-get saved :position))
       ;; Saving resumes once the restored queue has settled in mpv.
       (run-at-time 8 nil (lambda () (setq supersonic-resume--restoring nil))))
     (message "[Supersonic] Picked up where you left off: %d track(s), paused" (length (plist-get saved :ids))))))

(defun supersonic-resume--after-hang ()
  "Put the saved queue back into the mpv started anew after one hung."
  (ignore (supersonic-resume)))

(defconst supersonic-resume--hooks
  '((supersonic-playback-track-change-hook . supersonic-resume--save-soon)
    (supersonic-playback-queue-change-hook . supersonic-resume--save-soon)
    (supersonic-playback-state-change-hook . supersonic-resume--save-soon)
    (supersonic-mpv-hang-hook . supersonic-resume--after-hang)
    (kill-emacs-hook . supersonic-resume--save-on-exit))
  "When the queue is saved.")

;;;###autoload
(define-minor-mode supersonic-resume-mode
  "Save the play queue as it changes, and put it back when Emacs starts.
See the Commentary of `supersonic-resume.el'."
  :global t
  :group 'supersonic
  (dolist (hook supersonic-resume--hooks)
    (remove-hook (car hook) (cdr hook)))
  (when supersonic-resume--interval-timer
    (cancel-timer supersonic-resume--interval-timer)
    (setq supersonic-resume--interval-timer nil))
  (when supersonic-resume-mode
    (dolist (hook supersonic-resume--hooks)
      (add-hook (car hook) (cdr hook)))
    (setq supersonic-resume--interval-timer
          (run-at-time supersonic-resume-interval supersonic-resume-interval
                       (lambda ()
                         (when (supersonic-playback-live-p)
                           (ignore (supersonic-resume-save))))))
    (when (and supersonic-resume-on-start
               (not (ignore-errors (supersonic-playback-live-p)))
               (supersonic-provider-supports-p 'play-queue))
      (ignore (supersonic-resume)))))

(provide 'supersonic-resume)

;;; supersonic-resume.el ends here
