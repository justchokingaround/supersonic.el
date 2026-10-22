;;; supersonic-consult.el --- Search the library as you type, with consult -*- lexical-binding: t; -*-

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

;; `supersonic-consult' searches the library from the minibuffer while
;; one types, through the consult package (not a dependency of
;; Supersonic: install it to use this).  Artists, albums and songs come
;; grouped; RET plays a song or opens an album's or an artist's page in
;; the library browser, and with a prefix argument adds whichever it is
;; to the queue instead.

;;; Code:

(require 'aio)
(require 'cl-lib)
(require 'seq)

(require 'supersonic-custom)
(require 'supersonic-provider)
(require 'supersonic-playback)

(declare-function consult--read "ext:consult")
(declare-function consult--dynamic-collection "ext:consult")
(declare-function consult--lookup-member "ext:consult")
(declare-function supersonic-browse-visit "supersonic-browse")
(declare-function supersonic-browse--track-ids "supersonic-browse")
(declare-function supersonic-browse--duration "supersonic-browse")

(defconst supersonic-consult--groups
  '((artist . "Artists") (album . "Albums") (track . "Songs"))
  "Title of each kind's group.")

(defun supersonic-consult--candidates (results)
  "Return the candidates for RESULTS, a search result plist.
Each is a string naming the item, which it carries along as the
`supersonic-consult' property, (KIND . ITEM)."
  (let ((index 0))
    (cl-flet ((candidate (kind item text)
                ;; An invisible serial keeps namesakes apart.
                (propertize (concat text (propertize (format "\0%d" (cl-incf index)) 'invisible t))
                            'supersonic-consult (cons kind item))))
      (append
       (mapcar (lambda (artist) (candidate 'artist artist (or (plist-get artist :name) "?")))
               (plist-get results :artists))
       (mapcar (lambda (album)
                 (candidate 'album album
                            (concat (or (plist-get album :name) "?")
                                    (and (plist-get album :artist)
                                         (propertize (concat "  " (plist-get album :artist)) 'face 'shadow)))))
               (plist-get results :albums))
       (mapcar (lambda (track)
                 (candidate 'track track
                            (concat (or (plist-get track :title) "?")
                                    (and (plist-get track :artist)
                                         (propertize (concat "  " (plist-get track :artist)) 'face 'shadow)))))
               (plist-get results :tracks))))))

(defun supersonic-consult--item (candidate)
  "Return (KIND . ITEM) that CANDIDATE stands for."
  (get-text-property 0 'supersonic-consult candidate))

(defun supersonic-consult--group (candidate transform)
  "Return CANDIDATE's group title, or CANDIDATE itself if TRANSFORM."
  (if transform
      candidate
    (alist-get (car (supersonic-consult--item candidate)) supersonic-consult--groups)))

(defun supersonic-consult--annotate (candidate)
  "Return the annotation of CANDIDATE: a song's length, an album's year."
  (let* ((item (cdr (supersonic-consult--item candidate)))
         (note (pcase (car (supersonic-consult--item candidate))
                 ('track (and (plist-get item :duration) (supersonic-browse--duration (plist-get item :duration))))
                 ('album (and (plist-get item :year) (number-to-string (plist-get item :year)))))))
    (and note (concat "  " (propertize note 'face 'shadow)))))

(defun supersonic-consult--act (kind item enqueue)
  "Play a song, or open the page of anything else, ITEM of KIND.
With ENQUEUE, add ITEM's songs to the queue instead."
  (cond
   (enqueue
    (aio-listen (aio-catch (supersonic-browse--track-ids kind item))
                (lambda (value)
                  (let ((outcome (funcall value)))
                    (if (eq (car outcome) :success)
                        (progn (supersonic-playback-enqueue (cdr outcome))
                               (message "[Supersonic] Queued %d song(s)" (length (cdr outcome))))
                      (message "[Supersonic] Could not queue that: %s" (cdr outcome)))))))
   ((eq kind 'track) (supersonic-playback-start (list (plist-get item :id))))
   (t (supersonic-browse-visit (list :kind kind :id (plist-get item :id) :item item)))))

;;;###autoload
(defun supersonic-consult (enqueue)
  "Search the library as you type, and play or open what is chosen.
A song plays; an album or artist opens in the library browser.  With
prefix argument ENQUEUE, add the songs of the choice to the queue."
  (interactive "P")
  (unless (require 'consult nil t)
    (user-error "`supersonic-consult' needs the consult package"))
  (require 'supersonic-browse)
  (let* ((choice (consult--read
                  (consult--dynamic-collection
                      (lambda (input)
                        (supersonic-consult--candidates (aio-wait-for (supersonic-provider-search input)))))
                  :prompt (if enqueue "Queue: " "Play or open: ")
                  :lookup #'consult--lookup-member
                  :group #'supersonic-consult--group
                  :annotate #'supersonic-consult--annotate
                  :require-match t
                  :sort nil
                  :category 'supersonic))
         (item (and choice (supersonic-consult--item choice))))
    (when item
      (supersonic-consult--act (car item) (cdr item) enqueue))))

(provide 'supersonic-consult)

;;; supersonic-consult.el ends here
