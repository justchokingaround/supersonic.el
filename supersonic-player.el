;;; supersonic-player.el --- A now-playing page in the browser's style -*- lexical-binding: t; -*-

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

;; `supersonic-player' shows what is playing the way the library
;; browser shows the library: one centred column -- the cover, the
;; title, the artist and album (links into the browser), a progress
;; bar to click to seek, the transport controls, the volume, the line
;; of the lyrics being sung, and what plays next.
;;
;; Every control is an item, moved between and used like the
;; browser's: arrows (or whatever moves the cursor) and RET.  Besides,
;;
;;   SPC  play/pause          < >  previous/next     [ ]  seek
;;   s    shuffle             R    repeat            + -  volume
;;   a    the artist's page   o    the album's page  DEL  the library
;;
;; It keeps itself current: the progress bar moves every second, and
;; a new track, a pause or a change to the queue redraws it.

;;; Code:

(require 'aio)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)

(require 'supersonic-custom)
(require 'supersonic-provider)
(require 'supersonic-playback)
(require 'supersonic-art)
(require 'supersonic-browse)

(defvar supersonic-lyrics-line)
(defvar supersonic-lyrics--lyrics)
(defvar supersonic-lyrics--index)
(declare-function supersonic-lyrics--translation-text "supersonic-lyrics")
(declare-function supersonic-lyrics--line-text "supersonic-lyrics")

(defvar supersonic-player-mode-map)

(defconst supersonic-player-buffer-name "*supersonic-player*"
  "Name of the now-playing page's buffer.")

(defvar-local supersonic-player--state nil
  "What the page shows: a plist of `:track-id', `:track', `:paused',
`:position', `:shuffle', `:repeat', `:volume' and `:upcoming', the
tracks after the current one as (QUEUE-INDEX . TRACK).")
(defvar-local supersonic-player--timer nil "Timer moving the progress bar.")
(defvar-local supersonic-player--generation 0 "Bumped per refresh, so stale ones are dropped.")
(defvar-local supersonic-player--width nil "Window width the page was laid out for.")
(defvar-local supersonic-player--shown-track nil "Id of the track the page was last laid out for.")

;;;
;;; Icons
;;;

(defun supersonic-player--icon (name x y size color)
  "Return an SVG group drawing icon NAME, SIZE wide, at X, Y, in COLOR.
Drawn on a 24 unit grid, scaled."
  (let* ((k (/ size 24.0))
         (group (dom-node 'g `((transform . ,(format "translate(%s,%s) scale(%s)" x y k)))))
         (stroke `((fill . "none") (stroke . ,color) (stroke-width . 2) (stroke-linecap . "round")
                   (stroke-linejoin . "round"))))
    (cl-flet ((path (d &optional filled)
                (dom-append-child group (dom-node 'path (cons `(d . ,d) (if filled `((fill . ,color)) stroke)))))
              (bar (bx by bw bh)
                (dom-append-child group (dom-node 'rect `((x . ,bx) (y . ,by) (width . ,bw) (height . ,bh)
                                                          (rx . 1) (fill . ,color))))))
      (pcase name
        ('play (path "M8 5 L19 12 L8 19 Z" t))
        ('pause (bar 6.5 5 4 14) (bar 13.5 5 4 14))
        ('next (path "M5 5 L15 12 L5 19 Z" t) (bar 16 5 3 14))
        ('prev (path "M19 5 L9 12 L19 19 Z" t) (bar 5 5 3 14))
        ('shuffle (path "M3 7 H6 C11 7 13 17 18 17 H21 M3 17 H6 C11 17 13 7 18 7 H21 M18 4 L21 7 L18 10 M18 14 L21 17 L18 20"))
        ('repeat (path "M4 11 V9 A3 3 0 0 1 7 6 H20 M17 3 L20 6 L17 9 M20 13 V15 A3 3 0 0 1 17 18 H4 M7 21 L4 18 L7 15"))
        ('minus (path "M5 12 H19"))
        ('plus (path "M5 12 H19 M12 5 V19"))))
    group))

(defun supersonic-player--control-image (name on)
  "Return the display spec of transport control NAME, glowing if ON.
In focus, a control sits on a soft glow of the accent colour and takes
the accent colour itself; the big play/pause gets a halo instead."
  (let* ((state supersonic-player--state)
         (big (eq name 'toggle))
         (size (if big 76 56))
         (svg (svg-create size size))
         (r (/ size 2.0))
         (accent (supersonic-browse--accent))
         (active (pcase name
                   ('shuffle (plist-get state :shuffle))
                   ('repeat (plist-get state :repeat))))
         (ink (cond (big (supersonic-browse--bg))
                    ((or on active) accent)
                    (t (supersonic-browse--fg)))))
    (when on
      (let ((glow (dom-node 'radialGradient '((id . "glow")))))
        (dom-append-child glow (dom-node 'stop `((offset . ,(if big "0.72" "0.55")) (stop-color . ,accent)
                                                 (stop-opacity . ,(if big "0.75" "0.28")))))
        (dom-append-child glow (dom-node 'stop `((offset . "1") (stop-color . ,accent) (stop-opacity . "0"))))
        (dom-append-child svg glow)
        (dom-append-child svg (dom-node 'circle `((cx . ,r) (cy . ,r) (r . ,r) (fill . "url(#glow)"))))))
    (when big
      (dom-append-child svg (dom-node 'circle `((cx . ,r) (cy . ,r) (r . ,(- r 9)) (fill . ,accent)))))
    (let ((icon (pcase name
                  ('toggle (if (plist-get state :paused) 'play 'pause))
                  (_ name)))
          (glyph (cond (big 30) (on 26) (t 24))))
      (dom-append-child svg (supersonic-player--icon icon (- r (/ glyph 2.0) (if (and big (plist-get state :paused)) -1 0))
                                                     (- r (/ glyph 2.0)) glyph ink)))
    (when active
      (dom-append-child svg (dom-node 'circle `((cx . ,r) (cy . ,(- size 7)) (r . 2.5) (fill . ,accent)))))
    (when (and (eq name 'repeat) (eq (plist-get state :repeat) 'track))
      (dom-append-child svg (dom-node 'text `((x . ,r) (y . ,(+ r 4)) (text-anchor . "middle")
                                              (font-family . ,(supersonic-browse--font-family))
                                              (font-size . 10) (font-weight . "bold") (fill . ,accent))
                                      "1")))
    (svg-image svg :ascent 'center :scale 1)))

;;;
;;; Pieces of the page
;;;

(defun supersonic-player--center (pixels)
  "Return space bringing something PIXELS wide to the middle of the window."
  (supersonic-browse--align (max 0 (/ (- supersonic-player--width pixels) 2))))

(defun supersonic-player--centered-text (text)
  "Insert TEXT centred, cut to the page's width, and a newline."
  (let* ((text (supersonic-browse--fit text (supersonic-browse--available-width))))
    (insert (supersonic-player--center (supersonic--string-pixel-width text)) text "\n")))

(defun supersonic-player--art-size ()
  "Return how big the cover is drawn, for the window showing the page."
  (let ((window (supersonic-browse--window)))
    (max 160 (min supersonic-player-art-size
                  (round (* 0.62 (supersonic-browse--available-width)))
                  (round (* 0.42 (window-body-height window t)))))))

(defun supersonic-player--art (track size)
  "Insert TRACK's cover at SIZE, on a card with a soft shadow."
  (let* ((pad 28)
         (svg (svg-create (+ size (* 2 pad)) (+ size (* 2 pad))))
         (file (supersonic-browse--art-file track supersonic-now-playing-art-size))
         (filter (dom-node 'filter '((id . "shadow") (x . "-30%") (y . "-30%") (width . "160%") (height . "160%")))))
    (dom-append-child filter (dom-node 'feDropShadow '((dx . 0) (dy . 10) (stdDeviation . 14)
                                                       (flood-color . "#000000") (flood-opacity . 0.55))))
    (dom-append-child svg filter)
    (dom-append-child svg (dom-node 'rect `((x . ,pad) (y . ,(- pad 6)) (width . ,size) (height . ,size)
                                            (rx . ,(* size 0.04)) (fill . ,(supersonic-browse--bg))
                                            (filter . "url(#shadow)"))))
    (supersonic-browse--art-svg svg (append (list :name (plist-get track :album)) track) size 'square file pad (- pad 6))
    (insert (supersonic-player--center (+ size (* 2 pad)))
            (apply #'propertize "*"
                   'display (apply #'svg-image svg :ascent 100 :scale 1 (and file (list :base-uri file)))
                   'supersonic-player-art t
                   (supersonic-browse--item-properties
                    '(button art) (list :action #'supersonic-player-goto-album)
                    'help-echo "Open the album"))
            "\n")))

(defun supersonic-player--progress-image (width)
  "Return the display spec of the progress bar, WIDTH wide."
  (let* ((state supersonic-player--state)
         (duration (plist-get (plist-get state :track) :duration))
         (position (or (plist-get state :position) 0))
         (fraction (if (and duration (> duration 0)) (min 1 (/ position (float duration))) 0))
         (pad 10)
         (bar (- width (* 2 pad)))
         (svg (svg-create width 40))
         (accent (supersonic-browse--accent))
         (x (+ pad (* fraction bar)))
         (family (supersonic-browse--font-family)))
    (dom-append-child svg (dom-node 'rect `((x . ,pad) (y . 9) (width . ,bar) (height . 5) (rx . 2.5)
                                            (fill . ,(supersonic-browse--fg)) (fill-opacity . "0.16"))))
    (dom-append-child svg (dom-node 'rect `((x . ,pad) (y . 9) (width . ,(max 0 (- x pad))) (height . 5) (rx . 2.5)
                                            (fill . ,accent))))
    (dom-append-child svg (dom-node 'circle `((cx . ,x) (cy . 11.5) (r . 7) (fill . ,(supersonic-browse--fg)))))
    (dolist (label `((,pad "start" ,(supersonic-browse--duration position))
                     (,(- width pad) "end" ,(if duration (concat "-" (supersonic-browse--duration (max 0 (- duration position)))) ""))))
      (dom-append-child svg (dom-node 'text `((x . ,(nth 0 label)) (y . 36) (text-anchor . ,(nth 1 label))
                                              (font-family . ,family) (font-size . 13)
                                              (fill . ,(supersonic-browse--subtle)))
                                      (nth 2 label))))
    (svg-image svg :ascent 100 :scale 1
               :map `(((rect . ((0 . 0) . (,width . 24))) seek (pointer hand help-echo "Click to seek"))))))

(defvar supersonic-player-progress-map
  (let ((map (make-sparse-keymap)))
    (define-key map [seek mouse-1] #'supersonic-player-seek-click)
    (define-key map [mouse-1] #'supersonic-player-seek-click)
    map)
  "Keymap of the progress bar.")

(defun supersonic-player--progress-width ()
  "Return the progress bar's width."
  (min 720 (supersonic-browse--available-width)))

(defun supersonic-player--insert-progress ()
  "Insert the progress bar, centred."
  (let ((width (supersonic-player--progress-width)))
    (insert (supersonic-player--center width)
            (propertize "*" 'display (supersonic-player--progress-image width)
                        'supersonic-player-progress width
                        'keymap supersonic-player-progress-map 'pointer 'hand)
            "\n")))

(defun supersonic-player--insert-controls (names)
  "Insert the controls NAMES in a centred row, each an item of its own."
  (let* ((gap 18)
         (widths (mapcar (lambda (name) (if (eq name 'toggle) 76 56)) names))
         (total (+ (apply #'+ widths) (* gap (1- (length names))))))
    (supersonic-browse--new-region)
    (insert (supersonic-player--center total))
    (cl-loop
     for name in names for first = t then nil do
     (unless first (insert (supersonic-browse--space gap)))
     (let ((name name))
       (insert (apply #'propertize "*"
                      'display (supersonic-player--control-image name nil)
                      'supersonic-browse-image-function (lambda (on) (supersonic-player--control-image name on))
                      (supersonic-browse--item-properties
                       (list 'button name) (list :action (supersonic-player--control-action name))
                       'supersonic-browse-focus t 'supersonic-browse-region supersonic-browse--region
                       'help-echo (capitalize (symbol-name name)))))))
    (insert "\n")))

(defun supersonic-player--control-action (name)
  "Return the command control NAME runs."
  (pcase name
    ('toggle #'supersonic-toggle-playing)
    ('prev #'supersonic-prev-track)
    ('next #'supersonic-skip-track)
    ('shuffle #'supersonic-player-shuffle)
    ('repeat #'supersonic-player-repeat)
    ('minus #'supersonic-player-volume-down)
    ('plus #'supersonic-player-volume-up)))

(defun supersonic-player--volume-image ()
  "Return the display spec of the volume level."
  (let* ((volume (or (plist-get supersonic-player--state :volume) 0))
         (width 150)
         (svg (svg-create (+ width 56) 56)))
    (dom-append-child svg (dom-node 'rect `((x . 0) (y . 26) (width . ,width) (height . 4) (rx . 2)
                                            (fill . ,(supersonic-browse--fg)) (fill-opacity . "0.16"))))
    (dom-append-child svg (dom-node 'rect `((x . 0) (y . 26) (width . ,(* width (/ (min 100 volume) 100.0))) (height . 4)
                                            (rx . 2) (fill . ,(supersonic-browse--subtle)))))
    (dom-append-child svg (dom-node 'text `((x . ,(+ width 10)) (y . 33) (font-family . ,(supersonic-browse--font-family))
                                            (font-size . 13) (fill . ,(supersonic-browse--subtle)))
                                    (format "%d%%" (round volume))))
    (svg-image svg :ascent 'center :scale 1)))

(defun supersonic-player--insert-volume ()
  "Insert the volume controls, centred."
  (let ((gap 10))
    (supersonic-browse--new-region)
    (insert (supersonic-player--center (+ 56 gap 206 gap 56)))
    (dolist (part '(minus level plus))
      (if (eq part 'level)
          (insert (supersonic-browse--space gap)
                  (propertize "*" 'display (supersonic-player--volume-image) 'supersonic-player-volume t)
                  (supersonic-browse--space gap))
        (let ((name part))
          (insert (apply #'propertize "*"
                         'display (supersonic-player--control-image name nil)
                         'supersonic-browse-image-function (lambda (on) (supersonic-player--control-image name on))
                         (supersonic-browse--item-properties
                          (list 'button name) (list :action (supersonic-player--control-action name))
                          'supersonic-browse-focus t 'supersonic-browse-region supersonic-browse--region
                          'help-echo (if (eq name 'minus) "Quieter" "Louder")))))))
    (insert "\n")))

(defun supersonic-player--lyrics-lines ()
  "Return the lyrics lines to show: the line sung, its translation, the next."
  (when (and (bound-and-true-p supersonic-lyrics-line) (fboundp 'supersonic-lyrics--line-text)
             (plist-get (bound-and-true-p supersonic-lyrics--lyrics) :synced))
    (list supersonic-lyrics-line
          (and (fboundp 'supersonic-lyrics--translation-text)
               (supersonic-lyrics--translation-text supersonic-lyrics--index))
          (supersonic-lyrics--line-text (1+ (or supersonic-lyrics--index -1))))))

(defun supersonic-player--insert-lyrics ()
  "Insert the lyrics lines, three fixed lines so the page does not jump."
  (let ((lines (supersonic-player--lyrics-lines))
        (start (point)))
    (pcase-let ((`(,current ,translation ,next) lines))
      (dolist (line (list (cons current 'supersonic-player-lyric)
                          (cons translation 'supersonic-lyrics-translation)
                          (cons next 'supersonic-player-lyric-next)))
        (if (and (car line) (not (string-empty-p (car line))))
            (supersonic-player--centered-text (propertize (car line) 'face (cdr line)))
          (insert (propertize " " 'face (cdr line)) "\n"))))
    (put-text-property start (point) 'supersonic-player-lyrics t)))

(defun supersonic-player--insert-upcoming ()
  "Insert what plays next, as rows to jump to."
  (let* ((upcoming (plist-get supersonic-player--state :upcoming))
         (width (min 780 (supersonic-browse--available-width)))
         (left (/ (- supersonic-player--width width) 2))
         (right (+ left width))
         (thumb 44)
         (strut (supersonic-browse--row-strut)))
    (when upcoming
      (insert "\n" (supersonic-browse--align left)
              (propertize "Up next" 'face 'supersonic-browse-heading)
              (propertize "\n" 'line-height 1.5))
      (supersonic-browse--new-region)
      (dolist (entry upcoming)
        (let* ((index (car entry))
               (track (cdr entry))
               (duration (supersonic-browse--duration (plist-get track :duration)))
               (duration-x (- right (supersonic--string-pixel-width duration)))
               (title-x (+ left thumb 16))
               (text (concat (propertize (or (plist-get track :title) "?") 'face 'supersonic-browse-track)
                             (and (plist-get track :artist)
                                  (propertize (concat "   " (plist-get track :artist)) 'face 'shadow))))
               (svg (svg-create thumb thumb))
               (file (supersonic-browse--art-file track (* 2 supersonic-browse-tile-size))))
          (supersonic-browse--art-svg svg (append (list :name (plist-get track :album)) track) thumb 'square file)
          (insert (apply #'propertize
                         (concat strut
                                 (supersonic-browse--align left)
                                 (propertize "*" 'display (apply #'svg-image svg :ascent 'center :scale 1
                                                                 (and file (list :base-uri file))))
                                 (supersonic-browse--align title-x)
                                 (supersonic-browse--fit text (- duration-x title-x 24))
                                 (supersonic-browse--align duration-x)
                                 (propertize duration 'face 'supersonic-browse-track-number)
                                 "\n")
                         (supersonic-browse--item-properties
                          (list 'button (list 'upcoming index)) (append (list :action (lambda () (supersonic-playback-play-index index)))
                                                                        track)
                          'supersonic-browse-row t 'supersonic-browse-region supersonic-browse--region
                          'supersonic-player-thumb (cons track thumb))))
          (save-excursion
            (forward-line -1)
            (put-text-property (point) (1+ (point)) 'supersonic-browse-focus t)))))))

;;;
;;; The page
;;;

(defun supersonic-player--hints ()
  "Return the player's key hints, read off its keys."
  (mapconcat
   (lambda (pair)
     (let ((key (where-is-internal (car pair) supersonic-player-mode-map t)))
       (if key (concat (key-description key) " " (cdr pair)) "")))
   '((supersonic-toggle-playing . "play/pause") (supersonic-skip-track . "next")
     (supersonic-prev-track . "prev") (supersonic-seek-forward . "seek")
     (supersonic-player-goto-artist . "artist")
     (supersonic-player-goto-album . "album") (supersonic-player-library . "library"))
   "   "))

(defun supersonic-player--render ()
  "Lay the page out anew, in the window showing it."
  (let ((window (get-buffer-window (current-buffer) t)))
    (if (and window (not (eq window (selected-window))))
        (with-selected-window window
          (supersonic-player--render-here))
      (supersonic-player--render-here))))

(defun supersonic-player--render-here ()
  "Lay the page out anew, in the selected window."
  (let* ((inhibit-read-only t)
         (start (window-start (supersonic-browse--window)))
         (selected (or supersonic-browse--selected '(button toggle)))
         (state supersonic-player--state)
         (track (plist-get state :track))
         (graphic (supersonic-browse--graphic-p)))
    (erase-buffer)
    (setq supersonic-player--width (window-body-width (supersonic-browse--window) t)
          supersonic-browse--region 0
          supersonic-browse--region-fresh nil
          supersonic-browse--selected nil)
    (dolist (overlay (list supersonic-browse--title-overlay supersonic-browse--bar-overlay
                           supersonic-browse--focus-overlay))
      (when overlay (delete-overlay overlay)))
    (insert (propertize "\n" 'line-height 1.4)
            (supersonic-browse--space supersonic-browse-margin)
            (propertize (if (plist-get state :paused) "Paused" "Now playing") 'face 'supersonic-browse-trail-current))
    (let ((hints (propertize (supersonic-player--hints) 'face 'supersonic-browse-hint)))
      (when (> (- (supersonic-browse--available-width) (supersonic--string-pixel-width hints)) 300)
        (insert (supersonic-browse--align (- (+ supersonic-browse-margin (supersonic-browse--available-width))
                                             (supersonic--string-pixel-width hints)))
                hints)))
    (insert (propertize "\n" 'line-height 1.6))
    (if (not track)
        (progn
          (insert "\n")
          (supersonic-player--centered-text (propertize "Nothing playing" 'face 'supersonic-browse-greeting))
          (insert "\n")
          (supersonic-browse--new-region)
          (let* ((buttons `(("▶ Surprise me" . supersonic-browse-surprise) ("Library" . supersonic-browse)))
                 (labels (mapcar (lambda (b) (propertize (concat " " (car b) " ") 'face 'supersonic-browse-button)) buttons)))
            (insert (supersonic-player--center (+ (apply #'+ (mapcar #'supersonic--string-pixel-width labels)) 40)))
            (cl-loop for button in buttons for label in labels do
                     (insert (apply #'propertize label
                                    (supersonic-browse--item-properties
                                     (list 'button (cdr button)) (list :action (cdr button))
                                     'supersonic-browse-focus t 'supersonic-browse-region supersonic-browse--region))
                             "  "))
            (insert "\n")))
      (when graphic
        (supersonic-player--art track (supersonic-player--art-size)))
      (insert (propertize "\n" 'line-height 0.6))
      (supersonic-player--centered-text (propertize (or (plist-get track :title) "?") 'face 'supersonic-player-title))
      ;; Artist and album: links into the browser.
      (supersonic-browse--new-region)
      (let* ((artist (and (plist-get track :artist)
                          (apply #'propertize (plist-get track :artist) 'face 'supersonic-player-artist
                                 (supersonic-browse--item-properties
                                  '(button artist) (list :action #'supersonic-player-goto-artist)
                                  'supersonic-browse-focus t 'supersonic-browse-region supersonic-browse--region))))
             (album (and (plist-get track :album)
                         (apply #'propertize (plist-get track :album) 'face 'supersonic-player-album
                                (supersonic-browse--item-properties
                                 '(button album) (list :action #'supersonic-player-goto-album)
                                 'supersonic-browse-focus t 'supersonic-browse-region supersonic-browse--region))))
             (line (string-join (delq nil (list artist album)) (propertize "  ·  " 'face 'shadow))))
        (supersonic-player--centered-text line))
      (insert (propertize "\n" 'line-height 0.8))
      (when graphic
        (supersonic-player--insert-progress))
      (insert (propertize "\n" 'line-height 0.4))
      (supersonic-player--insert-controls '(shuffle prev toggle next repeat))
      (supersonic-player--insert-volume)
      (insert (propertize "\n" 'line-height 0.8))
      (supersonic-player--insert-lyrics)
      (supersonic-player--insert-upcoming))
    (insert "\n")
    (supersonic-browse--index-stops)
    (goto-char (let ((i (or (supersonic-browse--key-index selected) (supersonic-browse--key-index '(button toggle)) 0)))
                 (if (< i (supersonic-browse--stop-count)) (aref supersonic-browse--stops i) (point-min))))
    ;; Back to the top for a new track only: a pause or a seek must not
    ;; scroll away from where one was looking.
    (let ((window (get-buffer-window (current-buffer) t)))
      (when window
        (if (equal (plist-get state :track-id) supersonic-player--shown-track)
            (set-window-start window (min start (point-max)) t)
          (set-window-start window (point-min) t))))
    (setq supersonic-player--shown-track (plist-get state :track-id))
    (supersonic-browse--update-focus)
    (supersonic-player--fetch-art)
    (supersonic-player--update-timer)))

(defun supersonic-player--fetch-art ()
  "Fetch the cover and the thumbnails the page lacks, then draw them."
  (let* ((buff (current-buffer))
         (generation supersonic-player--generation)
         (wanted (delq nil (cons (let ((track (plist-get supersonic-player--state :track)))
                                   (and (plist-get track :art)
                                        (not (supersonic-browse--art-file track supersonic-now-playing-art-size))
                                        (cons (plist-get track :art) supersonic-now-playing-art-size)))
                                 (mapcar (lambda (entry)
                                           (let ((track (cdr entry)))
                                             (and (plist-get track :art)
                                                  (not (supersonic-browse--art-file track (* 2 supersonic-browse-tile-size)))
                                                  (cons (plist-get track :art) (* 2 supersonic-browse-tile-size)))))
                                         (plist-get supersonic-player--state :upcoming))))))
    (when (and wanted (supersonic-art-available-p))
      (unless supersonic-browse--sem
        (setq supersonic-browse--sem (aio-sem supersonic-art-fetch-concurrency)))
      (let ((promises (mapcar (lambda (pair) (supersonic--fetch-art-throttled supersonic-browse--sem (car pair) (cdr pair)))
                              wanted)))
        (aio-listen (car (last promises))
                    (lambda (_)
                      (when (buffer-live-p buff)
                        (with-current-buffer buff
                          (when (= generation supersonic-player--generation)
                            (supersonic-player--render))))))))))

;;;
;;; Keeping current
;;;

(aio-defun
 supersonic-player--refresh (buff)
 "Read everything the page shows from the backend, then lay BUFF out."
 (let ((generation (with-current-buffer buff (cl-incf supersonic-player--generation))))
   (condition-case err
       (let* ((track-id (aio-await (supersonic-playback-status 'track-id)))
              (state (list :track-id track-id
                           :paused (aio-await (supersonic-playback-status 'paused))
                           :position (aio-await (supersonic-playback-status 'position))
                           :shuffle (aio-await (supersonic-playback-status 'shuffle))
                           :repeat (aio-await (supersonic-playback-status 'repeat))
                           :volume (aio-await (supersonic-playback-status 'volume))))
              (old (buffer-local-value 'supersonic-player--state buff))
              (track (and track-id
                          (if (equal track-id (plist-get old :track-id))
                              (plist-get old :track)
                            (condition-case nil (aio-await (supersonic-provider-track track-id)) (error nil)))))
              (queue (aio-await (supersonic-playback-queue)))
              (current (seq-position queue t (lambda (entry _) (plist-get entry :current))))
              (upcoming nil))
         (when current
           (cl-loop for entry in (seq-take (nthcdr (1+ current) queue) supersonic-player-upcoming-count)
                    for index from (1+ current) do
                    (let ((info (or (plist-get entry :track)
                                    (condition-case nil (aio-await (supersonic-provider-track (plist-get entry :track-id)))
                                      (error nil)))))
                      (when info (push (cons index info) upcoming)))))
         (when (and (buffer-live-p buff)
                    (= generation (buffer-local-value 'supersonic-player--generation buff)))
           (with-current-buffer buff
             (setq supersonic-player--state (append (list :track track :upcoming (nreverse upcoming)) state))
             (supersonic-player--render))))
     (error (supersonic--log "now playing refresh failed: %s" (error-message-string err))))))

(defun supersonic-player--refresh-all ()
  "Refresh the now-playing page, if it is open."
  (let ((buff (get-buffer supersonic-player-buffer-name)))
    (when (buffer-live-p buff)
      (ignore (supersonic-player--refresh buff)))))

(defun supersonic-player--tick (buff)
  "Move BUFF's progress bar to where playback is."
  (if (not (and (buffer-live-p buff) (get-buffer-window buff t)))
      (when (buffer-live-p buff)
        (with-current-buffer buff (supersonic-player--update-timer)))
    (aio-listen (aio-catch (supersonic-playback-status 'position))
                (lambda (value)
                  (let ((outcome (funcall value)))
                    (when (and (eq (car outcome) :success) (numberp (cdr outcome)) (buffer-live-p buff))
                      (with-current-buffer buff
                        (setq supersonic-player--state (plist-put supersonic-player--state :position (cdr outcome)))
                        (supersonic-player--redraw-progress))))))))

(defun supersonic-player--redraw-progress ()
  "Redraw the progress bar in place, in the window showing the page."
  (let ((pos (text-property-not-all (point-min) (point-max) 'supersonic-player-progress nil))
        (window (get-buffer-window (current-buffer) t))
        (inhibit-read-only t))
    (when pos
      (cl-flet ((redraw ()
                  (put-text-property pos (1+ pos) 'display
                                     (supersonic-player--progress-image
                                      (get-text-property pos 'supersonic-player-progress)))))
        (if (and window (not (eq window (selected-window))))
            (with-selected-window window (redraw))
          (redraw))))))

(defun supersonic-player--update-timer ()
  "Run the progress timer exactly while something plays and the page is shown."
  (let ((wanted (and (plist-get supersonic-player--state :track)
                     (not (plist-get supersonic-player--state :paused))
                     (get-buffer-window (current-buffer) t))))
    (cond
     ((and wanted (not supersonic-player--timer))
      (setq supersonic-player--timer (run-at-time 1 1 #'supersonic-player--tick (current-buffer))))
     ((and (not wanted) supersonic-player--timer)
      (cancel-timer supersonic-player--timer)
      (setq supersonic-player--timer nil)))))

(defun supersonic-player--lyrics-changed ()
  "Redraw the lyrics lines in place, keeping the rest of the page still."
  (let ((buff (get-buffer supersonic-player-buffer-name)))
    (when (buffer-live-p buff)
      (with-current-buffer buff
        (let ((start (text-property-any (point-min) (point-max) 'supersonic-player-lyrics t)))
          (when start
            (let ((end (or (text-property-not-all start (point-max) 'supersonic-player-lyrics t) (point-max)))
                  (inhibit-read-only t)
                  (window (get-buffer-window buff t)))
              (let ((top (and window (window-start window))))
                (save-excursion
                  (delete-region start end)
                  (goto-char start)
                  (if window
                      (with-selected-window window
                        (with-current-buffer buff (supersonic-player--insert-lyrics)))
                    (supersonic-player--insert-lyrics)))
                (when window (set-window-start window top t)))
              (supersonic-browse--index-stops))))))))

(defun supersonic-player--window-changed (window)
  "Lay the page out again if WINDOW, showing it, changed width."
  (with-current-buffer (window-buffer window)
    (unless (eql supersonic-player--width (window-body-width window t))
      (supersonic-player--render))))

(defconst supersonic-player--hooks
  '((supersonic-playback-track-change-hook . supersonic-player--refresh-all)
    (supersonic-playback-state-change-hook . supersonic-player--refresh-all)
    (supersonic-playback-queue-change-hook . supersonic-player--refresh-all)
    (supersonic-playback-position-change-hook . supersonic-player--refresh-all)
    (supersonic-lyrics-line-change-hook . supersonic-player--lyrics-changed))
  "Hooks the page keeps current by.")

;;;
;;; Commands
;;;

(defun supersonic-player-seek-click (event)
  "Seek to where the progress bar was clicked with EVENT."
  (interactive "e")
  (let* ((posn (event-start event))
         (x (car (posn-object-x-y posn)))
         (width (car (posn-object-width-height posn)))
         (pad 10))
    (when (and x width (> width (* 2 pad)))
      (supersonic-playback-seek-fraction (min 1.0 (max 0.0 (/ (- x pad) (float (- width (* 2 pad))))))))))

(defun supersonic-player-shuffle ()
  "Toggle shuffle, and show it."
  (interactive)
  (supersonic-toggle-shuffle)
  (supersonic-player--refresh-all))

(defun supersonic-player-repeat ()
  "Cycle repeat, and show it."
  (interactive)
  (supersonic-cycle-repeat)
  (supersonic-player--refresh-all))

(defun supersonic-player-volume-up ()
  "Louder, and show it."
  (interactive)
  (supersonic-volume-up)
  (supersonic-player--refresh-all))

(defun supersonic-player-volume-down ()
  "Quieter, and show it."
  (interactive)
  (supersonic-volume-down)
  (supersonic-player--refresh-all))

(defun supersonic-player--track ()
  "Return the track the page shows, or complain."
  (or (plist-get supersonic-player--state :track) (user-error "Nothing playing")))

(defun supersonic-player-goto-artist ()
  "Show the playing track's artist in the library browser."
  (interactive)
  (let ((track (supersonic-player--track)))
    (unless (plist-get track :artist-id)
      (user-error "The server does not say who the artist is"))
    (supersonic-browse-visit (list :kind 'artist :id (plist-get track :artist-id)
                                   :item (list :id (plist-get track :artist-id) :name (plist-get track :artist))))))

(defun supersonic-player-goto-album ()
  "Show the playing track's album in the library browser."
  (interactive)
  (let ((track (supersonic-player--track)))
    (unless (plist-get track :album-id)
      (user-error "The server does not say which album this is"))
    (supersonic-browse-visit (list :kind 'album :id (plist-get track :album-id)
                                   :item (list :id (plist-get track :album-id) :name (plist-get track :album)
                                               :artist (plist-get track :artist) :year (plist-get track :year)
                                               :art (plist-get track :art))))))

(defun supersonic-player-library ()
  "Go to the library browser."
  (interactive)
  (supersonic-browse))

(defvar supersonic-player-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Transport on the same keys as everywhere else in supersonic: n p
    ;; next/previous track, f b seek.  Moving between the controls is
    ;; what the arrows (and whatever moves the cursor) are for.
    (define-key map (kbd "n") #'supersonic-skip-track)
    (define-key map (kbd "p") #'supersonic-prev-track)
    (define-key map (kbd "f") #'supersonic-seek-forward)
    (define-key map (kbd "b") #'supersonic-seek-back)
    (define-key map (kbd "s") #'supersonic-player-shuffle)
    (define-key map (kbd "z") #'supersonic-player-shuffle)
    (define-key map (kbd "R") #'supersonic-player-repeat)
    (define-key map (kbd "+") #'supersonic-player-volume-up)
    (define-key map (kbd "=") #'supersonic-player-volume-up)
    (define-key map (kbd "-") #'supersonic-player-volume-down)
    (define-key map (kbd "a") #'supersonic-player-goto-artist)
    (define-key map (kbd "o") #'supersonic-player-goto-album)
    (define-key map (kbd "DEL") #'supersonic-player-library)
    (define-key map (kbd "l") #'supersonic-player-library)
    (define-key map (kbd "P") #'supersonic-toggle-playing)
    (define-key map (kbd "g") #'supersonic-player-refresh)
    map)
  "Keymap for `supersonic-player-mode'; its parent is the browser's.")

(defun supersonic-player-refresh ()
  "Read the page anew from the backend."
  (interactive)
  (ignore (supersonic-player--refresh (current-buffer))))

(define-derived-mode supersonic-player-mode supersonic-browse-mode "Supersonic Player"
  "Major mode of the now-playing page, see `supersonic-player'.
\\{supersonic-player-mode-map}"
  (remove-hook 'window-size-change-functions #'supersonic-browse--window-changed t)
  (add-hook 'window-size-change-functions #'supersonic-player--window-changed nil t)
  (add-hook 'kill-buffer-hook (lambda () (when supersonic-player--timer (cancel-timer supersonic-player--timer))) nil t)
  (dolist (hook supersonic-player--hooks)
    (add-hook (car hook) (cdr hook))))

;;;###autoload
(defun supersonic-player ()
  "Show what is playing, with the controls and what plays next."
  (interactive)
  (let ((buff (get-buffer-create supersonic-player-buffer-name)))
    (with-current-buffer buff
      (unless (derived-mode-p 'supersonic-player-mode)
        (supersonic-player-mode)))
    (pop-to-buffer-same-window buff)
    (with-current-buffer buff
      (unless supersonic-player--state
        (supersonic-player--render))
      (ignore (supersonic-player--refresh buff)))))

(defun supersonic-player-unload-function ()
  "Remove the page's hooks."
  (dolist (hook supersonic-player--hooks)
    (remove-hook (car hook) (cdr hook)))
  nil)

(provide 'supersonic-player)

;;; supersonic-player.el ends here
