;;; supersonic-browse.el --- A visual library browser for supersonic.el -*- lexical-binding: t; -*-

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

;; `supersonic-browse' opens *supersonic*, one buffer to find music in
;; and decide what to play, browsed like a web page: every page has
;; the same handful of keys, `l' and `r' go back and forward, and the
;; trail at the top says where you are.
;;
;; - Home: rows of albums -- jump back in, on repeat, fresh, rediscover
;;   -- and a button that plays a random album.
;; - Artists and album lists: grids of cover art that fill the window
;;   and reflow with it.  Artists are round, albums square; anything
;;   without art gets a tile of its own colour with its initials.
;; - An artist or album: a header with its art, what it is and buttons
;;   to play, shuffle or queue it, above its albums or its tracks.
;; - Search: artists, albums and songs matching a query.
;;
;; Keys, the same on every page:
;;
;;   arrows, n p  move between items      RET  open / play from here
;;   TAB S-TAB    next / previous section  P    play
;;   l r          back / forward           s    shuffle
;;   h            home                     a    add to the queue
;;   /            search                   g    reload
;;
;; plus every supersonic buffer's playback keys (SPC, < >, [ ], ...).

;;; Code:

(require 'aio)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'color)
(require 'url-util)

(require 'supersonic-custom)
(require 'supersonic-provider)
(require 'supersonic-playback)
(require 'supersonic-art)
(require 'supersonic)

;;;
;;; State
;;;

(defconst supersonic-browse-buffer-name "*supersonic*"
  "Name of the library browser's buffer.")

(defvar-local supersonic-browse--page nil
  "The page shown, a plist with at least `:kind'.")
(defvar-local supersonic-browse--back nil "Pages visited before, most recent first.")
(defvar-local supersonic-browse--forward nil "Pages gone back from, most recent first.")
(defvar-local supersonic-browse--content nil "What the page shows, as its loader resolved it.")
(defvar-local supersonic-browse--selected nil "Key of the item in focus.")
(defvar-local supersonic-browse--width nil "Window width in pixels the page was laid out for.")
(defvar-local supersonic-browse--generation 0 "Bumped per page load, so stale loads are dropped.")
(defvar-local supersonic-browse--playing nil "Id of the track playing, for its marker.")
(defvar-local supersonic-browse--images nil "Cache of tile images, by key, shape and state.")
(defvar-local supersonic-browse--focus-overlay nil "Overlay on the track row in focus.")
(defvar-local supersonic-browse--sem nil "Semaphore bounding the art fetches.")
(defvar-local supersonic-browse--stops []
  "Positions items take focus at, in buffer order.")
(defvar-local supersonic-browse--stop-index nil
  "Hash table of item key to its index in `supersonic-browse--stops'.")
(defvar-local supersonic-browse--region 0
  "Region the items being inserted belong to.")
(defvar-local supersonic-browse--region-fresh nil
  "Non-nil right after a heading started a region.")
(defvar-local supersonic-browse--title-overlay nil
  "Overlay on the name under the tile in focus.")
(defvar-local supersonic-browse--bar-overlay nil
  "Overlay drawing the accent bar of the track row in focus.")
(defvar supersonic-browse-mode-map)

;;;
;;; Colours and measures
;;;

(defun supersonic-browse--color (face attribute &optional fallback)
  "Return FACE's ATTRIBUTE colour as an SVG colour, else FALLBACK."
  (let ((color (if (eq attribute :background)
                   (face-background face nil t)
                 (face-foreground face nil t))))
    (if (and (stringp color) (not (string-prefix-p "unspecified" color)))
        color
      fallback)))

(defun supersonic-browse--fg () "Foreground colour." (supersonic-browse--color 'default :foreground "#dddddd"))
(defun supersonic-browse--bg () "Background colour." (supersonic-browse--color 'default :background "#1e1e1e"))
(defun supersonic-browse--accent () "Accent colour." (supersonic-browse--color 'supersonic-browse-accent :foreground "#7aa2f7"))
(defun supersonic-browse--subtle () "Subtle colour." (supersonic-browse--color 'shadow :foreground "#888888"))

(defun supersonic-browse--font-family ()
  "Return the default font's family, for SVG text."
  (let ((family (face-attribute 'default :family)))
    (if (stringp family) family "sans-serif")))

(defun supersonic-browse--window ()
  "Return the window showing the browser, on any frame, else the selected one."
  (or (get-buffer-window (current-buffer)) (get-buffer-window (current-buffer) t) (selected-window)))

(defun supersonic-browse--available-width ()
  "Return the pixels a page can use between its margins."
  (- (window-body-width (supersonic-browse--window) t) (* 2 supersonic-browse-margin)))

(defun supersonic-browse--graphic-p ()
  "Return non-nil if tiles can be drawn as images where the browser is shown.
Asks the frame showing it, not the selected one: a redraw from a timer
may run while a daemon's terminal frame is selected."
  (and (display-graphic-p (window-frame (supersonic-browse--window))) (image-type-available-p 'svg)))

(defun supersonic-browse--hue (string)
  "Return a hue between 0 and 1 picked by STRING, the same every time."
  (/ (mod (string-to-number (substring (secure-hash 'md5 (or string "")) 0 6) 16) 360) 360.0))

(defun supersonic-browse--hsl (h s l)
  "Return the hex colour of hue H, saturation S and lightness L, each 0 to 1."
  (apply #'color-rgb-to-hex (append (color-hsl-to-rgb h s l) '(2))))

(defun supersonic-browse--initials (name)
  "Return up to two initials of NAME."
  (let ((words (seq-filter (lambda (w) (string-match-p "[[:alnum:]]" w))
                           (split-string (or name "?") "[[:space:]]+" t))))
    (upcase (apply #'string (mapcar (lambda (w) (aref (replace-regexp-in-string "[^[:alnum:]]" "" w) 0))
                                    (seq-take (or words '("?")) 2))))))

(defun supersonic-browse--duration (seconds)
  "Return SECONDS as M:SS, or H:MM:SS from an hour on."
  (if (not (numberp seconds))
      ""
    (let ((s (round seconds)))
      (if (>= s 3600)
          (format "%d:%02d:%02d" (/ s 3600) (/ (mod s 3600) 60) (mod s 60))
        (format "%d:%02d" (/ s 60) (mod s 60))))))

(defun supersonic-browse--total (seconds)
  "Return SECONDS as a rough total, such as \"41 min\" or \"1 h 12 min\"."
  (let ((minutes (round (/ (or seconds 0) 60.0))))
    (if (>= minutes 60)
        (format "%d h %d min" (/ minutes 60) (mod minutes 60))
      (format "%d min" minutes))))

(defun supersonic-browse--plural (n word)
  "Return N WORDs, WORD made plural unless N is 1."
  (format "%d %s%s" n word (if (eql n 1) "" "s")))

;;;
;;; Tiles
;;;

(defun supersonic-browse--art-file (item size)
  "Return ITEM's cover art cached at SIZE, or nil if it is not cached."
  (let ((art (plist-get item :art)))
    (and art supersonic-enable-art
         (let ((file (supersonic-art-cache-file art size)))
           (and (file-exists-p file) file)))))

(defun supersonic-browse--shape-node (shape size inset &rest attributes)
  "Return an SVG node of SHAPE, `circle' or `square', SIZE wide.
INSET from the edge, with ATTRIBUTES added."
  (let ((r (/ size 2.0)))
    (if (eq shape 'circle)
        (dom-node 'circle `((cx . ,r) (cy . ,r) (r . ,(- r inset)) ,@attributes))
      (dom-node 'rect `((x . ,inset) (y . ,inset) (width . ,(- size (* 2 inset))) (height . ,(- size (* 2 inset)))
                        (rx . ,(max 0 (- (* size 0.07) inset))) ,@attributes)))))

(defun supersonic-browse--art-svg (svg item size shape file &optional x y)
  "Draw ITEM's art FILE, or its placeholder, into SVG as a SHAPE SIZE wide.
At X and Y, 0 by default.  FILE is resolved against the image's
`:base-uri', so it must be that file."
  (let* ((x (or x 0)) (y (or y 0))
         (id (format "clip%d%d" x y))
         (clip (dom-node 'clipPath `((id . ,id)))))
    (dom-append-child clip (let ((node (supersonic-browse--shape-node shape size 0)))
                             (dom-set-attribute node 'transform (format "translate(%s,%s)" x y))
                             node))
    (dom-append-child svg clip)
    (if file
        (dom-append-child
         svg (dom-node 'image `((xlink:href . ,(url-hexify-string (file-name-nondirectory file)))
                                (x . ,x) (y . ,y) (width . ,size) (height . ,size)
                                (preserveAspectRatio . "xMidYMid slice")
                                (clip-path . ,(format "url(#%s)" id)))))
      (let* ((hue (supersonic-browse--hue (or (plist-get item :name) (plist-get item :title))))
             (gid (format "grad%d%d" x y))
             (gradient (dom-node 'linearGradient `((id . ,gid) (x1 . "0") (y1 . "0") (x2 . "1") (y2 . "1")))))
        (dom-append-child gradient (dom-node 'stop `((offset . "0") (stop-color . ,(supersonic-browse--hsl hue 0.55 0.42)))))
        (dom-append-child gradient (dom-node 'stop `((offset . "1") (stop-color . ,(supersonic-browse--hsl (mod (+ hue 0.12) 1) 0.6 0.24)))))
        (dom-append-child svg gradient)
        (dom-append-child svg (dom-node 'rect `((x . ,x) (y . ,y) (width . ,size) (height . ,size)
                                                (fill . ,(format "url(#%s)" gid))
                                                (clip-path . ,(format "url(#%s)" id)))))
        (dom-append-child
         svg (dom-node 'text `((x . ,(+ x (/ size 2.0))) (y . ,(+ y (* size 0.5)))
                               (dy . "0.35em") (text-anchor . "middle")
                               (font-family . ,(supersonic-browse--font-family))
                               (font-size . ,(round (* size 0.34))) (font-weight . "bold")
                               (fill . "#ffffff") (fill-opacity . "0.92"))
                       (supersonic-browse--initials (or (plist-get item :name) (plist-get item :title)))))))))

(defun supersonic-browse--tile-image (item shape selected)
  "Return the display spec of ITEM's tile, a SHAPE, lifted out if SELECTED.
A tile not in focus draws its art slightly inset; the one in focus
fills the tile, ringed in the accent colour, with a play badge."
  (let* ((size supersonic-browse-tile-size)
         (file (supersonic-browse--art-file item (* 2 size)))
         (cache-key (list (plist-get item :id) shape selected file size))
         (cached (and supersonic-browse--images (gethash cache-key supersonic-browse--images))))
    (or cached
        (let* ((svg (svg-create size size))
               (inset (if selected 0 (round (* size 0.045))))
               (inner (- size (* 2 inset)))
               (accent (supersonic-browse--accent)))
          (supersonic-browse--art-svg svg item inner shape file inset inset)
          (if selected
              (progn
                (dom-append-child svg (supersonic-browse--shape-node
                                       shape size 3 '(fill . "none") `(stroke . ,accent) '(stroke-width . 6)))
                (let ((r (round (* size 0.12)))
                      (c (- size (round (* size 0.17)))))
                  (dom-append-child svg (dom-node 'circle `((cx . ,c) (cy . ,c) (r . ,(+ r 3)) (fill . ,(supersonic-browse--bg))
                                                            (fill-opacity . "0.5"))))
                  (dom-append-child svg (dom-node 'circle `((cx . ,c) (cy . ,c) (r . ,r) (fill . ,accent))))
                  (dom-append-child svg (supersonic-browse--icon 'play (+ c 1) c (supersonic-browse--bg)))))
            (let ((outline (supersonic-browse--shape-node shape inner 0.5 '(fill . "none")
                                                          `(stroke . ,(supersonic-browse--fg))
                                                          '(stroke-opacity . "0.12") '(stroke-width . 1))))
              (dom-set-attribute outline 'transform (format "translate(%s,%s)" inset inset))
              (dom-append-child svg outline)))
          (let ((image (apply #'svg-image svg :ascent 100 :scale 1 (and file (list :base-uri file)))))
            (unless supersonic-browse--images
              (setq supersonic-browse--images (make-hash-table :test #'equal)))
            (puthash cache-key image supersonic-browse--images))))))

;;;
;;; Inserting things
;;;

(defun supersonic-browse--space (pixels)
  "Return a stretch of space PIXELS wide."
  (propertize " " 'display `(space :width (,pixels))))

(defun supersonic-browse--align (pixels)
  "Return space reaching PIXELS from the window's left edge."
  (propertize " " 'display `(space :align-to (,pixels))))

(defun supersonic-browse--fit (string pixels)
  "Return STRING cut to fit PIXELS."
  (if (<= (supersonic--string-pixel-width string) pixels)
      string
    (supersonic--truncate-to-pixels string pixels)))

(defvar supersonic-browse-item-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'supersonic-browse-click)
    (define-key map [mouse-3] #'supersonic-browse-click-play)
    map)
  "Keymap of the clickable items in the browser.")

(defun supersonic-browse--item-properties (key item &rest more)
  "Return the text properties marking KEY, holding ITEM, plus MORE."
  (append (list 'supersonic-browse-key key 'supersonic-browse-item item
                'keymap supersonic-browse-item-map 'pointer 'hand)
          more))

(defun supersonic-browse--heading (text &optional more-label more-action)
  "Insert section heading TEXT, with a MORE-LABEL button calling MORE-ACTION."
  (supersonic-browse--new-region t)
  (insert "\n" (supersonic-browse--space supersonic-browse-margin)
          (propertize text 'face 'supersonic-browse-heading 'supersonic-browse-section t))
  (when more-label
    (let ((label (concat more-label " ›")))
      (insert (supersonic-browse--align (+ supersonic-browse-margin
                                           (- (supersonic-browse--available-width)
                                              (supersonic--string-pixel-width label))))
              (apply #'propertize label 'face 'supersonic-browse-link
                     (supersonic-browse--item-properties (list 'more more-label supersonic-browse--region)
                                                         (list :action more-action)
                                                         'supersonic-browse-focus t
                                                         'supersonic-browse-region supersonic-browse--region)))))
  (insert (propertize "\n" 'line-height 1.6)))

(defvar-local supersonic-browse--grid-count 0 "Grids inserted so far on the page.")

(defun supersonic-browse--insert-grid (kind items shape &optional max-rows)
  "Insert ITEMS, all of KIND (`artist' or `album'), as a grid of SHAPE tiles.
At most MAX-ROWS rows of them, if given."
  (supersonic-browse--new-region)
  (let* ((size supersonic-browse-tile-size)
         (gap supersonic-browse-gap)
         (columns (max 1 (/ (+ (supersonic-browse--available-width) gap) (+ size gap))))
         (rows (seq-partition items columns))
         (grid (cl-incf supersonic-browse--grid-count))
         (graphic (supersonic-browse--graphic-p)))
    (when max-rows
      (setq rows (seq-take rows max-rows)))
    (cl-loop
     for row in rows for r from 0 do
     (when graphic
       (insert (supersonic-browse--space supersonic-browse-margin))
       (cl-loop
        for item in row for c from 0 do
        (let ((key (list kind (plist-get item :id) supersonic-browse--region)))
          (insert (apply #'propertize "*" 'display (supersonic-browse--tile-image item shape nil)
                         (supersonic-browse--item-properties
                          key item 'supersonic-browse-focus t 'supersonic-browse-tile shape
                          'supersonic-browse-region supersonic-browse--region
                          'supersonic-browse-grid grid 'supersonic-browse-row r 'supersonic-browse-col c
                          'help-echo (or (plist-get item :name) (plist-get item :title)))))
          (insert (supersonic-browse--space gap))))
       (insert "\n"))
     (dolist (line (list (cons 'supersonic-browse-tile-title (lambda (item) (or (plist-get item :name) "")))
                         (cons 'supersonic-browse-tile-subtitle #'supersonic-browse--subtitle)))
       (cl-loop
        for item in row for c from 0 do
        (let ((key (list kind (plist-get item :id) supersonic-browse--region))
              (text (propertize (funcall (cdr line) item) 'face (car line))))
          (insert (supersonic-browse--align (+ supersonic-browse-margin (* c (+ size gap))))
                  (apply #'propertize (supersonic-browse--fit text (- size 4))
                         (supersonic-browse--item-properties
                          key item 'supersonic-browse-grid grid 'supersonic-browse-row r 'supersonic-browse-col c
                          'supersonic-browse-region supersonic-browse--region
                          'supersonic-browse-title (and (eq (car line) 'supersonic-browse-tile-title) key)
                          ;; Without images the title is what gets focus.
                          'supersonic-browse-focus (and (not graphic) (eq (car line) 'supersonic-browse-tile-title))
                          'face (car line))))))
       (insert "\n"))
     (insert (propertize "\n" 'line-height 0.6)))))

(defun supersonic-browse--subtitle (item)
  "Return the line under ITEM's tile.
On an artist's own page, its albums say their year and size instead
of the artist's name over and over."
  (cond
   ((and (eq (plist-get supersonic-browse--page :kind) 'artist) (plist-get item :name)
         (not (plist-get item :album-count)))
    (string-join (delq nil (list (and (plist-get item :year) (number-to-string (plist-get item :year)))
                                 (and (plist-get item :song-count)
                                      (supersonic-browse--plural (plist-get item :song-count) "song"))))
                 " · "))
   ((plist-get item :album-count) (supersonic-browse--plural (plist-get item :album-count) "album"))
   ((plist-get item :artist)
    (string-join (delq nil (list (plist-get item :artist)
                                 (and (plist-get item :year) (number-to-string (plist-get item :year)))))
                 " · "))
   ((plist-get item :year) (number-to-string (plist-get item :year)))
   (t "")))

(defun supersonic-browse--row-strut ()
  "Return an invisible stretch giving a track row its height, its text centred.
`line-height' would put all the room above the text."
  (let* ((height (round (* 1.75 (frame-char-height))))
         (info (ignore-errors (font-info (face-font 'default))))
         (ascent (if info (aref info 4) (* 0.8 (frame-char-height))))
         (descent (if info (aref info 5) (* 0.2 (frame-char-height))))
         (above (+ (/ height 2.0) (/ (- ascent descent) 2.0))))
    (propertize " " 'display `(space :width (0) :height (,height)
                                     :ascent ,(max 1 (min 100 (round (* 100 (/ above height)))))))))

(defun supersonic-browse--numbering (tracks)
  "Return the number to show for each of TRACKS.
Their own track numbers when every one has one and no two collide on
the same disc, their place in the list otherwise."
  (let ((seen (make-hash-table :test #'equal)))
    (if (and (seq-every-p (lambda (track) (natnump (plist-get track :track))) tracks)
             (seq-every-p (lambda (track)
                            (let ((key (cons (or (plist-get track :disc) 1) (plist-get track :track))))
                              (prog1 (not (gethash key seen)) (puthash key t seen))))
                          tracks))
        (mapcar (lambda (track) (plist-get track :track)) tracks)
      (number-sequence 1 (length tracks)))))

(defun supersonic-browse--insert-tracks (tracks &optional show-album)
  "Insert TRACKS as a numbered list; with SHOW-ALBUM, each track's album too.
An album over several discs gets a heading per disc."
  (supersonic-browse--new-region)
  (let* ((ids (mapcar (lambda (track) (plist-get track :id)) tracks))
         (char (frame-char-width))
         (right (+ supersonic-browse-margin (supersonic-browse--available-width)))
         (marker-x supersonic-browse-margin)
         (number-right (+ supersonic-browse-margin (* 5 char)))
         (title-x (+ number-right (* 2 char)))
         (numbers (supersonic-browse--numbering tracks))
         (discs (delete-dups (mapcar (lambda (track) (or (plist-get track :disc) 1)) tracks)))
         (album-artist (and (not show-album) (plist-get (car tracks) :artist)))
         (strut (supersonic-browse--row-strut))
         (disc nil)
         (index 0))
    (cl-loop
     for track in tracks for number in numbers do
     (when (and (cdr discs) (not show-album) (not (eql disc (or (plist-get track :disc) 1))))
       (setq disc (or (plist-get track :disc) 1))
       (insert (supersonic-browse--align title-x)
               (propertize (format "Disc %d" disc) 'face 'supersonic-browse-hint)
               (propertize "\n" 'line-height 1.6)))
     (let* ((playing (and supersonic-browse--playing (equal (plist-get track :id) supersonic-browse--playing)))
            (number (propertize (number-to-string number)
                                'face (if playing 'supersonic-browse-playing 'supersonic-browse-track-number)))
            (duration (supersonic-browse--duration (plist-get track :duration)))
            (duration-x (- right (supersonic--string-pixel-width duration)))
            (detail (string-join
                     (delq nil (list (and (or show-album (not (equal (plist-get track :artist) album-artist)))
                                          (plist-get track :artist))
                                     (and show-album (plist-get track :album))))
                     " · "))
            (title (propertize (or (plist-get track :title) "?")
                               'face (if playing 'supersonic-browse-playing 'supersonic-browse-track)))
            (detail (and (not (string-empty-p detail)) (propertize (concat "   " detail) 'face 'shadow)))
            (shown (supersonic-browse--fit (concat title detail) (- duration-x title-x (* 2 char))))
            (properties (supersonic-browse--item-properties
                         (list 'track (plist-get track :id) index) track
                         'supersonic-browse-ids ids 'supersonic-browse-index index
                         'supersonic-browse-region supersonic-browse--region)))
       (insert (apply #'propertize
                      (concat strut
                              (supersonic-browse--align marker-x)
                              (if playing (propertize "▶" 'face 'supersonic-browse-playing) " ")
                              (supersonic-browse--align (- number-right (supersonic--string-pixel-width number)))
                              number
                              (supersonic-browse--align title-x)
                              shown
                              (supersonic-browse--align duration-x)
                              (propertize duration 'face 'supersonic-browse-track-number)
                              "\n")
                      properties))
       ;; The row takes focus at its start.
       (save-excursion
         (forward-line -1)
         (put-text-property (point) (1+ (point)) 'supersonic-browse-focus t))
       (setq index (1+ index))))))

;;;; The header of an artist or album page

(defun supersonic-browse--icon (icon cx cy color)
  "Return an SVG node drawing ICON centred on CX, CY in COLOR.
Drawn rather than taken from a font, which may lack the glyph."
  (pcase icon
    ('play (dom-node 'path `((d . ,(format "M%s %s L%s %s L%s %s Z" (- cx 5) (- cy 7) (+ cx 7) cy (- cx 5) (+ cy 7)))
                             (fill . ,color))))
    ('plus (dom-node 'path `((d . ,(format "M%s %s V%s M%s %s H%s" cx (- cy 7) (+ cy 7) (- cx 7) cy (+ cx 7)))
                             (stroke . ,color) (stroke-width . 2.2) (stroke-linecap . "round") (fill . "none"))))
    ('shuffle
     (dom-node 'path `((d . ,(format "M%s %s C%s %s %s %s %s %s M%s %s C%s %s %s %s %s %s M%s %s L%s %s L%s %s M%s %s L%s %s L%s %s"
                                     (- cx 8) (- cy 5) cx (- cy 5) cx (+ cy 5) (+ cx 8) (+ cy 5)
                                     (- cx 8) (+ cy 5) cx (+ cy 5) cx (- cy 5) (+ cx 8) (- cy 5)
                                     (+ cx 5) (- cy 8) (+ cx 8) (- cy 5) (+ cx 5) (- cy 2)
                                     (+ cx 5) (+ cy 2) (+ cx 8) (+ cy 5) (+ cx 5) (+ cy 8)))
                       (stroke . ,color) (stroke-width . 2) (stroke-linecap . "round") (stroke-linejoin . "round")
                       (fill . "none"))))))

(defun supersonic-browse--hero (item shape kicker title subtitle meta)
  "Insert the header of a page about ITEM, its art drawn as SHAPE.
KICKER, TITLE, SUBTITLE and META are the lines beside it.  Art and text
are one SVG with play, shuffle and queue buttons drawn in, clickable
through the image's map."
  (let* ((graphic (supersonic-browse--graphic-p))
         (key '(page))
         (properties (progn
                       (supersonic-browse--new-region)
                       (supersonic-browse--item-properties key item 'supersonic-browse-focus t
                                                           'supersonic-browse-region supersonic-browse--region))))
    (if (not graphic)
        (insert (apply #'propertize
                       (concat (supersonic-browse--space supersonic-browse-margin)
                               (propertize title 'face 'supersonic-browse-greeting) "\n"
                               (supersonic-browse--space supersonic-browse-margin) (or subtitle "") "  " (or meta ""))
                       properties)
                "\n")
      (let* ((size supersonic-browse-hero-size)
             (width (max (+ size 200) (supersonic-browse--available-width)))
             (file (supersonic-browse--art-file item supersonic-now-playing-art-size))
             (svg (svg-create width size))
             (family (supersonic-browse--font-family))
             (x (+ size 40))
             (room (- width x))
             (title-size (min 56 (max 28 (round (* size 0.2)))))
             (char (* 0.56 title-size))
             (title (truncate-string-to-width title (max 4 (floor (/ room char))) nil nil "…"))
             (h 44)
             (button-y (- size h))
             ;; Bottom-up: buttons at the foot of the art, the text stacked above.
             (meta-y (- button-y 24))
             (subtitle-y (- meta-y (if meta 34 0)))
             (title-y (- subtitle-y (if subtitle 42 0)))
             (kicker-y (- title-y title-size 6))
             (buttons '((supersonic-browse-play play "Play")
                        (supersonic-browse-shuffle shuffle "Shuffle")
                        (supersonic-browse-enqueue plus "Queue")))
             (bx x)
             (map nil))
        (supersonic-browse--art-svg svg item size shape file)
        (cl-flet ((text (y content font-size color &rest more)
                    (dom-append-child svg (dom-node 'text `((x . ,x) (y . ,y) (font-family . ,family)
                                                            (font-size . ,font-size) (fill . ,color) ,@more)
                                                    content))))
          (text kicker-y (upcase kicker) 13 (supersonic-browse--subtle)
                '(letter-spacing . "3") '(font-weight . "bold"))
          (text title-y title title-size (supersonic-browse--fg) '(font-weight . "bold"))
          (when subtitle
            (text subtitle-y subtitle 22 (supersonic-browse--accent)))
          (when meta
            (text meta-y meta 17 (supersonic-browse--subtle))))
        (cl-loop
         for (command icon label) in buttons for primary = t then nil do
         (let* ((w (+ 58 (* (length label) 11)))
                (ink (if primary (supersonic-browse--bg) (supersonic-browse--fg))))
           (dom-append-child svg (dom-node 'rect `((x . ,bx) (y . ,button-y) (width . ,w) (height . ,h) (rx . ,(/ h 2))
                                                   (fill . ,(if primary (supersonic-browse--accent) "none"))
                                                   (stroke . ,(if primary "none" (supersonic-browse--subtle)))
                                                   (stroke-width . 1.5))))
           (dom-append-child svg (supersonic-browse--icon icon (+ bx 22) (+ button-y (/ h 2)) ink))
           (dom-append-child svg (dom-node 'text `((x . ,(+ bx 38)) (y . ,(+ button-y (/ h 2))) (dy . "0.35em")
                                                   (font-family . ,family) (font-size . 17) (font-weight . "bold")
                                                   (fill . ,ink))
                                           label))
           (push `((rect . ((,bx . ,button-y) . (,(+ bx w) . ,(+ button-y h))))
                   ,command (pointer hand help-echo ,label))
                 map)
           (setq bx (+ bx w 14))))
        (let ((keymap (make-sparse-keymap)))
          (dolist (area map)
            (define-key keymap (vector (nth 1 area) 'mouse-1) (nth 1 area)))
          (set-keymap-parent keymap supersonic-browse-item-map)
          (insert (propertize "\n" 'line-height 0.5)
                  (supersonic-browse--space supersonic-browse-margin)
                  (apply #'propertize "*"
                         'display (apply #'svg-image svg :ascent 100 :scale 1 :map map (and file (list :base-uri file)))
                         (plist-put (copy-sequence properties) 'keymap keymap))
                  "\n"))))))

;;;
;;; Pages
;;;

(defun supersonic-browse--page-title (page)
  "Return the short title of PAGE, for the trail."
  (pcase (plist-get page :kind)
    ('home "Home")
    ('artists "Artists")
    ('albums (supersonic-browse--list-title (plist-get page :type)))
    ('artist (or (plist-get (plist-get page :item) :name) "Artist"))
    ('album (or (plist-get (plist-get page :item) :name) "Album"))
    ('search (format "“%s”" (plist-get page :query)))
    (_ "?")))

(defun supersonic-browse--list-title (type)
  "Return the title of album list TYPE."
  (or (cdr (assq type supersonic-browse-home-sections))
      (capitalize (symbol-name type))))

(defun supersonic-browse--greeting ()
  "Return a greeting for the time of day."
  (let ((hour (string-to-number (format-time-string "%H"))))
    (cond ((< hour 5) "Late night listening")
          ((< hour 12) "Good morning")
          ((< hour 18) "Good afternoon")
          (t "Good evening"))))

(aio-defun
 supersonic-browse--load-content (page)
 "Return a promise resolving to what PAGE shows."
 (pcase (plist-get page :kind)
   ('home
    (let ((promises (mapcar (lambda (section)
                              (cons section (aio-catch (supersonic-provider-album-list
                                                        (car section) supersonic-browse-section-count))))
                            supersonic-browse-home-sections))
          (sections nil))
      (dolist (pending promises)
        (let ((outcome (aio-await (cdr pending))))
          (when (and (eq (car outcome) :success) (cdr outcome))
            (push (cons (car pending) (cdr outcome)) sections))))
      (list :sections (nreverse sections))))
   ('artists (list :artists (aio-await (supersonic-provider-artists))))
   ('albums (list :albums (aio-await (supersonic-provider-album-list (plist-get page :type) supersonic-album-list-count))))
   ('artist (list :albums (aio-await (supersonic-provider-artist-albums (plist-get page :id)))))
   ('album (list :tracks (aio-await (supersonic-provider-album-tracks (plist-get page :id)))))
   ('search (list :results (aio-await (supersonic-provider-search (plist-get page :query)))))))

(defun supersonic-browse--render-trail ()
  "Insert the trail of pages back to here, and the key hints."
  (let* ((pages (reverse (cons supersonic-browse--page (seq-take supersonic-browse--back 3))))
         (depth (1- (length pages))))
    (insert (supersonic-browse--space supersonic-browse-margin))
    (cl-loop
     for page in pages for i from 0 do
     (let ((title (supersonic-browse--page-title page))
           (steps (- depth i)))
       (insert (if (zerop steps)
                   (propertize title 'face 'supersonic-browse-trail-current)
                 (propertize title 'face 'supersonic-browse-trail 'mouse-face 'highlight 'pointer 'hand
                             'help-echo "Go back here"
                             'keymap (let ((map (make-sparse-keymap)))
                                       (define-key map [mouse-1] (lambda () (interactive) (supersonic-browse-back steps)))
                                       map))))
       (unless (zerop steps)
         (insert (propertize "  ›  " 'face 'supersonic-browse-trail)))))
    (let ((hints (propertize (supersonic-browse--hints) 'face 'supersonic-browse-hint)))
      (when (> (- (supersonic-browse--available-width) (supersonic--string-pixel-width hints)) 300)
        (insert (supersonic-browse--align (- (+ supersonic-browse-margin (supersonic-browse--available-width))
                                             (supersonic--string-pixel-width hints)))
                hints)))
    (insert (propertize "\n" 'line-height 1.8))))

(defun supersonic-browse--hints ()
  "Return the key hints, read off the keys actually bound."
  (mapconcat
   (lambda (pair)
     (let ((key (where-is-internal (car pair) supersonic-browse-mode-map t)))
       (if key (concat (key-description key) " " (cdr pair)) "")))
   '((supersonic-browse-open . "open") (supersonic-browse-play . "play")
     (supersonic-browse-shuffle . "shuffle") (supersonic-browse-enqueue . "queue")
     (supersonic-browse-next-section . "section") (supersonic-browse-back . "back")
     (supersonic-browse-search . "search") (describe-mode . "keys"))
   "   "))

(defun supersonic-browse--title (title &optional subtitle)
  "Insert a page TITLE, with SUBTITLE beside it."
  (insert (supersonic-browse--space supersonic-browse-margin)
          (propertize title 'face 'supersonic-browse-greeting))
  (when subtitle
    (insert "   " (propertize subtitle 'face 'supersonic-browse-hint)))
  (insert (propertize "\n" 'line-height 1.4)))

(defun supersonic-browse--render-home (content)
  "Insert the home page from CONTENT."
  (supersonic-browse--title (supersonic-browse--greeting))
  (insert (supersonic-browse--space supersonic-browse-margin))
  (supersonic-browse--new-region)
  (dolist (button `(("▶ Surprise me" . supersonic-browse-surprise)
                    ("Artists" . supersonic-browse-artists)
                    ("Search" . supersonic-browse-search)))
    (insert (apply #'propertize (concat " " (car button) " ")
                   'face 'supersonic-browse-button
                   (supersonic-browse--item-properties (list 'button (cdr button)) (list :action (cdr button))
                                                       'supersonic-browse-focus t
                                                       'supersonic-browse-region supersonic-browse--region))
            "  "))
  (insert "\n")
  (let ((sections (plist-get content :sections)))
    (if (not sections)
        (insert "\n" (supersonic-browse--space supersonic-browse-margin)
                (propertize "Nothing here yet -- try Artists or Search." 'face 'shadow) "\n")
      (dolist (section sections)
        (let ((type (car (car section))))
          (supersonic-browse--heading (cdr (car section)) "See all"
                                      (lambda () (supersonic-browse-album-list type)))
          (supersonic-browse--insert-grid 'album (cdr section) 'square 1))))))

(defun supersonic-browse--render-page (content)
  "Insert the body of the current page from CONTENT."
  (let ((page supersonic-browse--page))
    (pcase (plist-get page :kind)
      ('home (supersonic-browse--render-home content))
      ('artists
       (let ((artists (plist-get content :artists)))
         (supersonic-browse--title "Artists" (supersonic-browse--plural (length artists) "artist"))
         (insert "\n")
         (supersonic-browse--insert-grid 'artist artists 'circle)))
      ('albums
       (let ((albums (plist-get content :albums)))
         (supersonic-browse--title (supersonic-browse--list-title (plist-get page :type))
                                   (supersonic-browse--plural (length albums) "album"))
         (insert "\n")
         (supersonic-browse--insert-grid 'album albums 'square)))
      ('artist
       (let* ((albums (plist-get content :albums))
              (item (plist-get page :item))
              (songs (apply #'+ (mapcar (lambda (a) (or (plist-get a :song-count) 0)) albums))))
         (supersonic-browse--hero
          (if (plist-get item :art) item (append (list :art (plist-get (car albums) :art)) item))
          'circle "Artist" (or (plist-get item :name) "")
          (supersonic-browse--plural (length albums) "album")
          (and (> songs 0) (supersonic-browse--plural songs "song")))
         (supersonic-browse--heading "Albums")
         (supersonic-browse--insert-grid 'album albums 'square)))
      ('album
       (let* ((tracks (plist-get content :tracks))
              (item (plist-get page :item))
              (first (car tracks))
              (artist (or (plist-get item :artist) (plist-get first :artist)))
              (year (or (plist-get item :year) (plist-get first :year)))
              (total (apply #'+ (mapcar (lambda (track) (or (plist-get track :duration) 0)) tracks))))
         (supersonic-browse--hero
          (if (plist-get item :art) item (append (list :art (plist-get first :art)) item))
          'square "Album" (or (plist-get item :name) (plist-get first :album) "")
          artist
          (string-join (delq nil (list (and year (number-to-string year))
                                       (supersonic-browse--plural (length tracks) "song")
                                       (supersonic-browse--total total)))
                       " · "))
         (insert (propertize "\n" 'line-height 1.4))
         (supersonic-browse--insert-tracks tracks)))
      ('search
       (let ((results (plist-get content :results)))
         (supersonic-browse--title (format "“%s”" (plist-get page :query)))
         (unless (or (plist-get results :artists) (plist-get results :albums) (plist-get results :tracks))
           (insert "\n" (supersonic-browse--space supersonic-browse-margin)
                   (propertize "Nothing found." 'face 'shadow) "\n"))
         (when (plist-get results :artists)
           (supersonic-browse--heading "Artists")
           (supersonic-browse--insert-grid 'artist (plist-get results :artists) 'circle 1))
         (when (plist-get results :albums)
           (supersonic-browse--heading "Albums")
           (supersonic-browse--insert-grid 'album (plist-get results :albums) 'square 2))
         (when (plist-get results :tracks)
           (supersonic-browse--heading "Songs")
           (supersonic-browse--insert-tracks (seq-take (plist-get results :tracks) 20) t)))))))

(defun supersonic-browse--render ()
  "Lay the current page out anew for the width of the window showing it.
Run in that window, whatever is selected: layout measures text as that
window's frame draws it, and loads finish in process callbacks."
  (let ((window (get-buffer-window (current-buffer) t)))
    (if (and window (not (eq window (selected-window))))
        (with-selected-window window
          (supersonic-browse--render-here))
      (supersonic-browse--render-here))))

(defun supersonic-browse--render-here ()
  "Lay the current page out anew, in the selected window."
  (let ((inhibit-read-only t)
        (selected supersonic-browse--selected)
        (start (window-start (supersonic-browse--window))))
    (erase-buffer)
    (setq supersonic-browse--grid-count 0
          supersonic-browse--region 0
          supersonic-browse--region-fresh nil
          supersonic-browse--width (window-body-width (supersonic-browse--window) t))
    (dolist (overlay (list supersonic-browse--title-overlay supersonic-browse--bar-overlay
                           supersonic-browse--focus-overlay))
      (when overlay (delete-overlay overlay)))
    (insert (propertize "\n" 'line-height 1.4))
    (supersonic-browse--render-trail)
    (if (not supersonic-browse--content)
        (insert (supersonic-browse--space supersonic-browse-margin)
                (propertize "Loading…" 'face 'supersonic-browse-hint) "\n")
      (supersonic-browse--render-page supersonic-browse--content))
    (insert "\n")
    (supersonic-browse--index-stops)
    (goto-char (let ((i (or (supersonic-browse--key-index selected) 0)))
                 (if (< i (supersonic-browse--stop-count)) (aref supersonic-browse--stops i) (point-min))))
    (when (get-buffer-window (current-buffer))
      (set-window-start (get-buffer-window (current-buffer))
                        (if (and selected (< start (point))) start (point-min))
                        t))
    (setq supersonic-browse--selected nil)
    (supersonic-browse--update-focus)
    (supersonic-browse--fetch-art)))

;;;; Art fetched once a page is shown

(defun supersonic-browse--wanted-art ()
  "Return (ITEM . SIZE) for every image on the page whose art is not cached."
  (let ((wanted nil)
        (pos (point-min)))
    (while (setq pos (next-single-property-change pos 'supersonic-browse-item))
      (let ((item (get-text-property pos 'supersonic-browse-item))
            (size (cond ((get-text-property pos 'supersonic-browse-tile) (* 2 supersonic-browse-tile-size))
                        ((equal (get-text-property pos 'supersonic-browse-key) '(page)) supersonic-now-playing-art-size))))
        (when (and size item (plist-get item :art) (not (supersonic-browse--art-file item size))
                   (not (supersonic-browse--art-failed-p (plist-get item :art) size)))
          (push (cons item size) wanted))))
    (seq-uniq wanted (lambda (a b) (and (equal (plist-get (car a) :art) (plist-get (car b) :art))
                                        (= (cdr a) (cdr b)))))))

(defvar supersonic-browse--art-failures (make-hash-table :test #'equal)
  "When fetching each (ART . SIZE) last failed, to wait before trying again.")

(defun supersonic-browse--art-failed-p (art size)
  "Return non-nil if fetching ART at SIZE failed less than five minutes ago."
  (let ((when (gethash (cons art size) supersonic-browse--art-failures)))
    (and when (< (- (float-time) when) 300))))

(defun supersonic-browse--fetch-art ()
  "Fetch the art the page lacks, redrawing the page as it arrives."
  (when (supersonic-art-available-p)
    (let ((buff (current-buffer))
          (generation supersonic-browse--generation)
          (wanted (supersonic-browse--wanted-art))
          (timer nil))
      (unless supersonic-browse--sem
        (setq supersonic-browse--sem (aio-sem supersonic-art-fetch-concurrency)))
      (dolist (pair wanted)
        (let ((promise (supersonic--fetch-art-throttled supersonic-browse--sem (plist-get (car pair) :art) (cdr pair))))
          (aio-listen promise
                      (lambda (_)
                        (unless (file-exists-p (supersonic-art-cache-file (plist-get (car pair) :art) (cdr pair)))
                          (puthash (cons (plist-get (car pair) :art) (cdr pair)) (float-time)
                                   supersonic-browse--art-failures))
                        ;; Redraw once a burst of arrivals settles, not per cover.
                        (when (and (buffer-live-p buff) (not timer))
                          (setq timer
                                (run-at-time 0.15 nil
                                             (lambda ()
                                               (setq timer nil)
                                               (when (buffer-live-p buff)
                                                 (with-current-buffer buff
                                                   (when (= generation supersonic-browse--generation)
                                                     (supersonic-browse--redraw-images)))))))))))))))

(defun supersonic-browse--redraw-images ()
  "Redraw every image on the page whose art may have arrived."
  (let ((window (get-buffer-window (current-buffer) t)))
    (if (and window (not (eq window (selected-window))))
        (with-selected-window window
          (supersonic-browse--redraw-images-here))
      (supersonic-browse--redraw-images-here))))

(defun supersonic-browse--redraw-images-here ()
  "Redraw the images whose art may have arrived, in the selected window."
  (let ((inhibit-read-only t)
        (pos (point-min)))
    (while (setq pos (next-single-property-change pos 'supersonic-browse-item))
      (let ((shape (get-text-property pos 'supersonic-browse-tile))
            (key (get-text-property pos 'supersonic-browse-key)))
        (cond
         (shape
          (put-text-property pos (1+ pos) 'display
                             (supersonic-browse--tile-image (get-text-property pos 'supersonic-browse-item) shape
                                                            (equal key supersonic-browse--selected))))
         ((and (equal key '(page)) (get-text-property pos 'display))
          ;; The header is one image: lay the page out again.
          (setq pos (point-max))
          (supersonic-browse--render)))))))

;;;
;;; Focus and moving about
;;;

;; Where items take focus is indexed once per layout, in
;; `supersonic-browse--stops', so moving about is a lookup, not a scan.
;; Items are grouped in regions -- the home page's buttons, each section,
;; an artist's or album's header, a tracklist -- which TAB moves between.


(defun supersonic-browse--new-region (&optional by-heading)
  "Start a new region of items, unless a heading BY-HEADING just did."
  (if supersonic-browse--region-fresh
      (setq supersonic-browse--region-fresh nil)
    (cl-incf supersonic-browse--region)
    (setq supersonic-browse--region-fresh by-heading)))

(defun supersonic-browse--index-stops ()
  "Index the positions items take focus at, once the page is laid out."
  (let ((stops nil)
        (index (make-hash-table :test #'equal))
        (pos (point-min)))
    (when (get-text-property pos 'supersonic-browse-focus)
      (push pos stops))
    (while (setq pos (next-single-property-change pos 'supersonic-browse-focus))
      (when (get-text-property pos 'supersonic-browse-focus)
        (push pos stops)))
    (setq supersonic-browse--stops (vconcat (nreverse stops)))
    (dotimes (i (length supersonic-browse--stops))
      (let ((key (get-text-property (aref supersonic-browse--stops i) 'supersonic-browse-key)))
        (unless (gethash key index)
          (puthash key i index))))
    (setq supersonic-browse--stop-index index)))

(defun supersonic-browse--stop-count ()
  "Return how many items can take focus."
  (length supersonic-browse--stops))

(defun supersonic-browse--prop (i property)
  "Return PROPERTY of the item at stop I."
  (get-text-property (aref supersonic-browse--stops i) property))

(defun supersonic-browse--key-index (key)
  "Return the stop index of the item KEY, or nil."
  (and key supersonic-browse--stop-index (gethash key supersonic-browse--stop-index)))

(defun supersonic-browse--current-index ()
  "Return the stop index of the item at point, else of the last before point."
  (or (supersonic-browse--key-index (get-text-property (point) 'supersonic-browse-key))
      (let ((i (1- (supersonic-browse--stop-count))))
        (while (and (> i 0) (> (aref supersonic-browse--stops i) (point)))
          (setq i (1- i)))
        (max 0 i))))

(defun supersonic-browse--goto-index (i)
  "Focus the item at stop I, if there is one."
  (when (and i (>= i 0) (< i (supersonic-browse--stop-count)))
    (goto-char (aref supersonic-browse--stops i))
    (supersonic-browse--update-focus)
    t))

(defun supersonic-browse--blend (a b amount)
  "Return colour A mixed into colour B by AMOUNT, 0 to 1."
  (let ((ra (color-name-to-rgb a)) (rb (color-name-to-rgb b)))
    (if (and ra rb)
        (apply #'color-rgb-to-hex
               (append (cl-mapcar (lambda (x y) (+ (* amount x) (* (- 1 amount) y))) ra rb) '(2)))
      b)))

(defun supersonic-browse--paint (pos on)
  "Show the item taking focus at POS as in focus if ON, else as not."
  (let ((inhibit-read-only t)
        (key (get-text-property pos 'supersonic-browse-key))
        (shape (get-text-property pos 'supersonic-browse-tile)))
    (when shape
      (put-text-property pos (1+ pos) 'display
                         (supersonic-browse--tile-image (get-text-property pos 'supersonic-browse-item) shape on)))
    (when on
      (let ((title (save-excursion
                     (goto-char (1+ pos))
                     (let ((match (text-property-search-forward 'supersonic-browse-title key t)))
                       (and match (cons (prop-match-beginning match) (prop-match-end match)))))))
        (unless supersonic-browse--title-overlay
          (setq supersonic-browse--title-overlay (make-overlay 1 1))
          (overlay-put supersonic-browse--title-overlay 'face '(:inherit (supersonic-browse-accent bold)))
          (setq supersonic-browse--bar-overlay (make-overlay 1 1))
          (setq supersonic-browse--focus-overlay (make-overlay 1 1)))
        (cond
         ((and shape title)
          (overlay-put supersonic-browse--title-overlay 'face '(:inherit (supersonic-browse-accent bold)))
          (move-overlay supersonic-browse--title-overlay (car title) (cdr title)))
         ((memq (car-safe key) '(button more))
          ;; A button or link lights up as a whole.
          (overlay-put supersonic-browse--title-overlay 'face
                       `(:background ,(supersonic-browse--accent) :foreground ,(supersonic-browse--bg)
                                     :box (:line-width (14 . 7) :color ,(supersonic-browse--accent))))
          (move-overlay supersonic-browse--title-overlay pos
                        (or (next-single-property-change pos 'supersonic-browse-key) (point-max))))
         (t (delete-overlay supersonic-browse--title-overlay)))
        (if (eq (car-safe key) 'track)
            (let ((start (save-excursion (goto-char pos) (line-beginning-position)))
                  (end (save-excursion (goto-char pos) (1+ (line-end-position))))
                  (accent (supersonic-browse--accent)))
              (overlay-put supersonic-browse--focus-overlay 'face
                           `(:background ,(supersonic-browse--blend accent (supersonic-browse--bg) 0.16) :extend t))
              (move-overlay supersonic-browse--focus-overlay start end)
              (overlay-put supersonic-browse--bar-overlay 'before-string
                           (concat (propertize " " 'display `(space :align-to (,(- supersonic-browse-margin 18))))
                                   (propertize " " 'display '(space :width (4)) 'face `(:background ,accent))))
              (move-overlay supersonic-browse--bar-overlay start start))
          (delete-overlay supersonic-browse--focus-overlay)
          (delete-overlay supersonic-browse--bar-overlay))))))

(defun supersonic-browse--update-focus ()
  "Show which item is in focus: ringed and named if a tile, highlighted if a row."
  (let ((key (get-text-property (point) 'supersonic-browse-key)))
    (unless (equal key supersonic-browse--selected)
      (let ((old (supersonic-browse--key-index supersonic-browse--selected))
            (new (supersonic-browse--key-index key)))
        (when old
          (supersonic-browse--paint (aref supersonic-browse--stops old) nil))
        (setq supersonic-browse--selected key)
        (when new
          (supersonic-browse--paint (aref supersonic-browse--stops new) t))))))

(defun supersonic-browse--post-command ()
  "Keep the focus shown where point is, after every command."
  (when (and (eq major-mode 'supersonic-browse-mode)
             (not (get-text-property (point) 'supersonic-browse-focus))
             (get-text-property (point) 'supersonic-browse-key))
    ;; Clicked or moved onto a name: the tile it names takes focus.
    (let ((i (supersonic-browse--key-index (get-text-property (point) 'supersonic-browse-key))))
      (when i (goto-char (aref supersonic-browse--stops i)))))
  (supersonic-browse--update-focus))

(defun supersonic-browse-next ()
  "Focus the next item."
  (interactive)
  (supersonic-browse--goto-index (min (1+ (supersonic-browse--current-index)) (1- (supersonic-browse--stop-count)))))

(defun supersonic-browse-previous ()
  "Focus the previous item."
  (interactive)
  (supersonic-browse--goto-index (max 0 (1- (supersonic-browse--current-index)))))

(defun supersonic-browse-first ()
  "Focus the first item."
  (interactive)
  (supersonic-browse--goto-index 0)
  (goto-char (point-min))
  (supersonic-browse--goto-index 0))

(defun supersonic-browse-last ()
  "Focus the last item."
  (interactive)
  (supersonic-browse--goto-index (1- (supersonic-browse--stop-count))))

(defun supersonic-browse--grid-stop (grid row col)
  "Return the stop of GRID's tile at ROW and column COL, or the last before it."
  (let ((best nil))
    (dotimes (i (supersonic-browse--stop-count))
      (when (and (eql (supersonic-browse--prop i 'supersonic-browse-grid) grid)
                 (eql (supersonic-browse--prop i 'supersonic-browse-row) row)
                 (<= (supersonic-browse--prop i 'supersonic-browse-col) col))
        (setq best i)))
    best))

(defun supersonic-browse-down ()
  "Focus the item below: the tile in this column a grid row down, or the row below."
  (interactive)
  (let* ((i (supersonic-browse--current-index))
         (n (supersonic-browse--stop-count))
         (grid (and (> n 0) (supersonic-browse--prop i 'supersonic-browse-grid))))
    (supersonic-browse--goto-index
     (or (and grid (supersonic-browse--grid-stop grid (1+ (supersonic-browse--prop i 'supersonic-browse-row))
                                                 (supersonic-browse--prop i 'supersonic-browse-col)))
         (let ((j (1+ i))
               (line-end (save-excursion (goto-char (aref supersonic-browse--stops i)) (line-end-position))))
           ;; Past the rest of this grid, or of this line of buttons.
           (while (and (< j n)
                       (if grid
                           (eql (supersonic-browse--prop j 'supersonic-browse-grid) grid)
                         (<= (aref supersonic-browse--stops j) line-end)))
             (setq j (1+ j)))
           (and (< j n) j))))))

(defun supersonic-browse-up ()
  "Focus the item above: the tile in this column a grid row up, or the row above."
  (interactive)
  (let* ((i (supersonic-browse--current-index))
         (n (supersonic-browse--stop-count))
         (grid (and (> n 0) (supersonic-browse--prop i 'supersonic-browse-grid)))
         (row (and grid (supersonic-browse--prop i 'supersonic-browse-row)))
         (col (and grid (supersonic-browse--prop i 'supersonic-browse-col))))
    (supersonic-browse--goto-index
     (or (and grid (> row 0) (supersonic-browse--grid-stop grid (1- row) col))
         (let ((j (1- i))
               (line-start (save-excursion (goto-char (aref supersonic-browse--stops i)) (line-beginning-position))))
           (while (and (>= j 0)
                       (if grid
                           (eql (supersonic-browse--prop j 'supersonic-browse-grid) grid)
                         (>= (aref supersonic-browse--stops j) line-start)))
             (setq j (1- j)))
           (when (>= j 0)
             (let ((other (supersonic-browse--prop j 'supersonic-browse-grid)))
               ;; Into a grid from below: its last row, the same column.
               (or (and other col (supersonic-browse--grid-stop other (supersonic-browse--prop j 'supersonic-browse-row) col))
                   j))))))))

(defun supersonic-browse--region-first (region)
  "Return the first stop of REGION, past a \"See all\" link if any."
  (let ((first nil) (best nil))
    (dotimes (i (supersonic-browse--stop-count))
      (when (eql (supersonic-browse--prop i 'supersonic-browse-region) region)
        (unless first (setq first i))
        (when (and (not best) (not (eq (car-safe (supersonic-browse--prop i 'supersonic-browse-key)) 'more)))
          (setq best i))))
    (or best first)))

(defun supersonic-browse-next-section ()
  "Focus the first item of the next region, from the last back to the first."
  (interactive)
  (let* ((n (supersonic-browse--stop-count))
         (i (supersonic-browse--current-index))
         (region (and (> n 0) (supersonic-browse--prop i 'supersonic-browse-region)))
         (j i))
    (while (and (< j n) (eql (supersonic-browse--prop j 'supersonic-browse-region) region))
      (setq j (1+ j)))
    (when (> n 0)
      (supersonic-browse--goto-index
       (supersonic-browse--region-first (supersonic-browse--prop (if (< j n) j 0) 'supersonic-browse-region))))))

(defun supersonic-browse-previous-section ()
  "Focus the first item of the previous region, from the first round to the last."
  (interactive)
  (let* ((n (supersonic-browse--stop-count))
         (i (supersonic-browse--current-index))
         (region (and (> n 0) (supersonic-browse--prop i 'supersonic-browse-region)))
         (j i))
    (while (and (>= j 0) (eql (supersonic-browse--prop j 'supersonic-browse-region) region))
      (setq j (1- j)))
    (when (> n 0)
      (supersonic-browse--goto-index
       (supersonic-browse--region-first (supersonic-browse--prop (if (>= j 0) j (1- n)) 'supersonic-browse-region))))))

;;;
;;; Visiting pages
;;;

(aio-defun
 supersonic-browse--load (buff)
 "Load the current page of BUFF and lay it out."
 (let ((page (buffer-local-value 'supersonic-browse--page buff))
       (generation (with-current-buffer buff (cl-incf supersonic-browse--generation))))
   (with-current-buffer buff
     (setq supersonic-browse--content nil)
     (supersonic-browse--render))
   (condition-case err
       (let ((content (aio-await (supersonic-browse--load-content page))))
         (when (and (buffer-live-p buff) (= generation (buffer-local-value 'supersonic-browse--generation buff)))
           (with-current-buffer buff
             (setq supersonic-browse--content content)
             (supersonic-browse--render))))
     (error
      (when (buffer-live-p buff)
        (with-current-buffer buff
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert "\n" (supersonic-browse--space supersonic-browse-margin)
                    (propertize (format "Could not load this page: %s" (error-message-string err))
                                'face 'error)
                    "\n"))))))))

(defun supersonic-browse--buffer ()
  "Return the browser's buffer, made ready."
  (let ((buff (get-buffer-create supersonic-browse-buffer-name)))
    (with-current-buffer buff
      (unless (derived-mode-p 'supersonic-browse-mode)
        (supersonic-browse-mode)))
    buff))

(defun supersonic-browse-visit (page)
  "Show PAGE in the browser, remembering the page left for going back."
  (let ((buff (supersonic-browse--buffer)))
    (pop-to-buffer-same-window buff)
    (with-current-buffer buff
      (when supersonic-browse--page
        (push (plist-put (copy-sequence supersonic-browse--page) :selected supersonic-browse--selected)
              supersonic-browse--back))
      (setq supersonic-browse--forward nil
            supersonic-browse--page page
            supersonic-browse--selected nil)
      (ignore (supersonic-browse--load buff)))))

(defun supersonic-browse--show (page)
  "Show PAGE, kept from before, focus on what had it then."
  (setq supersonic-browse--page page
        supersonic-browse--selected (plist-get page :selected))
  (ignore (supersonic-browse--load (current-buffer))))

(defun supersonic-browse-back (&optional steps)
  "Go back STEPS pages, one by default."
  (interactive "p")
  (dotimes (_ (or steps 1))
    (unless supersonic-browse--back
      (user-error "Nothing to go back to"))
    (push (plist-put (copy-sequence supersonic-browse--page) :selected supersonic-browse--selected)
          supersonic-browse--forward)
    (setq supersonic-browse--page (pop supersonic-browse--back)))
  (supersonic-browse--show supersonic-browse--page))

(defun supersonic-browse-forward ()
  "Go forward again, to the page gone back from."
  (interactive)
  (unless supersonic-browse--forward
    (user-error "Nothing to go forward to"))
  (push (plist-put (copy-sequence supersonic-browse--page) :selected supersonic-browse--selected)
        supersonic-browse--back)
  (supersonic-browse--show (pop supersonic-browse--forward)))

(defun supersonic-browse-reload ()
  "Load the page anew."
  (interactive)
  (ignore (supersonic-browse--load (current-buffer))))

;;;###autoload
(defun supersonic-browse ()
  "Open the library browser on its home page, or where it was left."
  (interactive)
  (let ((buff (supersonic-browse--buffer)))
    (if (buffer-local-value 'supersonic-browse--page buff)
        (pop-to-buffer-same-window buff)
      (supersonic-browse-visit '(:kind home)))))

;;;###autoload
(defun supersonic-browse-home ()
  "Show the browser's home page."
  (interactive)
  (supersonic-browse-visit '(:kind home)))

;;;###autoload
(defun supersonic-browse-artists ()
  "Show every artist in the browser."
  (interactive)
  (supersonic-browse-visit '(:kind artists)))

;;;###autoload
(defun supersonic-browse-album-list (type)
  "Show album list TYPE in the browser.
TYPE is one of `supersonic-provider-album-list-types'."
  (interactive (list (intern (completing-read "Albums: " (mapcar #'symbol-name supersonic-provider-album-list-types)
                                              nil t))))
  (supersonic-browse-visit (list :kind 'albums :type type)))

;;;###autoload
(defun supersonic-browse-search (query)
  "Show what matches QUERY in the browser."
  (interactive (list (read-string "Search music: ")))
  (supersonic-browse-visit (list :kind 'search :query query)))

;;;
;;; Acting on items
;;;

(defun supersonic-browse--target ()
  "Return the item in focus as (KIND . ITEM): the page's own item on its header."
  (let ((key (get-text-property (point) 'supersonic-browse-key))
        (item (get-text-property (point) 'supersonic-browse-item)))
    (pcase (car-safe key)
      ('page (cons (plist-get supersonic-browse--page :kind)
                   (append (list :id (plist-get supersonic-browse--page :id)) (plist-get supersonic-browse--page :item))))
      ('nil (user-error "Nothing here"))
      (kind (cons kind item)))))

(aio-defun
 supersonic-browse--track-ids (kind item)
 "Return a promise resolving to the track ids ITEM, a KIND, stands for."
 (pcase kind
   ('album (mapcar (lambda (track) (plist-get track :id))
                   (if (equal (plist-get item :id) (plist-get supersonic-browse--page :id))
                       (or (plist-get supersonic-browse--content :tracks)
                           (aio-await (supersonic-provider-album-tracks (plist-get item :id))))
                     (aio-await (supersonic-provider-album-tracks (plist-get item :id))))))
   ('artist
    (let* ((albums (aio-await (supersonic-provider-artist-albums (plist-get item :id))))
           (promises (mapcar (lambda (album) (supersonic-provider-album-tracks (plist-get album :id)))
                             (sort albums (lambda (a b) (< (or (plist-get a :year) 0) (or (plist-get b :year) 0))))))
           (ids nil))
      (dolist (promise promises)
        (setq ids (append ids (mapcar (lambda (track) (plist-get track :id)) (aio-await promise)))))
      ids))
   ('track (list (plist-get item :id)))))

(defun supersonic-browse--shuffled (list)
  "Return a shuffled copy of LIST."
  (let ((vector (vconcat list)))
    (cl-loop for i from (1- (length vector)) downto 1
             do (cl-rotatef (aref vector i) (aref vector (random (1+ i)))))
    (append vector nil)))

(defun supersonic-browse--with-ids (verb function)
  "Call FUNCTION with the track ids of the item in focus, saying VERB."
  (pcase-let ((`(,kind . ,item) (supersonic-browse--target)))
    (when (memq kind '(button more))
      (user-error "Not something to play"))
    (aio-listen (aio-catch (supersonic-browse--track-ids kind item))
                (lambda (value)
                  (let ((outcome (funcall value)))
                    (if (eq (car outcome) :error)
                        (message "[Supersonic] Failed: %s" (error-message-string (cdr outcome)))
                      (when (cdr outcome)
                        (funcall function (cdr outcome))
                        (message "%s %s" verb (or (plist-get item :name) (plist-get item :title) "")))))))))

(defun supersonic-browse-play ()
  "Play the item in focus: an album, an artist's every album, a track."
  (interactive)
  (if (eq (car-safe (get-text-property (point) 'supersonic-browse-key)) 'track)
      (supersonic-browse-open)
    (supersonic-browse--with-ids "Playing" #'supersonic-playback-start)))

(defun supersonic-browse-shuffle ()
  "Play the item in focus shuffled; for a track, the list it is in."
  (interactive)
  (if (eq (car-safe (get-text-property (point) 'supersonic-browse-key)) 'track)
      (supersonic-playback-start (supersonic-browse--shuffled (get-text-property (point) 'supersonic-browse-ids)))
    (supersonic-browse--with-ids "Shuffling" (lambda (ids) (supersonic-playback-start (supersonic-browse--shuffled ids))))))

(defun supersonic-browse-enqueue ()
  "Add the item in focus to the play queue."
  (interactive)
  (supersonic-browse--with-ids "Queued" #'supersonic-playback-enqueue))

(defun supersonic-browse-open ()
  "Open the item in focus, or play from the track in focus on."
  (interactive)
  (let ((key (get-text-property (point) 'supersonic-browse-key))
        (item (get-text-property (point) 'supersonic-browse-item)))
    (pcase (car-safe key)
      ('artist (supersonic-browse-visit (list :kind 'artist :id (plist-get item :id) :item item)))
      ('album (supersonic-browse-visit (list :kind 'album :id (plist-get item :id) :item item)))
      ('track (supersonic-playback-start (nthcdr (get-text-property (point) 'supersonic-browse-index)
                                                 (get-text-property (point) 'supersonic-browse-ids))))
      ((or 'button 'more) (funcall (plist-get item :action)))
      ('page (supersonic-browse-play))
      (_ (user-error "Nothing to open here")))))

(defun supersonic-browse-click (event)
  "Focus the item clicked with EVENT and open it."
  (interactive "e")
  (mouse-set-point event)
  (supersonic-browse--update-focus)
  (supersonic-browse-open))

(defun supersonic-browse-click-play (event)
  "Focus the item clicked with EVENT and play it."
  (interactive "e")
  (mouse-set-point event)
  (supersonic-browse--update-focus)
  (supersonic-browse-play))

(aio-defun
 supersonic-browse-surprise ()
 "Play a random album."
 (interactive)
 (let ((album (car (aio-await (supersonic-provider-album-list 'random 1)))))
   (unless album
     (user-error "No albums to pick from"))
   (supersonic-playback-start
    (mapcar (lambda (track) (plist-get track :id))
            (aio-await (supersonic-provider-album-tracks (plist-get album :id)))))
   (message "Surprise: %s%s" (plist-get album :name)
            (if (plist-get album :artist) (concat " by " (plist-get album :artist)) ""))))

;;;
;;; The mode
;;;

(defun supersonic-browse--track-changed ()
  "Move the playing marker in the browser, if it shows tracks."
  (let ((buff (get-buffer supersonic-browse-buffer-name)))
    (when (buffer-live-p buff)
      (aio-listen (aio-catch (supersonic-playback-status 'track-id))
                  (lambda (value)
                    (let ((outcome (funcall value)))
                      (when (and (eq (car outcome) :success) (buffer-live-p buff))
                        (with-current-buffer buff
                          (unless (equal (cdr outcome) supersonic-browse--playing)
                            (setq supersonic-browse--playing (cdr outcome))
                            (when (memq (plist-get supersonic-browse--page :kind) '(album search))
                              (supersonic-browse--render)))))))))))

(defun supersonic-browse--window-changed (window)
  "Lay the page out again if WINDOW, showing it, changed width."
  (with-current-buffer (window-buffer window)
    (when (and supersonic-browse--content
               (not (eql supersonic-browse--width (window-body-width window t))))
      (supersonic-browse--render))))

(defvar supersonic-browse-mode-map
  (let ((map (supersonic--define-common-keys (make-sparse-keymap))))
    (define-key map (kbd "RET") #'supersonic-browse-open)
    (define-key map (kbd "P") #'supersonic-browse-play)
    (define-key map (kbd "s") #'supersonic-browse-shuffle)
    (define-key map (kbd "a") #'supersonic-browse-enqueue)
    (define-key map (kbd "l") #'supersonic-browse-back)
    (define-key map (kbd "DEL") #'supersonic-browse-back)
    (define-key map (kbd "M-<left>") #'supersonic-browse-back)
    (define-key map (kbd "M-<right>") #'supersonic-browse-forward)
    (define-key map (kbd "r") #'supersonic-browse-forward)
    (define-key map (kbd "h") #'supersonic-browse-home)
    (define-key map (kbd "A") #'supersonic-browse-artists)
    (define-key map (kbd "/") #'supersonic-browse-search)
    (define-key map (kbd "g") #'supersonic-browse-reload)
    (define-key map (kbd "?") #'describe-mode)
    (define-key map (kbd "n") #'supersonic-browse-down)
    (define-key map (kbd "p") #'supersonic-browse-up)
    (define-key map (kbd "f") #'supersonic-browse-next)
    (define-key map (kbd "b") #'supersonic-browse-previous)
    (define-key map (kbd "<right>") #'supersonic-browse-next)
    (define-key map (kbd "<left>") #'supersonic-browse-previous)
    (define-key map (kbd "<down>") #'supersonic-browse-down)
    (define-key map (kbd "<up>") #'supersonic-browse-up)
    (define-key map (kbd "TAB") #'supersonic-browse-next-section)
    (define-key map (kbd "<backtab>") #'supersonic-browse-previous-section)
    (define-key map (kbd "<home>") #'supersonic-browse-first)
    (define-key map (kbd "<end>") #'supersonic-browse-last)
    (define-key map (kbd "<mouse-8>") #'supersonic-browse-back)
    (define-key map (kbd "<mouse-9>") #'supersonic-browse-forward)
    ;; Whatever moves the cursor moves between items instead -- C-n,
    ;; M-f, or a modal editing package's own motion commands.
    (dolist (remap '((next-line . supersonic-browse-down)
                     (previous-line . supersonic-browse-up)
                     (forward-char . supersonic-browse-next)
                     (backward-char . supersonic-browse-previous)
                     (right-char . supersonic-browse-next)
                     (left-char . supersonic-browse-previous)
                     (forward-word . supersonic-browse-next)
                     (backward-word . supersonic-browse-previous)
                     (forward-paragraph . supersonic-browse-next-section)
                     (backward-paragraph . supersonic-browse-previous-section)
                     (beginning-of-buffer . supersonic-browse-first)
                     (end-of-buffer . supersonic-browse-last)
                     ;; xah-fly-keys
                     (xah-beginning-of-line-or-block . supersonic-browse-previous-section)
                     (xah-end-of-line-or-block . supersonic-browse-next-section)
                     ;; evil and meow
                     (evil-next-line . supersonic-browse-down)
                     (evil-previous-line . supersonic-browse-up)
                     (evil-forward-char . supersonic-browse-next)
                     (evil-backward-char . supersonic-browse-previous)
                     (meow-next . supersonic-browse-down)
                     (meow-prev . supersonic-browse-up)
                     (meow-right . supersonic-browse-next)
                     (meow-left . supersonic-browse-previous)))
      (define-key map (vector 'remap (car remap)) (cdr remap)))
    map)
  "Keymap for `supersonic-browse-mode'.")

(define-derived-mode supersonic-browse-mode special-mode "Supersonic"
  "Major mode of the supersonic library browser, see `supersonic-browse'.
\\{supersonic-browse-mode-map}"
  (buffer-disable-undo)
  (setq-local truncate-lines t
              cursor-type nil
              line-spacing 0
              mouse-wheel-progressive-speed nil)
  ;; The current-line highlight would bar across tiles; focus is shown
  ;; by the browser itself.  `global-hl-line-mode' checks its variable
  ;; in the buffer it highlights, so a local nil opts this one out.
  (setq-local global-hl-line-mode nil)
  (when (bound-and-true-p hl-line-mode)
    (hl-line-mode -1))
  (add-hook 'post-command-hook #'supersonic-browse--post-command nil t)
  (add-hook 'window-size-change-functions #'supersonic-browse--window-changed nil t)
  (add-hook 'supersonic-playback-track-change-hook #'supersonic-browse--track-changed)
  (setq supersonic-browse--playing nil)
  (supersonic-browse--track-changed))

(provide 'supersonic-browse)

;;; supersonic-browse.el ends here
