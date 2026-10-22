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
;;   arrows, C-n C-p  move between items   RET  open / play from here
;;   TAB S-TAB    next / previous section  P    play
;;   l r          back / forward           s    shuffle
;;   h            home                     a    add to the queue
;;   /            search                   A    add to a playlist
;;   n p          next / previous track    f b  seek
;;   ?            every key, as a cheatsheet
;;
;; Whatever moves the cursor in your setup -- C-f, M-f, a modal
;; package's motions -- moves between items too.

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
(defvar-local supersonic-browse--more nil
  "Non-nil while more of an album list loads; `done' once it ran out.")
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
(defvar-local supersonic-browse--restore-row nil
  "Position of the track row to focus once the page is laid out again.")
(declare-function supersonic-download-tracks "supersonic-download")
(declare-function supersonic-local-album-in "supersonic-local")
(declare-function supersonic-local-toggle "supersonic-local")
(declare-function supersonic-download-albums "supersonic-download")
(declare-function supersonic-download-album-p "supersonic-download")
(declare-function supersonic-download-delete-album "supersonic-download")
(defvar supersonic-download--current)
(defvar supersonic-download--done)
(defvar supersonic-download--pending)
(defvar supersonic-browse-mode-map)
(defvar isearch-mode-end-hook-quit)
(defvar isearch-mode-end-hook-quit)

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
    (if (not words)
        ;; Nothing to take an initial from, as in a name of only signs.
        "♪"
      (upcase (apply #'string (mapcar (lambda (w) (aref (replace-regexp-in-string "[^[:alnum:]]" "" w) 0))
                                      (seq-take words 2)))))))

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
        (if (plist-get item :card)
            ;; A card (a genre, say): its whole name, wrapped, bottom left.
            (let* ((font (round (* size 0.13)))
                   (lines (supersonic-browse--wrap (plist-get item :name) (floor (/ (* size 0.84) (* font 0.58))) 3))
                   (top (- (+ y size) (* size 0.1) (* font 1.15 (1- (length lines))))))
              (cl-loop for line in lines for i from 0 do
                       (dom-append-child
                        svg (dom-node 'text `((x . ,(+ x (* size 0.08))) (y . ,(+ top (* i font 1.15)))
                                              (font-family . ,(supersonic-browse--font-family))
                                              (font-size . ,font) (font-weight . "bold")
                                              (fill . "#ffffff") (fill-opacity . "0.95"))
                                      line))))
          (dom-append-child
           svg (dom-node 'text `((x . ,(+ x (/ size 2.0))) (y . ,(+ y (* size 0.5)))
                                 (dy . "0.35em") (text-anchor . "middle")
                                 (font-family . ,(supersonic-browse--font-family))
                                 (font-size . ,(round (* size 0.34))) (font-weight . "bold")
                                 (fill . "#ffffff") (fill-opacity . "0.92"))
                         (supersonic-browse--initials (or (plist-get item :name) (plist-get item :title))))))))))

(defun supersonic-browse--wrap (text width lines)
  "Return TEXT broken into at most LINES lines of WIDTH characters.
The last line is cut with an ellipsis if TEXT does not fit."
  (let ((words (split-string (or text "") "[[:space:]]+" t))
        (result nil)
        (line ""))
    (while words
      (let ((candidate (if (string-empty-p line) (car words) (concat line " " (car words)))))
        (if (or (<= (length candidate) width) (string-empty-p line))
            (setq line candidate
                  words (cdr words))
          (push line result)
          (setq line ""))))
    (push line result)
    (setq result (nreverse (delete "" result)))
    (if (<= (length result) lines)
        (mapcar (lambda (l) (truncate-string-to-width l width nil nil "…")) result)
      (append (seq-take result (1- lines))
              (list (truncate-string-to-width (string-join (nthcdr (1- lines) result) " ") width nil nil "…"))))))

(defun supersonic-browse--tile-image (item shape selected)
  "Return the display spec of ITEM's tile, a SHAPE, glowing if SELECTED.
The art is the same size either way, so nothing moves as focus does;
the tile in focus glows in the accent colour around its edges."
  (let* ((size supersonic-browse-tile-size)
         (file (supersonic-browse--art-file item (* 2 size)))
         (saved (supersonic-browse--saved-p item))
         (cache-key (list (plist-get item :id) shape selected file size saved))
         (cached (and supersonic-browse--images (gethash cache-key supersonic-browse--images))))
    (or cached
        (let* ((svg (svg-create size size))
               ;; Room around the art for the glow to spread into.
               (inset (round (* size 0.06)))
               (inner (- size (* 2 inset)))
               (accent (supersonic-browse--accent)))
          (when selected
            (let ((filter (dom-node 'filter '((id . "glow") (x . "-50%") (y . "-50%") (width . "200%") (height . "200%")))))
              (dom-append-child filter (dom-node 'feGaussianBlur `((stdDeviation . ,(max 3 (round (* size 0.028)))))))
              (dom-append-child svg filter)
              ;; A blurred accent copy of the shape, a little larger than the art.
              (let* ((spread (round (* size 0.025)))
                     (halo (supersonic-browse--shape-node shape (+ inner (* 2 spread)) 0
                                                          `(fill . ,accent) '(fill-opacity . "1")
                                                          '(filter . "url(#glow)"))))
                (dom-set-attribute halo 'transform (format "translate(%s,%s)" (- inset spread) (- inset spread)))
                (dom-append-child svg halo))))
          (supersonic-browse--art-svg svg item inner shape file inset inset)
          (let ((outline (supersonic-browse--shape-node
                          shape inner (if selected 0.75 0.5) '(fill . "none")
                          `(stroke . ,(if selected accent (supersonic-browse--fg)))
                          `(stroke-opacity . ,(if selected "0.9" "0.12"))
                          `(stroke-width . ,(if selected 1.5 1)))))
            (dom-set-attribute outline 'transform (format "translate(%s,%s)" inset inset))
            (dom-append-child svg outline))
          (when saved
            ;; A check mark in a disc, bottom right: saved for offline.
            (let* ((r (* size 0.075))
                   (cx (- (+ inset inner) r (* size 0.04)))
                   (cy cx))
              (dom-append-child svg (dom-node 'circle `((cx . ,cx) (cy . ,cy) (r . ,r) (fill . ,accent)
                                                        (stroke . "#000000") (stroke-opacity . "0.35"))))
              (dom-append-child svg (dom-node 'path `((d . ,(format "M%s %s L%s %s L%s %s"
                                                                     (- cx (* r 0.45)) cy
                                                                     (- cx (* r 0.1)) (+ cy (* r 0.38))
                                                                     (+ cx (* r 0.5)) (- cy (* r 0.4))))
                                                      (fill . "none") (stroke . "#ffffff")
                                                      (stroke-width . ,(max 1.5 (* r 0.22)))
                                                      (stroke-linecap . "round") (stroke-linejoin . "round"))))))
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

(defun supersonic-browse--box-width (face)
  "Return the pixels FACE's box adds left and right of its text together."
  (let* ((box (face-attribute face :box nil t))
         (width (cond ((integerp box) box)
                      ((consp box) (plist-get box :line-width)))))
    (* 2 (abs (cond ((consp width) (car width))
                    ((integerp width) width)
                    (t 0))))))

(defun supersonic-browse--heading (text &optional more-label more-action)
  "Insert section heading TEXT, with a MORE-LABEL button calling MORE-ACTION."
  (supersonic-browse--new-region t)
  (insert "\n" (supersonic-browse--space supersonic-browse-margin)
          (propertize text 'face 'supersonic-browse-heading 'supersonic-browse-section t))
  (when more-label
    ;; The link's padding box is part of its width, which
    ;; `string-pixel-width' leaves out.
    (let ((label (propertize (concat more-label " ›") 'face 'supersonic-browse-link)))
      (insert (supersonic-browse--align (+ supersonic-browse-margin
                                           (- (supersonic-browse--available-width)
                                              (supersonic--string-pixel-width label)
                                              (supersonic-browse--box-width 'supersonic-browse-link))))
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

(defun supersonic-browse--saved-p (item)
  "Return non-nil if ITEM is an album saved for offline use."
  (and (fboundp 'supersonic-download-album-p) (supersonic-download-album-p (plist-get item :id))))

(defun supersonic-browse--subtitle (item)
  "Return what the line under ITEM's tile says.
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
   ((plist-get item :song-count) (supersonic-browse--plural (plist-get item :song-count) "song"))
   (t "")))

(defun supersonic-browse--row-strut ()
  "Return an invisible stretch giving a track row its height, its text centred.
`line-height' would put all the room above the text.  Centred is
where the letters look centred: half a capital's height above the
middle of the row."
  (let* ((height (round (* 1.75 (frame-char-height))))
         (info (ignore-errors (font-info (face-font 'default))))
         (pixels (if info (aref info 2) (* 0.8 (frame-char-height))))
         ;; Letters, not the font's whole box (room for accents and
         ;; descenders included), are what should look centred.
         (above (+ (/ height 2.0) (* 0.34 pixels))))
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
         (numbers (if show-album
                      (number-sequence 1 (length tracks))
                    (supersonic-browse--numbering tracks)))
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
     (let* ((playing (if (eq (plist-get supersonic-browse--page :kind) 'queue)
                         (plist-get track :queue-current)
                       (and supersonic-browse--playing (equal (plist-get track :id) supersonic-browse--playing))))
            (number (propertize (number-to-string number)
                                'face (if playing 'supersonic-browse-playing 'supersonic-browse-track-number)))
            (duration (supersonic-browse--duration (plist-get track :duration)))
            (duration-x (- right (supersonic--string-pixel-width duration)))
            (detail (string-join
                     (delq nil (list (and (or show-album (not (equal (plist-get track :artist) album-artist)))
                                          ;; Not on the artist's own page.
                                          (not (eq (plist-get supersonic-browse--page :kind) 'artist))
                                          (plist-get track :artist))
                                     (and show-album (plist-get track :album))))
                     " · "))
            (title (concat (propertize (or (plist-get track :title) "?")
                                       'face (cond (playing 'supersonic-browse-playing)
                                                   ;; Queue entries already played.
                                                   ((plist-get track :queue-played) 'shadow)
                                                   (t 'supersonic-browse-track)))
                           (and (plist-get track :starred) (propertize "  ♥" 'face 'supersonic-browse-accent))))
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

(defconst supersonic-browse--hero-buttons
  '((supersonic-browse-play play "Play" t)
    (supersonic-browse-shuffle shuffle "Shuffle" nil)
    (supersonic-browse-enqueue plus "Queue" nil))
  "The buttons of an artist or album page: (COMMAND ICON LABEL PRIMARY).")

(defconst supersonic-browse--button-height 44 "Height of a header button, in pixels.")

(defun supersonic-browse--button-image (spec height selected)
  "Return the display spec of header button SPEC, HEIGHT high, ringed if SELECTED.
SPEC is one of `supersonic-browse--hero-buttons'."
  (pcase-let* ((`(,_command ,icon ,label ,primary) spec)
               (h supersonic-browse--button-height)
               (w (+ 58 (* (length label) 11)))
               (pad 5)
               (svg (svg-create (+ w (* 2 pad)) height))
               (y (- height h pad))
               (accent (supersonic-browse--accent))
               (ink (if primary (supersonic-browse--bg) (supersonic-browse--fg))))
    (when selected
      (dom-append-child svg (dom-node 'rect `((x . 1.5) (y . ,(- y 3.5)) (width . ,(+ w 7)) (height . ,(+ h 7))
                                              (rx . ,(/ (+ h 7) 2.0)) (fill . "none")
                                              (stroke . ,accent) (stroke-width . 3)))))
    (dom-append-child svg (dom-node 'rect `((x . ,pad) (y . ,y) (width . ,w) (height . ,h) (rx . ,(/ h 2))
                                            (fill . ,(cond (primary accent)
                                                           (selected (supersonic-browse--blend accent (supersonic-browse--bg) 0.25))
                                                           (t "none")))
                                            (stroke . ,(if primary "none" (supersonic-browse--subtle)))
                                            (stroke-width . 1.5))))
    (dom-append-child svg (supersonic-browse--icon icon (+ pad 22) (+ y (/ h 2)) ink))
    (dom-append-child svg (dom-node 'text `((x . ,(+ pad 38)) (y . ,(+ y (/ h 2))) (dy . "0.35em")
                                            (font-family . ,(supersonic-browse--font-family)) (font-size . 17)
                                            (font-weight . "bold") (fill . ,ink))
                                    label))
    (svg-image svg :ascent 'center :scale 1)))

(defun supersonic-browse--hero (item shape kicker title subtitle meta)
  "Insert the header of a page about ITEM, its art drawn as SHAPE.
KICKER, TITLE, SUBTITLE and META are the lines beside it.  Play,
shuffle and queue buttons sit along the foot of the art, each an item
of its own, so they take focus like any other."
  (supersonic-browse--new-region)
  (let ((graphic (supersonic-browse--graphic-p))
        (region supersonic-browse--region))
    (if (not graphic)
        (progn
          (insert (supersonic-browse--space supersonic-browse-margin)
                  (propertize title 'face 'supersonic-browse-greeting) "\n"
                  (supersonic-browse--space supersonic-browse-margin) (or subtitle "") "  " (or meta "") "\n"
                  (supersonic-browse--space supersonic-browse-margin))
          (dolist (spec supersonic-browse--hero-buttons)
            (insert (apply #'propertize (concat " " (nth 2 spec) " ")
                           'face 'supersonic-browse-button
                           (supersonic-browse--item-properties
                            (list 'hero (car spec)) item 'supersonic-browse-focus t
                            'supersonic-browse-region region))
                    "  "))
          (insert "\n"))
      (let* ((size supersonic-browse-hero-size)
             (width (max (+ size 200) (supersonic-browse--available-width)))
             (file (supersonic-browse--art-file item supersonic-now-playing-art-size))
             (svg (svg-create width size))
             (family (supersonic-browse--font-family))
             (x (+ size 40))
             (title-size (min 56 (max 28 (round (* size 0.2)))))
             (title (truncate-string-to-width title (max 4 (floor (/ (- width x) (* 0.56 title-size)))) nil nil "…"))
             (foot (+ supersonic-browse--button-height 10))
             (top (- size foot))
             ;; Bottom-up: the text stacked above the line of buttons.
             (meta-y (- top 18))
             (subtitle-y (- meta-y (if meta 34 0)))
             (title-y (- subtitle-y (if subtitle 42 0)))
             (kicker-y (- title-y title-size 6)))
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
        ;; Centred on their lines, the two slices fill them exactly;
        ;; sitting on the baseline, each line would add the font's
        ;; descent below its slice, a seam across the art.
        (let ((image (apply #'svg-image svg :ascent 'center :scale 1 (and file (list :base-uri file))))
              (properties (list 'supersonic-browse-item item 'supersonic-browse-hero t)))
          ;; The header is one image cut in two: the top beside the
          ;; text, and the foot of the art, which the buttons follow.
          (insert (propertize "\n" 'line-height 0.5) (supersonic-browse--space supersonic-browse-margin))
          (insert (apply #'propertize "*" 'display `((slice 0 0 ,width ,top) ,image) properties) "\n")
          (insert (supersonic-browse--space supersonic-browse-margin))
          (insert (apply #'propertize "*" 'display `((slice 0 ,top ,x ,foot) ,image) properties))
          (dolist (spec supersonic-browse--hero-buttons)
            (insert (apply #'propertize "*"
                           'display (supersonic-browse--button-image spec foot nil)
                           'supersonic-browse-button spec
                           'supersonic-browse-button-height foot
                           (supersonic-browse--item-properties
                            (list 'hero (car spec)) item 'supersonic-browse-focus t
                            'supersonic-browse-region region 'help-echo (nth 2 spec)))
                    (supersonic-browse--space 6)))
          (insert "\n"))))))

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
    ('playlists "Playlists")
    ('starred "Favourites")
    ('queue "Queue")
    ('downloads "Downloads")
    ('genres "Genres")
    ('genre (plist-get page :id))
    ('playlist (or (plist-get (plist-get page :item) :name) "Playlist"))
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
      (list :sections (nreverse sections)
            :genres (and (supersonic-provider-supports-p 'genres)
                         (let ((outcome (aio-await (aio-catch (supersonic-provider-genres)))))
                           (and (eq (car outcome) :success)
                                (supersonic-browse--genre-cards (seq-take (cdr outcome) supersonic-browse-section-count)))))
            :playlists (and (supersonic-provider-supports-p 'playlists)
                            (let ((outcome (aio-await (aio-catch (supersonic-provider-playlists)))))
                              (and (eq (car outcome) :success) (cdr outcome)))))))
   ('artists (list :artists (aio-await (supersonic-provider-artists))))
   ('playlists (list :playlists (aio-await (supersonic-provider-playlists))))
   ('starred (list :starred (aio-await (supersonic-provider-starred))))
   ('downloads
    (let ((albums nil))
      ;; Played from the local files, the saved albums are those files'.
      (dolist (album (supersonic-download-albums))
        (let ((local (and (eq supersonic-provider 'local)
                          (aio-await (supersonic-local-album-in (plist-get album :download-dir))))))
          (push (if local (append (list :download-dir (plist-get album :download-dir)) local) album) albums)))
      (list :albums (nreverse albums))))
   ('queue
    (let ((pending (mapcar (lambda (entry)
                             (list entry
                                   (and (not (plist-get entry :track)) (plist-get entry :track-id)
                                        (aio-catch (supersonic-provider-track (plist-get entry :track-id))))))
                           (aio-await (supersonic-playback-queue))))
          tracks)
      ;; `dolist', so that `aio-await' stays where generator.el transforms it.
      (dolist (item pending)
        (let* ((entry (car item))
               (outcome (and (cadr item) (aio-await (cadr item))))
               (track (or (plist-get entry :track)
                          (and (eq (car outcome) :success) (cdr outcome))
                          (list :id (plist-get entry :track-id) :title "?"))))
          (push (append (list :queue-current (plist-get entry :current)) track) tracks)))
      (list :tracks (nreverse tracks))))
   ('genres (list :genres (supersonic-browse--genre-cards (aio-await (supersonic-provider-genres)))))
   ('genre (list :albums (aio-await (supersonic-provider-genre-albums (plist-get page :id) supersonic-album-list-count))))
   ('playlist (list :tracks (aio-await (supersonic-provider-playlist-tracks (plist-get page :id)))))
   ('albums (list :albums (aio-await (supersonic-provider-album-list (plist-get page :type) supersonic-album-list-count))))
   ('artist
    (let ((top (and (supersonic-provider-supports-p 'top-tracks)
                    (aio-catch (supersonic-provider-top-tracks
                                (list :id (plist-get page :id) :name (plist-get (plist-get page :item) :name))
                                supersonic-browse-top-count)))))
      (list :albums (aio-await (supersonic-provider-artist-albums (plist-get page :id)))
            :top (and top (let ((outcome (aio-await top)))
                            (and (eq (car outcome) :success) (cdr outcome)))))))
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
    (let* ((trail (supersonic--string-pixel-width (buffer-substring (line-beginning-position) (point))))
           ;; Room to spare after the trail: space aligned to the left of
           ;; where it ends has no width, and the two would run together.
           (room (- (+ supersonic-browse-margin (supersonic-browse--available-width)) trail 48))
           (hints (supersonic-browse-hint-line (supersonic-browse--hints) supersonic-browse-mode-map room)))
      (when (<= (supersonic--string-pixel-width hints) room)
        (insert (supersonic-browse--align (- (+ supersonic-browse-margin (supersonic-browse--available-width))
                                             (supersonic--string-pixel-width hints)))
                hints)))
    (insert (propertize "\n" 'line-height 1.8))))

(defun supersonic-browse-hint-line (hints map width)
  "Return HINTS for keys in MAP as a line at most WIDTH pixels wide.
HINTS are (COMMANDS . LABEL), most important first; COMMANDS is a
command or a list of them, whose keys show joined by \"/\".  Hints
without a key are left out, and the last one is always kept, the
others in order while they fit."
  (let* ((sep "   ")
         (items
          (delq nil
                (mapcar
                 (lambda (hint)
                   (let ((keys (delq nil (mapcar (lambda (command)
                                                   (let ((key (where-is-internal command map t)))
                                                     (and key (key-description key))))
                                                 (ensure-list (car hint))))))
                     (and keys
                          (concat (propertize (string-join keys "/") 'face 'supersonic-browse-hint-key)
                                  " " (propertize (cdr hint) 'face 'supersonic-browse-hint)))))
                 hints)))
         (last (car (last items)))
         (line "")
         (full nil))
    (dolist (item (butlast items))
      (let ((longer (concat line item sep)))
        (if (or full (> (supersonic--string-pixel-width (concat longer last)) width))
            (setq full t)
          (setq line longer))))
    (concat line (or last ""))))

(defun supersonic-browse--hints ()
  "Return the hints for the current page: its own keys first."
  (append
   (pcase (plist-get supersonic-browse--page :kind)
     ('queue '((supersonic-browse-open . "play") (supersonic-browse-playlist-remove . "remove")
               ((supersonic-browse-playlist-move-up supersonic-browse-playlist-move-down) . "move")
               (supersonic-clear-queue . "clear")))
     ('playlist '((supersonic-browse-open . "open") (supersonic-browse-play . "play")
                  (supersonic-browse-playlist-remove . "remove")
                  ((supersonic-browse-playlist-move-up supersonic-browse-playlist-move-down) . "move")
                  (supersonic-browse-playlist-rename . "rename") (supersonic-browse-delete . "delete")))
     ('downloads '((supersonic-browse-open . "open") (supersonic-browse-play . "play")
                   (supersonic-browse-delete . "delete")))
     (_ '((supersonic-browse-open . "open") (supersonic-browse-play . "play")
          (supersonic-browse-shuffle . "shuffle") (supersonic-browse-enqueue . "queue")
          (supersonic-browse-download . "save") (supersonic-browse-add-to-playlist . "playlist")
          (supersonic-browse-next-section . "section"))))
   '((supersonic-browse-back . "back") (supersonic-browse-search . "search")
     (supersonic-cheatsheet . "all keys"))))

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
                    ("Playlists" . supersonic-browse-playlists)
                    ("♥ Favourites" . supersonic-browse-favourites)
                    ("Genres" . supersonic-browse-genres)
                    ("Search" . supersonic-browse-search)))
    (insert (apply #'propertize (concat " " (car button) " ")
                   'face 'supersonic-browse-button
                   (supersonic-browse--item-properties (list 'button (cdr button)) (list :action (cdr button))
                                                       'supersonic-browse-focus t
                                                       'supersonic-browse-region supersonic-browse--region))
            "  "))
  (insert "\n")
  (let ((sections (plist-get content :sections))
        (playlists (plist-get content :playlists))
        (first t))
    (if (not (or sections playlists))
        (insert "\n" (supersonic-browse--space supersonic-browse-margin)
                (propertize "Nothing here yet -- try Artists or Search." 'face 'shadow) "\n")
      (dolist (section sections)
        (let ((type (car (car section))))
          (supersonic-browse--heading (cdr (car section)) "See all"
                                      (lambda () (supersonic-browse-album-list type)))
          (supersonic-browse--insert-grid 'album (cdr section) 'square 1))
        ;; Playlists second, right after jumping back in.
        (when (and first playlists)
          (supersonic-browse--heading "Your playlists" "See all" #'supersonic-browse-playlists)
          (supersonic-browse--insert-grid 'playlist playlists 'square 1))
        (setq first nil))
      (when (and playlists (not sections))
        (supersonic-browse--heading "Your playlists" "See all" #'supersonic-browse-playlists)
        (supersonic-browse--insert-grid 'playlist playlists 'square 1))
      ;; Server ids: under the local provider the downloads page maps them.
      (let ((saved (and (fboundp 'supersonic-download-albums) (not (eq supersonic-provider 'local))
                        (supersonic-download-albums))))
        (when saved
          (supersonic-browse--heading "Saved for offline" "See all" #'supersonic-browse-downloads)
          (supersonic-browse--insert-grid 'album saved 'square 1)))
      (when (plist-get content :genres)
        (supersonic-browse--heading "Genres" "See all" #'supersonic-browse-genres)
        (supersonic-browse--insert-grid 'genre (plist-get content :genres) 'square 1)))))

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
          'circle (if (plist-get item :starred) "Artist · ♥ favourite" "Artist") (or (plist-get item :name) "")
          (supersonic-browse--plural (length albums) "album")
          (and (> songs 0) (supersonic-browse--plural songs "song")))
         (when (plist-get content :top)
           (supersonic-browse--heading "Popular")
           (supersonic-browse--insert-tracks (plist-get content :top) t))
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
          'square (if (plist-get item :starred) "Album · ♥ favourite" "Album")
          (or (plist-get item :name) (plist-get first :album) "")
          artist
          (string-join (delq nil (list (and year (number-to-string year))
                                       (supersonic-browse--plural (length tracks) "song")
                                       (supersonic-browse--total total)))
                       " · "))
         (insert (propertize "\n" 'line-height 1.4))
         (supersonic-browse--insert-tracks tracks)))
      ('downloads
       (let ((albums (plist-get content :albums))
             (current (bound-and-true-p supersonic-download--current)))
         (supersonic-browse--title "Downloads" (supersonic-browse--plural (length albums) "album"))
         (when current
           (insert (supersonic-browse--space supersonic-browse-margin)
                   (propertize (format "Saving “%s” · %d done, %d to go"
                                       (or (plist-get current :title) "?") supersonic-download--done
                                       (1+ (length supersonic-download--pending)))
                               'face 'supersonic-browse-hint)
                   "\n"))
         (insert "\n")
         (if albums
             (supersonic-browse--insert-grid 'album albums 'square)
           (insert (supersonic-browse--space supersonic-browse-margin)
                   (propertize (substitute-command-keys
                                "Nothing saved yet: \\<supersonic-browse-mode-map>\\[supersonic-browse-download] on an album saves it for offline.")
                               'face 'shadow)
                   "\n"))))
      ('queue
       (let* ((tracks (plist-get content :tracks))
              (current (seq-position tracks t (lambda (track _) (plist-get track :queue-current))))
              (left (and current (apply #'+ (mapcar (lambda (track) (or (plist-get track :duration) 0))
                                                    (nthcdr current tracks))))))
         ;; Opened, the page shows the song playing, past those played.
         (when (and current (not supersonic-browse--selected) (not supersonic-browse--restore-row))
           (setq supersonic-browse--restore-row current))
         (cl-loop for track in tracks for i below (or current 0)
                  do (plist-put track :queue-played t))
         (supersonic-browse--title "Queue"
                                   (string-join (delq nil (list (supersonic-browse--plural (length tracks) "song")
                                                                (and left (concat (supersonic-browse--total left) " left"))))
                                                " · "))
         (if tracks
             (progn (insert "\n") (supersonic-browse--insert-tracks tracks t))
           (insert "\n" (supersonic-browse--space supersonic-browse-margin)
                   (propertize "Nothing queued." 'face 'shadow) "\n"))))
      ('starred
       (let* ((starred (plist-get content :starred))
              (tracks (plist-get starred :tracks)))
         (supersonic-browse--title "Favourites")
         (unless (or tracks (plist-get starred :albums) (plist-get starred :artists))
           (insert "\n" (supersonic-browse--space supersonic-browse-margin)
                   (propertize (substitute-command-keys
                                "None yet: \\<supersonic-browse-mode-map>\\[supersonic-browse-star] on a song, album or artist hearts it.")
                               'face 'shadow)
                   "\n"))
         (when tracks
           (supersonic-browse--heading "Songs")
           (supersonic-browse--insert-tracks tracks t))
         (when (plist-get starred :albums)
           (supersonic-browse--heading "Albums")
           (supersonic-browse--insert-grid 'album (plist-get starred :albums) 'square))
         (when (plist-get starred :artists)
           (supersonic-browse--heading "Artists")
           (supersonic-browse--insert-grid 'artist (plist-get starred :artists) 'circle))))
      ('genres
       (let ((genres (plist-get content :genres)))
         (supersonic-browse--title "Genres" (supersonic-browse--plural (length genres) "genre"))
         (insert "\n")
         (supersonic-browse--insert-grid 'genre genres 'square)))
      ('genre
       (let* ((albums (plist-get content :albums))
              (item (plist-get page :item)))
         (supersonic-browse--hero
          item 'square "Genre" (plist-get item :name)
          (and (plist-get item :album-count) (supersonic-browse--plural (plist-get item :album-count) "album"))
          (and (plist-get item :song-count) (supersonic-browse--plural (plist-get item :song-count) "song")))
         (supersonic-browse--heading "Albums")
         (supersonic-browse--insert-grid 'album albums 'square)))
      ('playlists
       (let ((playlists (plist-get content :playlists)))
         (supersonic-browse--title "Playlists" (supersonic-browse--plural (length playlists) "playlist"))
         (insert "\n")
         (if playlists
             (supersonic-browse--insert-grid 'playlist playlists 'square)
           (insert (supersonic-browse--space supersonic-browse-margin)
                   (propertize (substitute-command-keys
                                "None yet: \\<supersonic-browse-mode-map>\\[supersonic-browse-add-to-playlist] on a song, album or artist starts one.")
                               'face 'shadow)
                   "\n"))))
      ('playlist
       (let* ((tracks (plist-get content :tracks))
              (item (plist-get page :item))
              (total (apply #'+ (mapcar (lambda (track) (or (plist-get track :duration) 0)) tracks))))
         (supersonic-browse--hero
          (if (plist-get item :art) item (append (list :art (plist-get (car tracks) :art)) item))
          'square "Playlist" (or (plist-get item :name) "")
          (plist-get item :owner)
          (string-join (list (supersonic-browse--plural (length tracks) "song") (supersonic-browse--total total))
                       " · "))
         (insert (propertize "\n" 'line-height 1.4))
         (if tracks
             (supersonic-browse--insert-tracks tracks t)
           (insert (supersonic-browse--space supersonic-browse-margin)
                   (propertize "Empty for now." 'face 'shadow) "\n"))))
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
    (when supersonic-browse--restore-row
      (let ((row supersonic-browse--restore-row))
        (setq supersonic-browse--restore-row nil)
        (dotimes (i (supersonic-browse--stop-count))
          (when (and (eq (car-safe (supersonic-browse--prop i 'supersonic-browse-key)) 'track)
                     (eql (supersonic-browse--prop i 'supersonic-browse-index) row))
            (setq selected (supersonic-browse--prop i 'supersonic-browse-key))))))
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
                        ((get-text-property pos 'supersonic-browse-hero) supersonic-now-playing-art-size))))
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
         ((get-text-property pos 'supersonic-browse-hero)
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

(defun supersonic-browse--reveal-top (window)
  "Scroll WINDOW to the page's very top when that still shows point.
Moving up to the first items otherwise leaves the title cut off."
  (when (and window (> (window-start window) (point-min)))
    (let ((need (cdr (window-text-pixel-size
                      window (point-min) (save-excursion (forward-line 3) (point))))))
      (when (<= need (window-body-height window t))
        (set-window-start window (point-min))
        (set-window-vscroll window 0 t)))))

(defun supersonic-browse--goto-index (i)
  "Focus the item at stop I, if there is one."
  (when (and i (>= i 0) (< i (supersonic-browse--stop-count)))
    (goto-char (aref supersonic-browse--stops i))
    (supersonic-browse--update-focus)
    (supersonic-browse--reveal-top (get-buffer-window (current-buffer)))
    t))

(defun supersonic-browse--blend (a b amount)
  "Return colour A mixed into colour B by AMOUNT, 0 to 1."
  (let ((ra (color-name-to-rgb a)) (rb (color-name-to-rgb b)))
    (if (and ra rb)
        (apply #'color-rgb-to-hex
               (append (cl-mapcar (lambda (x y) (+ (* amount x) (* (- 1 amount) y))) ra rb) '(2)))
      b)))

(defmacro supersonic-browse--keeping-scroll (window &rest body)
  "Run BODY, then put WINDOW's scroll back as it was, line and pixels.
Changing a display property can make redisplay forget a partial
scroll, which with a tall image in view is like being stuck."
  (declare (indent 1))
  (let ((w (make-symbol "window")) (start (make-symbol "start")) (vscroll (make-symbol "vscroll")))
    `(let* ((,w ,window)
            (,start (and (window-live-p ,w) (window-start ,w)))
            (,vscroll (and (window-live-p ,w) (window-vscroll ,w t))))
       (prog1 (progn ,@body)
         (when (and ,start (window-live-p ,w) (eq (window-buffer ,w) (current-buffer)))
           (set-window-start ,w (min ,start (point-max)) t)
           (set-window-vscroll ,w ,vscroll t))))))

(defun supersonic-browse--paint (pos on)
  "Show the item taking focus at POS as in focus if ON, else as not."
  (let ((inhibit-read-only t)
        (key (get-text-property pos 'supersonic-browse-key))
        (shape (get-text-property pos 'supersonic-browse-tile)))
    (when shape
      (put-text-property pos (1+ pos) 'display
                         (supersonic-browse--tile-image (get-text-property pos 'supersonic-browse-item) shape on)))
    (when (get-text-property pos 'supersonic-browse-image-function)
      ;; An item drawing its own image, in or out of focus.
      (put-text-property pos (1+ pos) 'display
                         (funcall (get-text-property pos 'supersonic-browse-image-function) on)))
    (when (get-text-property pos 'supersonic-browse-button)
      (put-text-property pos (1+ pos) 'display
                         (supersonic-browse--button-image (get-text-property pos 'supersonic-browse-button)
                                                          (get-text-property pos 'supersonic-browse-button-height) on)))
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
         ((and (memq (car-safe key) '(button more))
               (not (get-text-property pos 'supersonic-browse-image-function)))
          ;; A button or link lights up as a whole.
          (overlay-put supersonic-browse--title-overlay 'face
                       `(:background ,(supersonic-browse--accent) :foreground ,(supersonic-browse--bg)
                                     :box (:line-width (14 . 7) :color ,(supersonic-browse--accent))))
          (move-overlay supersonic-browse--title-overlay pos
                        (or (next-single-property-change pos 'supersonic-browse-key) (point-max))))
         (t (delete-overlay supersonic-browse--title-overlay)))
        (if (or (eq (car-safe key) 'track) (get-text-property pos 'supersonic-browse-list-row))
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
  "Show which item is in focus: glowing if a tile, highlighted if a row."
  (let ((key (get-text-property (point) 'supersonic-browse-key)))
    (unless (equal key supersonic-browse--selected)
     (supersonic-browse--keeping-scroll (get-buffer-window (current-buffer))
      (let ((old (supersonic-browse--key-index supersonic-browse--selected))
            (new (supersonic-browse--key-index key)))
        (when old
          (supersonic-browse--paint (aref supersonic-browse--stops old) nil))
        (setq supersonic-browse--selected key)
        (when new
          (supersonic-browse--paint (aref supersonic-browse--stops new) t)))))))

(defun supersonic-browse--post-command ()
  "Keep the focus shown where point is, after every command."
  (when (and (derived-mode-p 'supersonic-browse-mode)
             ;; After a scroll, point is wherever Emacs had to put it to
             ;; keep it in view; moving it to an item off screen would
             ;; scroll right back.
             (not (supersonic-browse--scroll-command-p this-command))
             (not (get-text-property (point) 'supersonic-browse-focus))
             (get-text-property (point) 'supersonic-browse-key))
    ;; Clicked or moved onto a name: the tile it names takes focus.
    (let* ((i (supersonic-browse--key-index (get-text-property (point) 'supersonic-browse-key)))
           (pos (and i (aref supersonic-browse--stops i))))
      (when (and pos (pos-visible-in-window-p pos))
        (goto-char pos))))
  (supersonic-browse--update-focus)
  (supersonic-browse--maybe-more))

(defun supersonic-browse--maybe-more ()
  "Load more of an album list once focus nears its end, and add them."
  (when (and (eq (plist-get supersonic-browse--page :kind) 'albums)
             supersonic-browse--content
             (not supersonic-browse--more)
             (let ((i (supersonic-browse--key-index supersonic-browse--selected)))
               ;; About two rows of tiles before the end.
               (and i (>= i (- (supersonic-browse--stop-count) 12)))))
    (let* ((buff (current-buffer))
           (generation supersonic-browse--generation)
           (albums (plist-get supersonic-browse--content :albums))
           (count supersonic-album-list-count))
      (setq supersonic-browse--more t)
      (aio-listen
       (aio-catch (supersonic-provider-album-list (plist-get supersonic-browse--page :type) count (length albums)))
       (lambda (value)
         (let ((outcome (funcall value)))
           (when (and (buffer-live-p buff) (= generation (buffer-local-value 'supersonic-browse--generation buff)))
             (with-current-buffer buff
               (let* ((ids (mapcar (lambda (album) (plist-get album :id)) albums))
                      (new (and (eq (car outcome) :success)
                                ;; A random list may come up with some again.
                                (seq-remove (lambda (album) (member (plist-get album :id) ids)) (cdr outcome)))))
                 (setq supersonic-browse--more (and (or (null new) (< (length (cdr outcome)) count)) 'done))
                 (when new
                   (setq supersonic-browse--content
                         (plist-put (copy-sequence supersonic-browse--content) :albums (append albums new)))
                   (supersonic-browse--render)))))))))))

(defun supersonic-browse--isearch-done ()
  "Focus the item a search ended on, scrolling to it if need be."
  (unless isearch-mode-end-hook-quit
    (let ((i (supersonic-browse--key-index (get-text-property (point) 'supersonic-browse-key))))
      (when i
        (goto-char (aref supersonic-browse--stops i))
        (supersonic-browse--update-focus)))))

(defun supersonic-browse--scroll-command-p (command)
  "Return non-nil if COMMAND scrolls rather than moves."
  (and (symbolp command)
       (or (memq command '(mwheel-scroll scroll-up-command scroll-down-command scroll-up scroll-down
                                         scroll-other-window recenter-top-bottom))
           (string-match-p "scroll" (symbol-name command)))))

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

(defun supersonic-browse--line-range (i)
  "Return (FIRST . LAST), the stops on the same line as stop I."
  (let* ((n (supersonic-browse--stop-count))
         (pos (aref supersonic-browse--stops i))
         (start (save-excursion (goto-char pos) (line-beginning-position)))
         (end (save-excursion (goto-char pos) (line-end-position)))
         (first i) (last i))
    (while (and (> first 0) (>= (aref supersonic-browse--stops (1- first)) start))
      (setq first (1- first)))
    (while (and (< last (1- n)) (<= (aref supersonic-browse--stops (1+ last)) end))
      (setq last (1+ last)))
    (cons first last)))

(defun supersonic-browse--region-last (region)
  "Return the last stop of REGION."
  (let ((last nil))
    (dotimes (i (supersonic-browse--stop-count))
      (when (eql (supersonic-browse--prop i 'supersonic-browse-region) region)
        (setq last i)))
    last))

(defun supersonic-browse-row-start ()
  "Focus the first item of this line; there already, the first of the section."
  (interactive)
  (when (> (supersonic-browse--stop-count) 0)
    (let* ((i (supersonic-browse--current-index))
           (first (car (supersonic-browse--line-range i))))
      (supersonic-browse--goto-index
       (if (/= i first) first
         (let ((region-first nil) (region (supersonic-browse--prop i 'supersonic-browse-region)))
           (dotimes (j (supersonic-browse--stop-count))
             (when (and (not region-first) (eql (supersonic-browse--prop j 'supersonic-browse-region) region))
               (setq region-first j)))
           region-first))))))

(defun supersonic-browse-row-end ()
  "Focus the last item of this line; there already, the last of the section."
  (interactive)
  (when (> (supersonic-browse--stop-count) 0)
    (let* ((i (supersonic-browse--current-index))
           (last (cdr (supersonic-browse--line-range i))))
      (supersonic-browse--goto-index
       (if (/= i last) last
         (supersonic-browse--region-last (supersonic-browse--prop i 'supersonic-browse-region)))))))

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
     (setq supersonic-browse--content nil
           supersonic-browse--more nil)
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
                    (propertize (supersonic-browse--error-text err) 'face 'error)
                    "\n" (supersonic-browse--space supersonic-browse-margin)
                    (supersonic-browse-hint-line
                     '((supersonic-browse-reload . "try again") (supersonic-local-toggle . "local files")
                       (supersonic-browse-back . "back"))
                     supersonic-browse-mode-map 10000)
                    "\n"))))))))

(defun supersonic-browse--error-text (err)
  "Return what to say about ERR, a page that failed to load."
  (let ((text (error-message-string err)))
    (if (string-match-p "No answer\\|connection\\|refused\\|unreachable\\|resolve\\|timed out\\|network" text)
        "The server isn't answering."
      (concat "This page could not be loaded: " text))))

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
      ((or 'page 'hero) (cons (plist-get supersonic-browse--page :kind)
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
   ('playlist (mapcar (lambda (track) (plist-get track :id))
                      (if (equal (plist-get item :id) (plist-get supersonic-browse--page :id))
                          (plist-get supersonic-browse--content :tracks)
                        (aio-await (supersonic-provider-playlist-tracks (plist-get item :id))))))
   ('genre (mapcar (lambda (track) (plist-get track :id))
                   (aio-await (supersonic-provider-random-tracks supersonic-browse-genre-count (plist-get item :id)))))
   ('track (list (plist-get item :id)))))

(defun supersonic-browse--shuffled (list)
  "Return a shuffled copy of LIST."
  (let ((vector (vconcat list)))
    (cl-loop for i from (1- (length vector)) downto 1
             do (cl-rotatef (aref vector i) (aref vector (random (1+ i)))))
    (append vector nil)))

(defun supersonic-browse--flash (buff key text)
  "Show TEXT for a moment in place of the second line of item KEY in BUFF.
That is a tile's line under its title, or a track row's duration: it
says right there that something was done to the item."
  (when (buffer-live-p buff)
    (with-current-buffer buff
      (let ((pos (point-min)) (found nil))
        (while (and (not found) (setq pos (text-property-any pos (point-max) 'supersonic-browse-key key)))
          (let ((end (or (next-single-property-change pos 'supersonic-browse-key) (point-max))))
            (when (memq (get-text-property pos 'face) '(supersonic-browse-tile-subtitle supersonic-browse-track-number))
              (setq found (cons pos end)))
            (unless found
              ;; A track row: its duration is a stretch within it.
              (let ((duration (text-property-any pos end 'face 'supersonic-browse-track-number)))
                (when (and duration (> duration pos))
                  (setq found (cons duration (or (next-single-property-change duration 'face nil end) end))))))
            (setq pos end)))
        (when found
          (let ((overlay (make-overlay (car found) (cdr found))))
            (overlay-put overlay 'display (propertize text 'face 'supersonic-browse-accent))
            (run-at-time 2 nil #'delete-overlay overlay)))))))

(defun supersonic-browse--with-ids (verb function)
  "Call FUNCTION with the track ids of the item in focus, saying VERB."
  (pcase-let ((`(,kind . ,item) (supersonic-browse--target))
              (buff (current-buffer))
              (key (get-text-property (point) 'supersonic-browse-key)))
    (when (memq kind '(button more))
      (user-error "Not something to play"))
    (aio-listen (aio-catch (supersonic-browse--track-ids kind item))
                (lambda (value)
                  (let ((outcome (funcall value)))
                    (if (eq (car outcome) :error)
                        (message "[Supersonic] Failed: %s" (error-message-string (cdr outcome)))
                      (when (cdr outcome)
                        (funcall function (cdr outcome))
                        (supersonic-browse--flash buff key verb)
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

(declare-function supersonic-radio "supersonic-radio")

(defun supersonic-browse-star ()
  "Heart the song, album or artist in focus, or take the heart away."
  (interactive)
  (supersonic-provider-require 'star)
  (pcase-let* ((`(,kind . ,item) (supersonic-browse--target))
               (kind (pcase kind ((or 'track 'album 'artist) kind) (_ (user-error "Heart a song, an album or an artist"))))
               (flag (not (plist-get item :starred))))
    (aio-wait-for (supersonic-provider-star (plist-get item :id) kind flag))
    ;; The page's own item, as the header shows it.
    (when (equal (plist-get item :id) (plist-get supersonic-browse--page :id))
      (setq supersonic-browse--page
            (plist-put supersonic-browse--page :item
                       (plist-put (copy-sequence (plist-get supersonic-browse--page :item)) :starred flag))))
    (message (if flag "♥ %s is a favourite" "%s is no longer a favourite")
             (or (plist-get item :name) (plist-get item :title) "It"))
    (supersonic-browse-reload)))

;;;###autoload
(defun supersonic-browse-favourites ()
  "Show the favourite songs, albums and artists in the browser."
  (interactive)
  (supersonic-browse-visit '(:kind starred)))

;;;###autoload
(defun supersonic-browse-downloads ()
  "Show the albums saved for offline use, and a download running."
  (interactive)
  (supersonic-browse-visit '(:kind downloads)))

;;;###autoload
(defun supersonic-browse-queue ()
  "Show the play queue in the browser."
  (interactive)
  (supersonic-browse-visit '(:kind queue)))

;;;###autoload
(defun supersonic-browse-genres ()
  "Show every genre in the browser."
  (interactive)
  (supersonic-browse-visit '(:kind genres)))

(defun supersonic-browse--genre-cards (genres)
  "Return GENRES marked to be drawn as cards with their names."
  (mapcar (lambda (genre) (append (list :card t) genre)) genres))

(defun supersonic-browse-radio ()
  "Start a radio from the item in focus: it, then what is like it."
  (interactive)
  (pcase-let ((`(,kind . ,item) (supersonic-browse--target)))
    (pcase kind
      ('track (supersonic-radio (plist-get item :id)))
      ((or 'album 'artist)
       (let ((ids (aio-wait-for (supersonic-browse--track-ids kind item))))
         (unless ids (user-error "Nothing to start a radio from"))
         (supersonic-radio (plist-get item :id) (list (nth (random (length ids)) ids)))))
      (_ (user-error "Start a radio from a song, an album or an artist")))))

(defun supersonic-browse-enqueue ()
  "Add the item in focus to the play queue."
  (interactive)
  (supersonic-browse--with-ids "Queued" #'supersonic-playback-enqueue))

(aio-defun
 supersonic-browse--download-ids (ids)
 "Download the tracks with IDS, looked up all at once."
 (let ((promises (mapcar #'supersonic-provider-track ids))
       tracks)
   ;; `dolist', so that `aio-await' stays where generator.el transforms it.
   (dolist (promise promises)
     (push (aio-await promise) tracks))
   (supersonic-download-tracks (nreverse tracks))))

(defun supersonic-browse-download ()
  "Save the item in focus for offline use, see `supersonic-download-tracks'."
  (interactive)
  (require 'supersonic-download)
  (supersonic-browse--with-ids "Downloading" (lambda (ids) (ignore (supersonic-browse--download-ids ids)))))

(defun supersonic-browse-open ()
  "Open the item in focus, or play from the track in focus on."
  (interactive)
  (let ((key (get-text-property (point) 'supersonic-browse-key))
        (item (get-text-property (point) 'supersonic-browse-item)))
    (pcase (car-safe key)
      ('artist (supersonic-browse-visit (list :kind 'artist :id (plist-get item :id) :item item)))
      ('album (supersonic-browse-visit (list :kind 'album :id (plist-get item :id) :item item)))
      ('playlist (supersonic-browse-visit (list :kind 'playlist :id (plist-get item :id) :item item)))
      ('genre (supersonic-browse-visit (list :kind 'genre :id (plist-get item :id) :item item)))
      ('track (if (eq (plist-get supersonic-browse--page :kind) 'queue)
                  (supersonic-playback-play-index (get-text-property (point) 'supersonic-browse-index))
                (supersonic-playback-start (nthcdr (get-text-property (point) 'supersonic-browse-index)
                                                   (get-text-property (point) 'supersonic-browse-ids)))))
      ((or 'button 'more)
       (let ((action (plist-get item :action)))
         ;; A command asks what it needs, as when run with a key.
         (if (commandp action) (call-interactively action) (funcall action))))
      ('hero (call-interactively (cadr key)))
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
;;; Playlists
;;;

;;;###autoload
(defun supersonic-browse-playlists ()
  "Show every playlist in the browser."
  (interactive)
  (supersonic-browse-visit '(:kind playlists)))

(defun supersonic-browse--playlist-choose (prompt)
  "Pick a playlist with PROMPT; return it, or the name for a new one as a string."
  (supersonic-provider-require 'playlists)
  (let* ((playlists (aio-wait-for (supersonic-provider-playlists)))
         (new "+ New playlist")
         (names (mapcar (lambda (p) (plist-get p :name)) playlists))
         (choice (completing-read prompt (cons new names) nil t)))
    (if (equal choice new)
        (let ((name (string-trim (read-string "Name of the new playlist: "))))
          (when (string-empty-p name)
            (user-error "A playlist needs a name"))
          name)
      (seq-find (lambda (p) (equal (plist-get p :name) choice)) playlists))))

(defun supersonic-browse-add-to-playlist ()
  "Add the song, album or artist in focus to a playlist, or to a new one."
  (interactive)
  (pcase-let ((`(,kind . ,item) (supersonic-browse--target)))
    (when (memq kind '(button more))
      (user-error "Not something to add to a playlist"))
    (let* ((target (supersonic-browse--playlist-choose "Add to playlist: "))
           (ids (aio-wait-for (supersonic-browse--track-ids kind item))))
      (if (stringp target)
          (progn
            (aio-wait-for (supersonic-provider-create-playlist target ids))
            (message "Made playlist %s of %s" target (supersonic-browse--plural (length ids) "song")))
        (aio-wait-for (supersonic-provider-edit-playlist (plist-get target :id) (list :add ids)))
        (message "Added %s to %s" (supersonic-browse--plural (length ids) "song") (plist-get target :name))
        (when (equal (plist-get target :id) (plist-get supersonic-browse--page :id))
          (supersonic-browse-reload))))))

(defun supersonic-browse--playlist-row ()
  "Return the position of the playlist or queue track in focus, or complain."
  (unless (memq (plist-get supersonic-browse--page :kind) '(playlist queue))
    (user-error "Only on a playlist's page or the queue"))
  (unless (eq (car-safe (get-text-property (point) 'supersonic-browse-key)) 'track)
    (user-error "No song here"))
  (get-text-property (point) 'supersonic-browse-index))

(defun supersonic-browse-playlist-remove ()
  "Remove the song in focus from this playlist or the queue."
  (interactive)
  (let ((row (supersonic-browse--playlist-row)))
    (setq supersonic-browse--restore-row (max 0 (min row (- (length (plist-get supersonic-browse--content :tracks)) 2))))
    ;; The queue's change hook shows the queue anew by itself.
    (if (eq (plist-get supersonic-browse--page :kind) 'queue)
        (supersonic-playback-remove row)
      (aio-wait-for (supersonic-provider-edit-playlist (plist-get supersonic-browse--page :id) (list :remove (list row))))
      (supersonic-browse-reload))))

(defun supersonic-browse--playlist-move (delta)
  "Move the song in focus DELTA places in this playlist or the queue."
  (let* ((row (supersonic-browse--playlist-row))
         (ids (vconcat (mapcar (lambda (track) (plist-get track :id)) (plist-get supersonic-browse--content :tracks))))
         (to (+ row delta)))
    (when (and (>= to 0) (< to (length ids)))
      (setq supersonic-browse--restore-row to)
      (if (eq (plist-get supersonic-browse--page :kind) 'queue)
          (supersonic-playback-move row to)
        (cl-rotatef (aref ids row) (aref ids to))
        (aio-wait-for (supersonic-provider-set-playlist-tracks (plist-get supersonic-browse--page :id) (append ids nil)))
        (supersonic-browse-reload)))))

(defun supersonic-browse-playlist-move-up ()
  "Move the song in focus one place up this playlist."
  (interactive)
  (supersonic-browse--playlist-move -1))

(defun supersonic-browse-playlist-move-down ()
  "Move the song in focus one place down this playlist."
  (interactive)
  (supersonic-browse--playlist-move 1))

(defun supersonic-browse-playlist-rename (name)
  "Rename this playlist NAME."
  (interactive
   (progn
     (unless (eq (plist-get supersonic-browse--page :kind) 'playlist)
       (user-error "Only on a playlist's page"))
     (list (read-string "Rename playlist to: " (plist-get (plist-get supersonic-browse--page :item) :name)))))
  (aio-wait-for (supersonic-provider-edit-playlist (plist-get supersonic-browse--page :id) (list :name name)))
  (setq supersonic-browse--page
        (plist-put supersonic-browse--page :item (plist-put (copy-sequence (plist-get supersonic-browse--page :item)) :name name)))
  (supersonic-browse-reload))

(defun supersonic-browse-delete ()
  "Delete this playlist, or on the downloads page the album in focus.
Asks first."
  (interactive)
  (if (not (eq (plist-get supersonic-browse--page :kind) 'downloads))
      (supersonic-browse-playlist-delete)
    (let ((item (get-text-property (point) 'supersonic-browse-item)))
      (unless (plist-get item :download-dir)
        (user-error "No saved album here"))
      (when (yes-or-no-p (format "Delete the saved %s? " (plist-get item :name)))
        (supersonic-download-delete-album item)
        (message "Deleted %s" (abbreviate-file-name (plist-get item :download-dir)))))))

(defun supersonic-browse--downloads-changed ()
  "Show the downloads anew, if the browser shows them."
  (let ((buff (get-buffer supersonic-browse-buffer-name)))
    (when (and (buffer-live-p buff)
               (eq (plist-get (buffer-local-value 'supersonic-browse--page buff) :kind) 'downloads))
      ;; Not a reload: that would blank the page for every song saved.
      (aio-listen (aio-catch (supersonic-browse--load-content '(:kind downloads)))
                  (lambda (value)
                    (let ((outcome (funcall value)))
                      (when (and (eq (car outcome) :success) (buffer-live-p buff))
                        (with-current-buffer buff
                          (setq supersonic-browse--content (cdr outcome))
                          (supersonic-browse--render)))))))))

(defun supersonic-browse-playlist-delete ()
  "Delete this playlist, after asking."
  (interactive)
  (unless (eq (plist-get supersonic-browse--page :kind) 'playlist)
    (user-error "Only on a playlist's page"))
  (let ((name (plist-get (plist-get supersonic-browse--page :item) :name)))
    (when (yes-or-no-p (format "Delete the playlist %s? " name))
      (aio-wait-for (supersonic-provider-delete-playlist (plist-get supersonic-browse--page :id)))
      (message "Deleted playlist %s" name)
      (if supersonic-browse--back
          (supersonic-browse-back)
        (supersonic-browse-home)))))

;;;###autoload
(defun supersonic-save-queue-as-playlist (name)
  "Save the play queue as the playlist NAME, replacing one so called after asking."
  (interactive (list (string-trim (read-string "Save the queue as playlist: "))))
  (when (string-empty-p name)
    (user-error "A playlist needs a name"))
  (supersonic-provider-require 'playlists)
  (let ((ids (delq nil (mapcar (lambda (entry) (plist-get entry :track-id))
                               (aio-wait-for (supersonic-playback-queue)))))
        (existing (seq-find (lambda (p) (equal (plist-get p :name) name))
                            (aio-wait-for (supersonic-provider-playlists)))))
    (unless ids
      (user-error "The queue is empty"))
    (if existing
        (if (yes-or-no-p (format "Replace the playlist %s? " name))
            (aio-wait-for (supersonic-provider-set-playlist-tracks (plist-get existing :id) ids))
          (user-error "Kept the playlist %s as it was" name))
      (aio-wait-for (supersonic-provider-create-playlist name ids)))
    (message "Saved the queue, %s, as %s" (supersonic-browse--plural (length ids) "song") name)))

;;;
;;; Cheatsheet
;;;

(defconst supersonic-cheatsheet-buffer-name "*supersonic-keys*"
  "Name of the cheatsheet's buffer.")

(defconst supersonic-cheatsheet--sections
  '(("Playback" all
     ("play / pause" supersonic-toggle-playing)
     ("next track" supersonic-skip-track)
     ("previous track" supersonic-prev-track)
     ("seek forward" supersonic-seek-forward)
     ("seek back" supersonic-seek-back)
     ("louder" (:or supersonic-player-volume-up supersonic-volume-up))
     ("quieter" (:or supersonic-player-volume-down supersonic-volume-down))
     ("shuffle" (:or supersonic-player-shuffle supersonic-toggle-shuffle))
     ("repeat" (:or supersonic-player-repeat supersonic-cycle-repeat)))
    ("Move" all
     ("up / down" supersonic-browse-up supersonic-browse-down)
     ("left / right" supersonic-browse-previous supersonic-browse-next)
     ("start / end of line" supersonic-browse-row-start supersonic-browse-row-end)
     ("next / previous section" supersonic-browse-next-section supersonic-browse-previous-section))
    ("On an item" browser
     ("open, or play from here" supersonic-browse-open)
     ("play" supersonic-browse-play)
     ("shuffle" supersonic-browse-shuffle)
     ("add to the queue" supersonic-browse-enqueue)
     ("add to a playlist" supersonic-browse-add-to-playlist)
     ("save for offline" supersonic-browse-download)
     ("radio: it, then more like it" supersonic-browse-radio)
     ("favourite (♥) or not" supersonic-browse-star))
    ("Pages" browser
     ("back / forward" supersonic-browse-back supersonic-browse-forward)
     ("home" supersonic-browse-home)
     ("search" supersonic-browse-search)
     ("reload" supersonic-browse-reload)
     ("local files or the server" supersonic-local-toggle)
     ("close" quit-window))
    ("The queue" queue
     ("play this one" supersonic-browse-open)
     ("remove it" supersonic-browse-playlist-remove)
     ("move it up / down" supersonic-browse-playlist-move-up supersonic-browse-playlist-move-down)
     ("clear all but the one playing" supersonic-clear-queue))
    ("A playlist's page" playlist
     ("remove the song" supersonic-browse-playlist-remove)
     ("move it up / down" supersonic-browse-playlist-move-up supersonic-browse-playlist-move-down)
     ("rename the playlist" supersonic-browse-playlist-rename)
     ("delete the playlist" supersonic-browse-delete))
    ("The downloads" downloads
     ("delete the saved album" supersonic-browse-delete))
    ("Now playing" player
     ("go to the artist" supersonic-player-goto-artist)
     ("go to the album" supersonic-player-goto-album)
     ("radio from this song" supersonic-radio)
     ("favourite (♥) or not" supersonic-player-star)
     ("all the lyrics" supersonic-lyrics)
     ("the queue" supersonic-browse-queue)
     ("back to the library" supersonic-player-library)
     ("reload" supersonic-player-refresh))
    ("From anywhere" prefix
     ("library" supersonic-browse)
     ("now playing" (:or supersonic-player supersonic-show-now-playing))
     ("search" (:or supersonic-consult supersonic-browse-search supersonic-search-read))
     ("artists" supersonic-browse-artists)
     ("playlists" supersonic-browse-playlists)
     ("favourites" supersonic-browse-favourites)
     ("genres" supersonic-browse-genres)
     ("play queue" (:or supersonic-browse-queue supersonic-show-queue))
     ("downloads" supersonic-browse-downloads)
     ("save the queue as a playlist" supersonic-save-queue-as-playlist)
     ("lyrics" supersonic-lyrics)
     ("lyrics in the echo area" supersonic-lyrics-toggle-echo-area)
     ("lyrics translations" supersonic-lyrics-toggle-translation)
     ("local files / server" supersonic-local-toggle)
     ("everything else" supersonic)))
  "What the cheatsheet lists: (TITLE WHERE (LABEL COMMAND...)...).
WHERE says when a section applies: `all', `browser' (the library
browser, not the now-playing page), `playlist' (a playlist's page),
`player' (the now-playing page) or `prefix' (the keys under
`supersonic-command-map', wherever that is bound).  An entry with
several commands shows the keys of each, joined by \"/\"; one of
them may be (:or COMMAND...), the first of which with a key counts.
An entry none of whose commands has a key is left out.")

(defun supersonic-cheatsheet--remapped-from (command map)
  "Return the commands MAP remaps to COMMAND."
  (let ((remaps (lookup-key map [remap]))
        (from nil))
    (when (keymapp remaps)
      (map-keymap (lambda (original target) (when (eq target command) (push original from))) remaps))
    from))

(defun supersonic-cheatsheet--prefix-keys (maps)
  "Return the keys `supersonic-command-map' is bound to in MAPS."
  (where-is-internal 'supersonic-command-map
                     (append maps (and (boundp 'xah-fly-command-map) (list (symbol-value 'xah-fly-command-map))))))

(defun supersonic-cheatsheet--keys (command maps)
  "Return the keys running COMMAND in MAPS, as short strings.
Keys of commands MAPS remap to COMMAND count too, looked up in the
modal editing package's own map if there is one -- that is where the
motions people use live."
  (let* ((modal (and (boundp 'xah-fly-command-map) (list (symbol-value 'xah-fly-command-map))))
         (prefixes (supersonic-cheatsheet--prefix-keys maps))
         (keys (append (where-is-internal command maps)
                       (mapcan (lambda (original)
                                 (append (where-is-internal original (append modal maps))))
                               (seq-mapcat (lambda (map) (supersonic-cheatsheet--remapped-from command map)) maps)))))
    ;; Shortest first: "l" says more at a glance than "M-<left>".
    (seq-take
     (sort (delete-dups
            (delq nil (mapcar (lambda (key)
                                (let ((description (key-description key)))
                                  (unless (or (eq (aref key 0) 'remap) (memq (aref key 0) '(menu-bar tool-bar))
                                              (string-match-p "mouse\\|wheel" description)
                                              ;; Through the prefix map: one's own, not the buffer's.
                                              (seq-some (lambda (prefix)
                                                          (and (> (length key) (length prefix))
                                                               (equal (seq-take key (length prefix)) prefix)))
                                                        prefixes))
                                    description)))
                              keys)))
           (lambda (a b) (< (length a) (length b))))
     3)))

(defun supersonic-cheatsheet--prefix ()
  "Return the key `supersonic-command-map' is on, as a string, or nil."
  (let* ((maps (append (and (boundp 'xah-fly-command-map) (list (symbol-value 'xah-fly-command-map)))
                       (list global-map)))
         (key (car (where-is-internal 'supersonic-command-map maps))))
    (and key (key-description key))))

(defun supersonic-cheatsheet--entries (section maps prefix)
  "Return SECTION's entries as (KEYS . LABEL) with keys found in MAPS.
PREFIX is put before the keys of a `prefix' section."
  (delq nil
        (mapcar (lambda (entry)
                  (let* ((found (delq nil (mapcar (lambda (command)
                                                    (if (eq (car-safe command) :or)
                                                        (seq-some (lambda (c) (car (supersonic-cheatsheet--keys c maps)))
                                                                  (cdr command))
                                                      (car (supersonic-cheatsheet--keys command maps))))
                                                  (cdr entry))))
                         (found (if (eq (nth 1 section) 'prefix)
                                    (mapcar (lambda (key) (concat prefix " " key)) found)
                                  found)))
                    (and found (cons (seq-take (delete-dups found) 2) (car entry)))))
                (nthcdr 2 section))))

(defun supersonic-cheatsheet--chip (key)
  "Return KEY drawn as a key cap."
  (propertize (concat " " key " ") 'face 'supersonic-cheatsheet-key))

(defun supersonic-cheatsheet--render (from)
  "Lay the cheatsheet out for the buffer FROM it was asked for in."
  (let* ((inhibit-read-only t)
         (mode (buffer-local-value 'major-mode from))
         (page (buffer-local-value 'supersonic-browse--page from))
         (player (provided-mode-derived-p mode 'supersonic-player-mode))
         (browser (and (provided-mode-derived-p mode 'supersonic-browse-mode) (not player)))
         (maps (with-current-buffer from (current-active-maps)))
         (prefix (or (supersonic-cheatsheet--prefix) "M-x supersonic-command-map"))
         (prefix-maps (list supersonic-command-map))
         (blocks
          (delq nil
                (mapcar
                 (lambda (section)
                   (let ((entries (pcase (nth 1 section)
                                    ('all (supersonic-cheatsheet--entries section maps prefix))
                                    ('browser (and browser (supersonic-cheatsheet--entries section maps prefix)))
                                    ('playlist (and browser (eq (plist-get page :kind) 'playlist)
                                                    (supersonic-cheatsheet--entries section maps prefix)))
                                    ('player (and player (supersonic-cheatsheet--entries section maps prefix)))
                                    ('downloads (and browser (eq (plist-get page :kind) 'downloads)
                                                     (supersonic-cheatsheet--entries section maps prefix)))
                                    ('queue (and browser (eq (plist-get page :kind) 'queue)
                                                 (supersonic-cheatsheet--entries section maps prefix)))
                                    ('prefix (and supersonic-cheatsheet-show-prefix
                                                  (supersonic-cheatsheet--entries section prefix-maps prefix))))))
                     (and entries (cons (car section) entries))))
                 supersonic-cheatsheet--sections)))
         (width (window-body-width (get-buffer-window (current-buffer) t) t))
         (key-width (* 13 (frame-char-width)))
         (column-width (+ key-width (* 26 (frame-char-width))))
         (columns (max 1 (min (length blocks) (/ (- width (* 2 supersonic-browse-margin)) column-width))))
         (heights (make-vector columns 0))
         (placed (make-vector columns nil)))
    ;; Each section into the shortest column so far.
    (dolist (block blocks)
      (let ((shortest (seq-position (append heights nil) (seq-min heights))))
        (aset placed shortest (append (aref placed shortest) (list block)))
        (aset heights shortest (+ (aref heights shortest) (length block) 2))))
    (erase-buffer)
    (insert (propertize "\n" 'line-height 1.2)
            (supersonic-browse--space supersonic-browse-margin)
            (propertize "Keys" 'face 'supersonic-browse-heading)
            "   " (propertize (substitute-command-keys "\\<supersonic-cheatsheet-mode-map>\\[supersonic-cheatsheet] or \\[quit-window] closes this")
                              'face 'supersonic-browse-hint)
            (propertize "\n" 'line-height 1.6))
    (let ((lines (make-vector columns nil)))
      (dotimes (c columns)
        (dolist (block (aref placed c))
          (aset lines c (append (aref lines c)
                                (list (cons 'heading (car block)))
                                (mapcar (lambda (entry) (cons 'entry entry)) (cdr block))
                                (list (cons 'gap nil))))))
      (dotimes (row (seq-max (mapcar #'length lines)))
        (dotimes (c columns)
          (let ((line (nth row (aref lines c)))
                (x (+ supersonic-browse-margin (* c column-width))))
            (pcase (car line)
              ('heading (insert (supersonic-browse--align x) (propertize (cdr line) 'face 'supersonic-browse-trail-current)))
              ('entry (insert (supersonic-browse--align x)
                              (mapconcat #'supersonic-cheatsheet--chip (cadr line) (propertize " / " 'face 'shadow))
                              (supersonic-browse--align (+ x key-width))
                              (propertize (cddr line) 'face 'default))))))
        (insert (propertize "\n" 'line-height 1.35))))
    (goto-char (point-min))))

;;;###autoload
(defun supersonic-cheatsheet ()
  "Show the keys of supersonic that apply here; again, hide them.
The keys are read off the keymaps, so they are the ones bound."
  (interactive)
  (let ((shown (get-buffer-window supersonic-cheatsheet-buffer-name)))
    (if shown
        (quit-window nil shown)
      (let ((from (current-buffer))
            (buff (get-buffer-create supersonic-cheatsheet-buffer-name)))
        (with-current-buffer buff
          (unless (derived-mode-p 'supersonic-cheatsheet-mode)
            (supersonic-cheatsheet-mode)))
        (let ((window (display-buffer buff '((display-buffer-reuse-window display-buffer-in-side-window)
                                             (side . bottom) (window-height . 0.42)))))
          (with-current-buffer buff
            (with-selected-window window
              (supersonic-cheatsheet--render from))
            (fit-window-to-buffer window (floor (* 0.6 (frame-height (window-frame window)))))))))))

(defvar supersonic-cheatsheet-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "?") #'supersonic-cheatsheet)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `supersonic-cheatsheet-mode'.")

(define-derived-mode supersonic-cheatsheet-mode special-mode "Supersonic Keys"
  "Major mode of the cheatsheet, see `supersonic-cheatsheet'."
  (setq-local truncate-lines t
              cursor-type nil)
  (setq-local global-hl-line-mode nil))

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
                              (supersonic-browse--render)))
                          (when (eq (plist-get supersonic-browse--page :kind) 'queue)
                            (supersonic-browse-reload))))))))))

(defun supersonic-browse--queue-changed ()
  "Show the queue anew, if the browser shows it."
  (let ((buff (get-buffer supersonic-browse-buffer-name)))
    (when (and (buffer-live-p buff)
               (eq (plist-get (buffer-local-value 'supersonic-browse--page buff) :kind) 'queue))
      (with-current-buffer buff
        (supersonic-browse-reload)))))

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
    (define-key map (kbd "A") #'supersonic-browse-add-to-playlist)
    (define-key map (kbd "S") #'supersonic-browse-download)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "O") #'supersonic-local-toggle)
    (define-key map (kbd "x") #'supersonic-browse-radio)
    (define-key map (kbd "*") #'supersonic-browse-star)
    (define-key map (kbd "d") #'supersonic-browse-playlist-remove)
    (define-key map (kbd "M-p") #'supersonic-browse-playlist-move-up)
    (define-key map (kbd "M-n") #'supersonic-browse-playlist-move-down)
    (define-key map (kbd "M-<up>") #'supersonic-browse-playlist-move-up)
    (define-key map (kbd "M-<down>") #'supersonic-browse-playlist-move-down)
    (define-key map (kbd "e") #'supersonic-browse-playlist-rename)
    (define-key map (kbd "D") #'supersonic-browse-delete)
    (define-key map (kbd "C") #'supersonic-clear-queue)
    (define-key map (kbd "/") #'supersonic-browse-search)
    (define-key map (kbd "g") #'supersonic-browse-reload)
    ;; The same transport keys as everywhere in supersonic; moving
    ;; between items is for the arrows and whatever moves the cursor.
    (define-key map (kbd "n") #'supersonic-skip-track)
    (define-key map (kbd "p") #'supersonic-prev-track)
    (define-key map (kbd "f") #'supersonic-seek-forward)
    (define-key map (kbd "b") #'supersonic-seek-back)
    (define-key map (kbd "?") #'supersonic-cheatsheet)
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
                     (move-beginning-of-line . supersonic-browse-row-start)
                     (move-end-of-line . supersonic-browse-row-end)
                     (beginning-of-line . supersonic-browse-row-start)
                     (end-of-line . supersonic-browse-row-end)
                     (beginning-of-visual-line . supersonic-browse-row-start)
                     (end-of-visual-line . supersonic-browse-row-end)
                     ;; xah-fly-keys
                     (xah-beginning-of-line-or-block . supersonic-browse-row-start)
                     (xah-end-of-line-or-block . supersonic-browse-row-end)
                     ;; evil and meow
                     (evil-next-line . supersonic-browse-down)
                     (evil-previous-line . supersonic-browse-up)
                     (evil-forward-char . supersonic-browse-next)
                     (evil-backward-char . supersonic-browse-previous)
                     (evil-beginning-of-line . supersonic-browse-row-start)
                     (evil-end-of-line . supersonic-browse-row-end)
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
  (add-hook 'isearch-mode-end-hook #'supersonic-browse--isearch-done nil t)
  (add-hook 'window-size-change-functions #'supersonic-browse--window-changed nil t)
  (add-hook 'supersonic-playback-track-change-hook #'supersonic-browse--track-changed)
  (add-hook 'supersonic-playback-queue-change-hook #'supersonic-browse--queue-changed)
  (add-hook 'supersonic-download-change-hook #'supersonic-browse--downloads-changed)
  (setq supersonic-browse--playing nil)
  (supersonic-browse--track-changed))

(provide 'supersonic-browse)

;;; supersonic-browse.el ends here
