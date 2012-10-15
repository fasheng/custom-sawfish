
(require 'animate-move)

(defun rsk-shove-size-window (dir inc &optional no-focus)
  "Size focused window by chunks 'left, 'right, 'up or 'down"
  (interactive)
  (let* ((win (input-focus))
         (dim (window-dimensions win))
	 (dimx (car dim))
	 (dimy (cdr dim)))
    (cond ((eq dir 'left) (setq dimx (- dimx inc)))
          ((eq dir 'right) (setq dimx (+ dimx inc)))
          ((eq dir 'up) (setq dimy (- dimy inc)))
          ((eq dir 'down) (setq dimy (+ dimy inc))))
    (resize-window-to win dimx dimy)
    (unless no-focus
      (display-window win))))

(defvar rsk-panel-height 50
  "Panel heigth for rsk-grid-place-window"
  :type number
  :group no-mouse)

(defun rsk-grid-place-window (grid)
  "move the window to the grid position specified by the numeric keypad"
  (interactive)
  (let* ((win (input-focus))
	 (pos (window-position win))
	 (posx (car pos))
	 (posy (cdr pos))
	 (dim (window-frame-dimensions win))
	 (dimx (car dim))
	 (dimy (cdr dim))
	 (bottom (- (screen-height) rsk-panel-height))
	 (x 0)
	 (y 0))
    (cond ((or (equal grid 7) (equal grid 8) (equal grid 9))
	   (setq y 0))
	  ((or (equal grid 4) (equal grid 5) (equal grid 6))
	   (setq y (- (/ bottom 2) (/ dimy 2))))
	  ((or (equal grid 1) (equal grid 2) (equal grid 3))
	   (setq y (- bottom dimy))))
    (cond ((or (equal grid 1) (equal grid 4) (equal grid 7))
	   (setq x 0))
	  ((or (equal grid 2) (equal grid 5) (equal grid 8))
	   (setq x (- (/ (screen-width) 2) (/ dimx 2))))
	  ((or (equal grid 3) (equal grid 6) (equal grid 9))
	   (setq x (- (screen-width) dimx))))
    ;; the following is necessary because "/" function can result in
    ;; fractional numbers
    (setq x (truncate x))
    (setq y (truncate y))
    ;; (move-window-to win x y)
    (animate-move-window-to win x y)
    (display-window win)))

;; (bind-keys global-keymap "C-M-Left"      '(rsk-shove-size-window 'left 30))
;; (bind-keys global-keymap "C-M-Right"     '(rsk-shove-size-window 'right 30))
;; (bind-keys global-keymap "C-M-Up"        '(rsk-shove-size-window 'up 30))
;; (bind-keys global-keymap "C-M-Down"      '(rsk-shove-size-window 'down 30))
;; (bind-keys global-keymap "S-C-M-Left"    '(rsk-shove-size-window 'left 1))
;; (bind-keys global-keymap "S-C-M-Right"   '(rsk-shove-size-window 'right 1))
;; (bind-keys global-keymap "S-C-M-Up"      '(rsk-shove-size-window 'up 1))
;; (bind-keys global-keymap "S-C-M-Down"    '(rsk-shove-size-window 'down 1))

;; (bind-keys global-keymap "C-M-KP_End"    '(rsk-grid-place-window 1))
;; (bind-keys global-keymap "C-M-KP_Down"   '(rsk-grid-place-window 2))
;; (bind-keys global-keymap "C-M-KP_Next"   '(rsk-grid-place-window 3))
;; (bind-keys global-keymap "C-M-KP_Left"   '(rsk-grid-place-window 4))
;; (bind-keys global-keymap "C-M-KP_Begin"  '(rsk-grid-place-window 5))
;; (bind-keys global-keymap "C-M-KP_Right"  '(rsk-grid-place-window 6))
;; (bind-keys global-keymap "C-M-KP_Home"   '(rsk-grid-place-window 7))
;; (bind-keys global-keymap "C-M-KP_Up"     '(rsk-grid-place-window 8))
;; (bind-keys global-keymap "C-M-KP_Prior"  '(rsk-grid-place-window 9))

;; (bind-keys global-keymap "C-M-KP_Insert" '(maximize-window-vertically-toggle (input-focus)))
;; (bind-keys global-keymap "C-M-KP_Delete" '(maximize-window-toggle (input-focus)))
