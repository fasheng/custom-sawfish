;;;; direction.jl -- focus window east/west/north/south of current one
;;;; (C) Ives Aerts <ives.aerts@sonycom.com>, 12 June 2002

;;; First some helper functions.

;; The window must be 'visible' (i.e.: not hidden/iconified), not
;; 'fully obscured' (completely behind another window or on another
;; viewport), in the window cycle list, and not 'ignored'.
(defun window-dfocusable-p (w)
  "Is window focusable by direction?"
  (and (window-visible-p w)
       (window-mapped-p w)
       (not (equal `fully-obscured (window-visibility w)))
       (window-in-cycle-p w)
       (not (window-get w `ignored)) ) )

(defun abs (x)
  (if (> x 0) x (- x)))
  
(defun window-center (w)
  "Compute the center of a window."
  (let ((x (car (window-position w)))
        (y (cdr (window-position w)))
        (w (car (window-dimensions w)))
        (h (cdr (window-dimensions w))) )
       (cons (+ x (/ w 2)) (+ y (/ h 2))) ) )

(defun horizontal-distance (w1 w2)
  "Compute horizontal distance between two windows."
  (let ((w1_x (car (window-position w1)))
        (w1_y (cdr (window-position w1)))
        (w2_x (car (window-position w2)))
        (w2_y (cdr (window-position w2))) )
       (+ (abs (- w1_x w2_x)) (* 2 (abs (- w1_y w2_y)))) ) )

(defun vertical-distance (w1 w2)
  "Compute vertical distance between two windows."
  (let ((w1_x (car (window-position w1)))
        (w1_y (cdr (window-position w1)))
        (w2_x (car (window-position w2)))
        (w2_y (cdr (window-position w2))) )
       (+ (* 2 (abs (- w1_x w2_x))) (abs (- w1_y w2_y))) ) )

(defun nearest (window window-list distfunc)
  "Using 'distfunc', find the nearest window in 'window-list' to 'window'."
  (when window-list
    (let* ((result-win (car window-list))
           (result-dist (distfunc window result-win)) )
      (mapc (lambda (w)
              (let ((dist (distfunc window w)))
                (when (< dist result-dist)
                  (setq result-win w)
                  (setq result-dist dist) ) ) )
            (cdr window-list) )
      result-win) ) )

;;; These functions could be used for other purposes than focussing
;;; nearby windows.

(defun direction-west (w)
  "Return nearest window west of w."
  (let* ((wx (car (window-center w)))
         (dfocusable-windows (filter (lambda (w)
                                        (and (window-dfocusable-p w)
                                             (< (car (window-center w)) wx)))
                                     (managed-windows))))
        (nearest w dfocusable-windows horizontal-distance)))

(defun direction-east (w)
  "Return nearest window east of w."
  (let* ((wx (car (window-center w)))
         (dfocusable-windows (filter (lambda (w)
                                        (and (window-dfocusable-p w)
                                             (> (car (window-center w)) wx)))
                                     (managed-windows))))
        (nearest w dfocusable-windows horizontal-distance)))

(defun direction-north (w)
  "Return nearest window north of w."
  (let* ((wy (cdr (window-center w)))
         (dfocusable-windows (filter (lambda (w)
                                        (and (window-dfocusable-p w)
                                             (< (cdr (window-center w)) wy)))
                                     (managed-windows))))
        (nearest w dfocusable-windows vertical-distance)))

(defun direction-south (w)
  "Return nearest window south of w."
  (let* ((wy (cdr (window-center w)))
         (dfocusable-windows (filter (lambda (w)
                                        (and (window-dfocusable-p w)
                                             (> (cdr (window-center w)) wy)))
                                     (managed-windows))))
        (nearest w dfocusable-windows vertical-distance)))

;;; And now the actual focussing functions, which can be used from
;;; sawmill-ui.

(defun focus-west (w)
  "Focus nearest window west of w."
  (interactive "%W")
  (let ((nearest (direction-west w)))
       (when nearest (raise-window (set-input-focus nearest))) ) )

(defun focus-east (w)
  "Focus nearest window east of w."
  (interactive "%W")
  (let ((nearest (direction-east w)))
       (when nearest (raise-window (set-input-focus nearest))) ) )

(defun focus-north (w)
  "Focus nearest window north of w."
  (interactive "%W")
  (let ((nearest (direction-north w)))
       (when nearest (raise-window (set-input-focus nearest))) ) )

(defun focus-south (w)
  "Focus nearest window south of w."
  (interactive "%W")
  (let ((nearest (direction-south w)))
       (when nearest (raise-window (set-input-focus nearest))) ) )

