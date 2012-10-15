;; hqw-util.jl 1.0 -- A bunch of util functions for sawfish

;; Time-stamp: <2012-10-15 20:03:31 Monday>
;; Copyright (C) 2011, hqwrong <hq.wrong@gmail.com>

;; Permission is granted to copy, distribute and/or modify this
;; document under the terms of the GNU Free Documentation License,
;; Version 1.3 or any later version published by the Free Software
;; Foundation.

;;
;;; Commentary:
(define-structure hqw-util
      (export hqw-wait-for
              hqw-exit-wait
              ;; window tile
              bottom-panel-height
              top-panel-height
              tile-wins
              ;;
              window-class-name)

      (open rep
            rep.data
            rep.system
            rep.regexp
            rep.lang.math
            sawfish.wm.state.shading
            sawfish.wm.state.iconify
            sawfish.wm.stacking
            sawfish.wm.viewport
            sawfish.wm.state.maximize
            sawfish.wm.custom
            sawfish.wm.commands
            sawfish.wm.colors
            sawfish.wm.events
            sawfish.wm.fonts
            sawfish.wm.images
            sawfish.wm.misc
            sawfish.wm.util.x
            sawfish.wm.windows
            sawfish.wm.workspace)
(provide 'hqw-util)

(define (hqw-wait-for #!key
                      (keymap '(keymap))
                      loop-on-unbound
                      handler
                      exit-hook)
   "wait-for an event.
	`keymap' is used as the override keymap during `recursive-edit'

        `handler' is a list of cons cell,as (predict
. procedure), when the key event has no bound in keymap, then the
key name is passed to each predict by order.Once eval to t ,then
the associated procedure is called with key as argument,then
re-enter in `recursive-edit'.Otherwise,if loop-on-unbound is nil,
exit loop.

        `exit-hook' ,if setted,will be called with key's name as argument,
after exit from loop."

   (call-with-keyboard-grabbed
    (lambda ()
       (let ((override-keymap keymap)
             (re-exit (lambda ()
                         (throw 're-exit
                                (event-name (current-event)))))
             (key nil))
          (add-hook 'unbound-key-hook re-exit)
          (while (catch 'hqw-exit
                    (setq key
                          (catch 're-exit
                             (recursive-edit)))
                    (when handler
                       (do ((l handler (cdr handler)))
                             ((null l) t)
                          (let* ((cell (car l))
                                 (pred (car cell))
                                 (proc (cdr cell)))
                             (when (pred key)
                                (proc key)
                                (throw 'hqw-exit t)))))
                    (when (not loop-on-unbound)
                       (throw 'hqw-exit nil))
                    t)
             t)
          (when exit-hook
             (exit-hook key))))))

(define (hqw-exit-wait)
   "You'll find it useful,when you're using `hqw-wait-for'"
   (throw 'hqw-exit))

(define (window-class-name w #!optional (ref 1))
   "Quick way to get window class name."
   (aref (get-x-text-property w 'WM_CLASS) ref))


(defvar top-panel-height 0
   "This var will be considered when using tile commands offered by hqw-util")
(defvar bottom-panel-height 0
   "This var will be considered when using tile commands offered by hqw-util")

(define-command 'tile-windows-vertically
   (lambda ()
      (let (
            (wins (workspace-windows current-workspace)))
         (tile-wins wins t
                    #:top-left-corner `(0 . ,top-panel-height)
                    #:height-for-tile (- (screen-height) (+ top-panel-height bottom-panel-height))))))

(define-command 'tile-windows-horizontally
   (lambda ()
      (let (
            (wins (workspace-windows current-workspace)))
         (tile-wins wins nil
                    #:top-left-corner `(0 . ,top-panel-height)
                    #:height-for-tile (- (screen-height)
                                         (+ top-panel-height bottom-panel-height))))))

(define (tile-wins wins #!optional
                      vertically
                      #!key
                      (width-for-tile (screen-width))
                      (height-for-tile (screen-height))
                      (top-left-corner '(0 . 0)))
   "To tile windows. "
   ;; There are always some gap between my emacs windows and other windows' borders.
   ;; Why?
   (let* (
         (len (length wins))
         (border-width (- (car (window-frame-dimensions (car wins)))
                          (car (window-dimensions (car wins)))))
         (border-height (- (cdr (window-frame-dimensions (car wins)))
                           (cdr (window-dimensions (car wins)))))
         (width-step (ceiling (/  width-for-tile
                                  len)))
         (height-step (ceiling (/ height-for-tile
                                  len))))
      (do (
           (w (car wins) (progn (setq wins (cdr wins)) (car wins)))
           (x (car top-left-corner) (if vertically
                                          x
                                       (setq x (+ x width-step))))
           (y (cdr top-left-corner) (if vertically
                                          (setq y (+ y height-step))
                                       y))
           (win-width (if vertically
                            (- width-for-tile border-width)
                         (- width-step border-width)))
           (win-height (if vertically
                             (- height-step border-height)
                           (- height-for-tile border-height)))
           )
            ((null wins) )
         (move-resize-window-to w x y win-width win-height))))
)
