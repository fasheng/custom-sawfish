; Author: Eric Mangold - teratorn (at) world (dash) net (dot) net
;
; When a user attempts to move a window, this will move the mouse
; pointer to the center of the window first. It's Nice to keep the
; pointer from hitting the sides of your screen :)
;
; This won't interfere if you move the window by clicking the title bar
;
; all you need to do is put the following line in your ~/.sawfish/rc file
; (require 'center-mouse-on-window-move)

(defun center-mouse-on-window ()
  ;don't move pointer if user is moving window by clicking title bar
  (if (string-match "Click" (event-name (last-event))) ()
  (let ((win-origin (window-position (input-focus)))
	(win-dims (window-dimensions (input-focus))))
    (warp-cursor
     (+ (car win-origin) (quotient (car win-dims) 2))
     (+ (cdr win-origin) (quotient (cdr win-dims) 2)))
    (allow-events 'sync-both) ;make the new pointer position take affect immediately
    )))
(add-hook 'before-move-hook center-mouse-on-window)
