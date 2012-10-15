;;; move-resize-by-key-cursor.jl ---  Places mouse cursor in a right
;;; -*- lisp-mode -*-                 position when moving/resizing
;;;                                   windows by the keyboard.

;; Author: Fernando Carmona - ferkiwi@gmail.com
;; Version: 0.2
;; This script is based on center-mouse-on-window-move
;; made by Eric Mangold - teratorn (at) world (dash) net (dot) net

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; http://www.gnu.org/licenses/gpl.html


;;; Commentary:

;; When a user attempts to move/resize a window using the keyboard
;; (setting a binding for it), this script will first warp the mouse
;; cursor to convenient position of the window (but only if the user
;; didn't use the mouse). It will center the cursor on the focused window
;; for moving it, or put it on the bottom-right side for resizing.

;; Without this script, the default behavior of Sawfish when moving the
;; cursor with the keyboard can easily make it hit the sides of the
;; screen (and even worse, when resizing the behavior depends on current
;; mouse position). I think that this is rather a bug than a intended
;; behavior, because this is really annoying for someone who likes to use
;; the keyboard and has led to many other scripts that intend to replace
;; this behavior with new functions.

;; This scripts takes a different approach, trying to improve the current
;; move/resize commands that already exist in Sawfish and fixing this
;; bug.

;; You can now set a binding (or a menu entry in the window-menu) for the
;; standard move/resize Sawfish commands and move the window with the
;; keyboard (or also with mouse) and then press enter.

;; The automatic cursor warping wont interfere if you are moving by
;; clicking the title bar nor if you resize clicking the borders.


;;; Installation:

;; Copy this script somewhere in your load-path (~/.sawfish/lisp by
;; default) and add the following line in your ~/.sawfish/rc file.
;; (require 'move-resize-by-key-cursor)


;;; Code:


(defun warp-cursor-to-center-by-key ()
  "If mouse was not used, place the cursor at the center of the
focused window."
  ;; don't move pointer if user started the move command by 
  ;; mouse clicking (any button).
  (unless (string-match "Click" (event-name (last-event)))
    (let ((win-origin (window-position (input-focus)))
          (win-dims (window-dimensions (input-focus))))
      (warp-cursor
       (+ (car win-origin) (quotient (car win-dims) 2))
       (+ (cdr win-origin) (quotient (cdr win-dims) 2)))
      (allow-events 'sync-both) ;makes pointer position take effect immediately
      )))

(defun warp-cursor-to-bottom-right-by-key ()
  "If mouse was not used, place the cursor at the bottom-right of the
focused window."
  ;; don't move pointer if user started the move command by 
  ;; mouse clicking (any button).
  (unless (string-match "Click" (event-name (last-event)))
    (let ((win-origin (window-position (input-focus)))
          (win-dims (window-dimensions (input-focus))))
      (warp-cursor
       (+ (car win-origin) (car win-dims))
       (+ (cdr win-origin) (cdr win-dims)))
      (allow-events 'sync-both) ;makes pointer position take effect immediately
      )))


;; Move Hook. Centers the cursor before moving
;; ONLY when it was not called by a mouse click
(add-hook 'before-move-hook warp-cursor-to-center-by-key)

;; Resize Hook. Moves to bottom-right the cursor before resizing
;; ONLY when it was not called by a mouse click
(add-hook 'before-resize-hook warp-cursor-to-bottom-right-by-key)

