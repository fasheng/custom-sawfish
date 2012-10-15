;;
;; window-alpha.jl  0.1 -- script handling window's opacity
;; Copyright 2008 by Michal Nazarewicz (mina86/AT/mina86.com)
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;

;;; Commentary:
;;
;; This script handles windows opacity changing it depending whether
;; window has focus or not but unlike other available scripts it
;; supports a window property so that one can set different opacity
;; for different windows.
;;
;;; Installation:
;;
;; To install put this file as window-alpha.jl in ~/.sawfish/lisp and
;; add (require 'window-alpha) in your configuration file
;; (ie. ~/.sawfishrc).
;;
;; Of course, your X server have to be configured to support window
;; translucancy.  For more information see
;; <http://sawfish.wikia.com/wiki/Enabling_transparency>.
;;
;; (Please note that this file may be out dated.  So if it is 2009
;; when you're reading it you should check out
;; <http://github.com/mina86/dot-files/tree/master/sawfishrc> for
;; updates. :) )
;;
;;; Usage:
;;
;; As soon as you install the script your windows will start changing
;; opacity when they gain/loose focus.  To change default opacity
;; issue a following command:
;;
;;   (setq default-window-alpha (cons focus blur))
;;
;; Wher focus and blur are values from 0.1 to 1.0 specifying how
;; opaque window should be if it has or has not focus.
;;
;; But as promised, you can also change alpha for each individual
;; window but issuing (window-put w 'alpha (cons focus blur)) where
;; w is the window to change alpha of.  For instance this is part of
;; my configuration file:
;;
;; (setq match-window-profile
;;       '((((WM_CLASS . "^Gimp/gimp$")) (alpha . 1))))
;;

(defvar default-window-alpha (cons 0.9 0.7))

(defconst window-alpha-max   #xffffffff)
(defconst window-alpha-min   #x1fffffff)

;; If alpha is a list then:
;; * if blur is nil     takes alpha's car,
;; * if blur is not nil takes alpha's cdr.
;; If now alpha is not a number returns `window-alpha-max'.
;; If alpha is within [0, 1] multiplies it by `window-alpha-max'
;; Returns (max `window-alpha-min' (min `window-alpha-max' alpha))
(defun window-alpha-value (alpha blur)
  (let ((a (if (listp alpha) (if blur (cdr alpha) (car alpha)) alpha)))
    (if (numberp a)
        (max window-alpha-min
             (min window-alpha-max
                  (or (and (>= a 0) (<= a 1) (* a window-alpha-max)) a)))
      window-alpha-max)))

;; Return's window's alpha property or `default-window-alpha'
(defun window-alpha-property (w)
          (or (window-get w 'alpha) default-window-alpha))

;; Sets window opacity.
;; If alpha is nil uses `window-alpha-property'.
;; Uses `window-alpha-value' to get alpha value from alpha and blur arguments.
(defun set-window-alpha (w alpha #!optional blur)
  (let ((a (window-alpha-value (or alpha (window-alpha-property w)) blur)))
    (if (= a window-alpha-max)
        (delete-x-property (window-frame-id w) '_NET_WM_WINDOW_OPACITY))
    (set-x-property (window-frame-id w) '_NET_WM_WINDOW_OPACITY
                    (make-vector 1 a) 'CARDINAL 32))
  (sync-server))

(add-hook 'focus-in-hook  (lambda (w fmode) (set-window-alpha w nil nil)))
(add-hook 'focus-out-hook (lambda (w fmode) (set-window-alpha w nil t  )))
