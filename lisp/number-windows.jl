;;;; number-windows.jl -- Add numbers to windows like emacs buffers

;; Copyright (C) 2000 Peter Makholm <peter@makholm.net>
;; $Id: number-windows.jl,v 1.1 2000/10/06 15:33:01 brother Exp $
;;
;; "THE BEER-WARE LICENSE" (Revision 42): 
;; <peter@makholm.net> wrote this file. As long as you retain this
;; notice you can do whatever you want with this stuff. If we meet some
;; day, and you think this stuff is worth it, you can buy me a beer in
;; return.
;;        Peter Makholm
;;
;; What: 
;;      This code names you windows in the same way Emacs names it's
;;      buffers. That is if you try to open two windows with the name
;;      "xterm" the second one will be named "xterm<1>"
;;
;;      Should be compatible wit Emacs's generate-new-buffer-name
;;
;; Use:
;;      1) Put this file in you ~/.sawfish/lisp
;;         or anywhere else on you load-path
;;      2) Add a "(require 'number-windows)" to you ~/.sawfishrc
;;      3) Open a lot of windows
;;



(provide 'number-windows)

(defun pm-number-window (w) 
  "Give window w a uniq name by adding a number just like emacs buffers"
  (let* ((np (get-x-text-property w 'WM_NAME))
	 (name (aref np 0))
	 (nexp1 (concat (quote-regexp name) "($|<([0-9]*)>)"))
	 (winlist (managed-windows))
	 (numberlist nil)
	 (number 0))
    (while winlist
	   (let* ((w0 (car winlist))
		  (w0-name (aref (get-x-text-property w0 'WM_NAME) 0)))
	     (if (not (equal w w0))
		 (if (string-looking-at nexp1 w0-name)
		     (setq numberlist (cons
				       (read-from-string (concat "#d0" (expand-last-match "\\2")))
				       numberlist)))))
	   (setq winlist (cdr winlist)))
    (setq numberlist (sort numberlist))
    (while numberlist
	   (if (< number (car numberlist))
	       (setq numberlist nil)
	       (progn
		(setq numberlist (cdr numberlist))
		(setq number (1+ number))
		)
	       ))
    (if (< 0 number)
	(setq name (concat name "<" (prin1-to-string number) ">")))
    (aset np 0 name)
    (set-x-text-property w 'WM_NAME np)))


(defun pm-window-property-changed (w an type)
  "property-notify-hook callback. Checks for WM_NAME being set"
  (when (and (eq an 'WM_NAME) (eq type 'new-value))
	(pm-number-window w)))

(add-hook 'property-notify-hook pm-window-property-changed t)
(add-hook 'add-window-hook pm-number-window t)
