;;; prompt-menu.jl --- Prompt extensions

;; Copyright (C) 2009 Jeremy Hankins
;; $Date: 2009-09-21 15:25:39 -0500 (Mon, 21 Sep 2009) $	
;; $Revision: 856 $	

;; Author: Jeremy Hankins <nowan at nowan dot org>
;; Keywords: prompt

;;; GPL blurb

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary

;; Extensions to the sawfish prompt system.  The main thing this
;; provides is `prompt-menu' which can be used to access a menu via
;; the prompt system.

;;; Code:

(define-structure prompt-menu

    (export prompt-menu
	    prompt-menu-item)

    (open rep
	  rep.system
	  rep.io.processes
	  rep.io.files
	  sawfish.wm
	  sawfish.wm.menus
	  sawfish.wm.util.prompt
	  sawfish.wm.util.with-output)

  ;; Given how much this is used, perhaps it should be a bit more
  ;; rigorous.  But if the options are only menu or menu-item, it
  ;; should work, and this is faster.
  (define (menu-p menu)
    "Simple-minded check to see if `menu' looks like a menu rather
than a menu item."
    (and (consp (car menu)) (stringp (car (car menu)))))

  ;; TODO: For dynamically constructed menus this is much slower than
  ;; it ought to be: it pre-emptively evaluates the item if it might
  ;; be a menu constructor.  To fix this I'll also need to give
  ;; prompt-menu-completor a more intelligent way to know whether an
  ;; item is a menu or not.

  (define (prompt-menu-item menu path)
    "Treat `path' as a slash-separated path through `menu', and return
the object at the end of the path."
    (let ((m (if (menu-p menu)
		 (filter-menu menu)
	       nil))
	  (sep ?/)
	  (len-path (length path)))
      (do ((i 0 (+ 1 i))
	   (start 0))
	  ((> i len-path))
	(if (= i len-path start)             ; if path is empty or ends in /
	    (setq m (and (menu-p m) m))      ;   return nil if m is not a menu
	  (when (or (= i len-path)           ; otherwise if at end of path
		    (eq (aref path i) sep))  ;   or at a /
	    (unless (= i start)              ; unless / at begining of
	                                     ;   path, or double /
	      (let ((item (cdr (assoc (substring path start i) m))))
		(setq m
		      (filter-menu
		       (progn
			 (when (symbolp item)
			   (setq item (symbol-value item)))
			 (cond ((closurep item)
				(funcall item))
			       ((menu-p item)
				item)
			       ((null item)  ; Ignore dividers.
				nil)
			       ((and (listp item) (= 1 (length item)))
				(car item))
			       (t ; What kind of thing does this catch?
				item)))))))
	    (setq start (+ 1 i)))))
      m))

  (define (filter-menu menu)
    "Remove any underscore characters from the entry names in (the top
level of) `menu'; their special meaning doesn't apply here."
    (if (menu-p menu)
	(mapcar (lambda (ent)
		  (if (and (consp ent) (stringp (car ent)))
		      (cons (filter-string (car ent) ?_) (cdr ent))
		    ent))
		menu)
      menu))

  (define (filter-string str char)
    "Return a copy of `str' with all of instances of `char' removed."
    (let ((new ""))
      (do ((i 0 (+ 1 i))
	   (len (length str))
	   (start 0))
	  ((> i len))
	(when (or (= i len)
		  (and (eq (aref str i) char)
		       (not (= i start))))
	  (setq new (concat new (substring str start i))))
	(when (and (not (= i len))
		   (eq (aref str i) char))
	  (setq start (+ 1 i))))
      new))

  (define (prompt-menu-completor menu)
    "Construct a completor function for `menu'."
    (lambda (word)
      (let* ((path (file-name-directory word))
	     (file (file-name-nondirectory word))
	     (item (prompt-menu-item menu path))
	     (files (if (not (menu-p item))
			nil
		      (delete nil (mapcar car item)))))
	(mapcar (lambda (x)
		  (when (menu-p (prompt-menu-item item x))
		    (setq x (concat x ?/)))
		  (concat path x))
		(delete-if (lambda (f)
			     (not (string-head-eq f file)))
			   files)))))

  (define (prompt-menu-abbreviate name)
    "Return the final portion of the path name."
    (let ((abbrev (file-name-nondirectory name)))
      (if (string= abbrev "")
	  (file-name-as-directory
	   (file-name-nondirectory (directory-file-name name)))
	abbrev)))

  (define (prompt-menu menu #!optional title start)
    "Access menu via the prompt mechanisim."
    (let ((item
           (prompt-menu-item
            menu
            (prompt #:title (or title "Menu:")
                    #:start (or start "")
                    #:completion-fun (prompt-menu-completor menu)
                    #:validation-fun (lambda (p)
                                       (and (prompt-menu-item menu p) p))
                    #:abbrev-fun prompt-menu-abbreviate))))
      (cond ((menu-p item)
	     nil)
	    ((commandp item)
	     (call-command item))
	    ((functionp item)
	     (item))
	    ((consp item)
	     (user-eval item)))))

  )

;;; prompt-menu.jl ends here
