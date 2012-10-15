#|

Shutdown Sawfish Module.  Version 1.0.

Copyright (c) 2003 Ewan Mellor <sawfish@ewanmellor.org.uk>.
All rights reserved.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.


This module contains one function -- shutdown -- which shuts down each window
listed in the configuration variable *shutdown-windows*.  It does this by
sending the configured keypress to the window matching the given regular
expression, and then waiting for that window to be destroyed.  This means
that, for example, Opera can be shutdown cleanly on logout, and won't complain
when you start back up again.


Install this file by placing it inside your ~/.sawfish/lisp/ directory,
and then use the shutdown function wherever you have your log-out
functionality.  I have the following lines in my ~/.sawfish/rc:

(require 'shutdown)
(nconc root-menu '(("_Quit" (shutdown quit))))

This adds a "Quit" option to the root menu, and calls shutdown and
then (quit) with that menu option.  Notice the continuation-style handling of
what to call once the windows have been destroyed.

|#


(define-structure shutdown
  (export shutdown
           shutting-window
          *shutdown-windows*
          )

  (open rep
        rep.system
        sawfish.wm
        )

(defvar *shutdown-windows*
  '(("Opera " . "C-q"))
  
  "Each window to shut down, and the keystroke used to shut them down.  Should
be of the form
     '((\"Win regexp 1\" . \"Keystroke 1\")
       (\"Win regexp 2\" . \"Keystroke 2\")
       ...
      )"
  )


(define shutting-window nil)
(define shutting-continuation nil)


;; A hook added to destroy-notify-hook.  This calls the next continuation
;; (shutting-continuation) once shutting-window has been destroyed.
(define (destroy-hook w)
  (when (= w shutting-window)
    (remove-hook 'destroy-notify-hook destroy-hook)
    (setq shutting-window nil)
    (when shutting-continuation
      (shutting-continuation)
      )
    )
  )


;; Close the window matching the regexp win-re, using the keystroke given, and
;; then call the given continuation, if any.  If there is a window matching
;; win-re then this is achieved by saving the continuation as
;; shutting-continuation to be picked up by destroy-hook.  Otherwise, the
;; continuation is called immediately.
(define (close win-re keystroke continuation)
  (let ((window (get-window-by-name-re win-re)))
    (cond
     (window
      (setq shutting-window window)
      (setq shutting-continuation continuation)
      (add-hook 'destroy-notify-hook destroy-hook)
      (synthesize-event keystroke window)
      )
     
     (continuation
      (continuation)
      )
     )
    )
  )


;; Close each window in the list given, then call the given continuation.
;; The list should be in the same form as *shutdown-windows*.
(define (shutdown-list l continuation)
  (when l
    (let* ((thiscon (if (= 1 (length l))
                        continuation
                      '(shutdown-list (cdr l) continuation)))
           (winkey (car l))
           (win (car winkey))
           (key (cdr winkey)))
      (close win key thiscon)
      )
    )
  )

  
(define (shutdown continuation)
  "Shutdown each window listed in *shutdown-windows*, then call the
continuation given.  The continuation may be nil."
  (interactive)
  (when (not shutting-window)
    (shutdown-list *shutdown-windows* continuation)
    )
  )
)
