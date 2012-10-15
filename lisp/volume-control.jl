(require 'rep.io.timers)

(defun rexima-get ()
  (let* ((stream (make-string-output-stream))
         (process (make-process stream))
         (retval (call-process process nil
                               "/usr/bin/rexima" "-v")))
    (when (and (eql retval 0)
               (string-match "^vol\\s+(\\d+)"
                             (get-output-stream-string stream)))
      (string->number (expand-last-match "\\1")))))

(define volume-display-timer
  (make-timer (lambda ()
                (display-message (format nil "Volume: %d" (rexima-get)))
                (make-timer (lambda () (display-message nil)) 1))))

(defun display-volume ()
  (set-timer volume-display-timer 0 500))

(defun rexima-inc (increment)
  (let ((val (format nil "%+d" increment))
        (process (make-process standard-output)))
    (start-process process "/usr/bin/rexima" "vol" val)))

(define-command 'audio-up
  (lambda ()
    (rexima-inc 5)
    (display-volume)))

(define-command 'audio-down
  (lambda ()
    (rexima-inc -5)
    (display-volume)))
