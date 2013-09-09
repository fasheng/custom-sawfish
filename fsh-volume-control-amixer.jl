;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 2013-09-09, archlinux, test OK on amixer v1.0.27.2,
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'rep.io.timers)

(defun amixer-get ()
  (let* ((stream (make-string-output-stream))
         (process (make-process stream))
         (retval (call-process process nil
                               "/usr/bin/amixer" "get" "Master")))
    (when (and (eql retval 0)
               (string-match "Mono:.*?\\[([^%]*)%\\].*\\[(.*)\\]"
                             (get-output-stream-string stream)))
      (expand-last-match "\\1 [\\2]"))))

(define volume-display-timer
  (make-timer (lambda ()
                (display-message (format nil "Volume: %s" (amixer-get)))
                (make-timer (lambda () (display-message nil)) 1))))

(defun display-volume ()
  (set-timer volume-display-timer 0 300))

(defun amixer-inc (increment)
  (let ((val (if (> increment 0)
                 (format nil "%d%%+" increment)
               (format nil "%d%%-" (* increment -1))))
        (process (make-process standard-output)))
    (start-process process "/usr/bin/amixer" "set" "Master" val)))

(defun amixer-toggle-mute ()
  (let ((process (make-process standard-output)))
    (start-process process "/usr/bin/amixer" "set" "Master" "toggle")))

(define-command 'audio-up
  (lambda ()
    (amixer-inc 5)
    (display-volume)))

(define-command 'audio-down
  (lambda ()
    (amixer-inc -5)
    (display-volume)))

(define-command 'audio-toggle-mute
  (lambda ()
    (amixer-toggle-mute)
    (display-volume)))

(bind-keys global-keymap "XF86AudioRaiseVolume" 'audio-up
                         "XF86AudioLowerVolume" 'audio-down
                         "XF86AudioMute"        'audio-toggle-mute)
