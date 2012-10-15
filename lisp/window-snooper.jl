;;; Window Snooping

;; doesn't work without this:
(require 'rep.io.timers)
;; and you forgotten this:
(define (display-message-with-timeout message timeout)
  (display-message message)
  (make-timer (lambda () (display-message nil)) timeout))

(defvar window-snooper-timeout 15)

(define (get-wm-name)
  (setq net-wm-name (get-x-text-property (input-focus) '_NET_WM_NAME)))

(define (get-wm-class)
  (setq wm-class (get-x-text-property (input-focus) 'WM_CLASS)))

(define (get-wm-icon-name)
  (setq net-wm-icon-name (get-x-text-property (input-focus) '_NET_WM_ICON_NAME)))

(define (get-wm-role)
  (setq wm-window-role (get-x-text-property (input-focus) 'WM_WINDOW_ROLE)))

(define (get-wm-locale-name)
  (setq wm-locale-name (get-x-text-property (input-focus) 'WM_LOCALE_NAME)))

(define (get-window-pos)
  (setq window-x (car (window-position (input-focus))))
  (setq window-y (cdr (window-position (input-focus)))))

(define (get-window-dims)
  (setq window-width (car (window-dimensions (input-focus))))
  (setq window-height (cdr (window-dimensions (input-focus)))))

(define (get-frame-dims)
  (setq frame-width (- (car (window-frame-dimensions (input-focus))) (car (window-dimensions (input-focus)))))
  (setq frame-height (- (cdr (window-frame-dimensions (input-focus))) (cdr (window-dimensions (input-focus))))))

(define (get-window-infos)
  (get-wm-name)
  (get-wm-class)
  (get-wm-icon-name)
  (get-wm-role)
  (get-wm-locale-name)
  (get-window-pos)
  (get-window-dims)
  (get-frame-dims))

(define (window-snooper)
  (get-window-infos)
  (display-message-with-timeout
   (format nil "About the currently focused window:\
	\n\n===================================\
	\n\n _NET_WM_NAME:\t\t %s\
	\nWM_CLASS:\t\t\t %s\
	\n_NET_WM_ICON_NAME:\t %s\
	\nWM_WINDOW_ROLE:\t %s\
	\nWM_LOCALE_NAME:\t %s\
	\nWindow X:\t\t\t %s pixels\
	\nWindow Y:\t\t\t %s pixels\
	\nWindow Width:\t\t %s pixels\
	\nWindow Height:\t\t %s pixels\
	\nFrame Width:\t\t %s pixels\
	\nFrame Height:\t\t %s pixels\
	\n\n==================================="
           net-wm-name
           wm-class
           net-wm-icon-name
           wm-window-role
           wm-locale-name
           window-x
           window-y
           window-width
           window-height
           frame-width
           frame-height)
   window-snooper-timeout))

(define-command 'window-snooper window-snooper)
