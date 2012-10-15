
(define (wy-describe-symbol fun #!optional stream)
  "Display the documentation of a specified symbol into stream."
  (describe-value (symbol-value fun t) fun)
  (format (if stream stream standard-output)
          "\n%s\n"
          (or (documentation fun nil (symbol-value fun t)) "Undocumented.")))

(define (wy-describe-key key #!optional map stream)
    "Print key's binding in keymap map, to stream stream."
    (require 'rep.lang.doc)
    (require 'sawfish.wm.commands.describe)
    (let (components)
      (letrec
      ((loop
        (lambda (keymap)
          (let* ((binding (and key (search-keymap key keymap))))
        (when binding
          (setq binding (car binding))
          (format (or stream standard-output) "\n%s:\n" map)
          (setq components (concat components
                                   (and components ? )
                                   (event-name key)))
          (cond ((keymapp binding)
                 (loop binding))
                ((and (symbolp binding)
                      (keymapp (symbol-value binding t)))
                 (loop (symbol-value binding)))
                (t
                 (format (or stream standard-output)
                         "`%s' is bound to `%s'"
                         components binding)
                 (setq command binding)
                 (while (car command)
                   (setq command (car command)))
                 (wy-describe-symbol command stream))))))))
      (loop (or map global-keymap)))))

(require 'sawfish.wm.util.keymap)
(defun wy-describe-what-am-i-doing ()
  "Read a event and search the keymaps for it's definitions. Detailed."
  (interactive)
  (let ((s (make-string-output-stream))
        (key (read-event (concat "Describe key: "))))
    (mapc
     (lambda (map)
       (wy-describe-key key map s))
     '(global-keymap
       window-keymap
       title-keymap
       root-window-keymap
       border-keymap
       close-button-keymap
       iconify-button-keymap
       maximize-button-keymap
       menu-button-keymap
       ))
    (setq msg (get-output-stream-string s))
    (if (> (length msg) 0)
        (display-message msg
                         `((background . ,tooltips-background-color)
                           (foreground . ,tooltips-foreground-color)
                           (x-justify . left)
                           (spacing . 2)
                           (font . ,tooltips-font))
      (display-message (concat
                        "key `"
                        (prin1-to-string (event-name key))
                        "' is not bound"
                        ))))))

(setq tooltips-background-color (get-color "black"))
(setq tooltips-foreground-color (get-color "red"))
(setq tooltips-font (get-font "-*-lucida-medium-r-*-*-14-*-*-*-*-*-*-*"))
