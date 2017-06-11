(require 'vterm-module)
(require 'subr-x)

(defvar vterm-term nil
  "Pointer to struct Term.")
(make-variable-buffer-local 'term)

(defvar vterm-keymap-exceptions '("C-x" "M-o" "C-u" "C-g")
  "Exceptions for vterm-keymap.

If you use a keybinding with a prefix-key that prefix-key cannot
be send to the terminal.")

(define-derived-mode vterm-mode fundamental-mode "VTerm"
  "TODO: Documentation.")

(dolist (exception vterm-keymap-exceptions)
  (define-key vterm-mode-map (kbd exception) (key-binding (kbd exception))))
(define-key vterm-mode-map [t] #'vterm-self-insert)

(defun vterm-self-insert ()
  (interactive)
  (let* ((modifiers (event-modifiers last-input-event))
         (shift (memq 'shift modifiers))
         (meta (memq 'meta modifiers))
         (ctrl (memq 'control modifiers)))
    (when-let ((key (key-description (vector (event-basic-type last-input-event))))
               (inhibit-redisplay t)
               (inhibit-read-only t))
      (when (equal modifiers '(shift))
        (setq key (upcase key)))
      (vterm-update vterm-term key shift meta ctrl))))

(defun vterm-create ()
  (interactive)
  (let ((buffer (generate-new-buffer "vterm")))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (vterm-mode)
      (setq vterm-term (vterm-new (window-height) (window-width))
            buffer-read-only t)
      (setq-local scroll-conservatively 101)
      (setq-local scroll-margin 0)
      (run-with-timer .1 .1 #'vterm-run-timer buffer))))

(defun vterm-run-timer (buffer)
  (interactive)
  (let ((inhibit-redisplay t)
        (inhibit-read-only t))
    (with-current-buffer buffer
      (vterm-update vterm-term))))

(provide 'vterm)
