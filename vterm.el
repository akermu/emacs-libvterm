(require 'vterm-module)
(require 'subr-x)

(defvar vterm-term nil
  "Pointer to struct Term.")
(make-variable-buffer-local 'term)

;; TODO Remove this horrible keybinding hack

(defvar vterm-keymap-exceptions '("C-x" "M-o" "C-u")
  "Exceptions for vterm-keymap.

If you use a keybinding with a prefix-key that prefix-key cannot
be send to the terminal.")

(defvar vterm-keymap (let ((keymap (make-sparse-keymap)))
                       (dolist (exception vterm-keymap-exceptions)
                         (define-key keymap (kbd exception) (key-binding exception)))
                       (define-key keymap [t] #'vterm-self-insert)
                       keymap)
  "Keymap for vterm.")

(define-derived-mode vterm-mode fundamental-mode "VTerm"
  "TODO: Documentation."
  (set-transient-map vterm-keymap #'vterm-true))

(defun vterm-true ()
  t)

(defun vterm-self-insert ()
  (interactive)
  (let* ((modifiers (event-modifiers last-input-event))
         (shift (memq 'shift modifiers))
         (meta (memq 'meta modifiers))
         (ctrl (memq 'control modifiers)))
    (message "%s" modifiers)
    (when-let ((key (key-description (vector (event-basic-type last-input-event))))
               (inhibit-redisplay t)
               (inhibit-read-only t))
      (vterm-update vterm-term key shift meta ctrl))))

(defun vterm-create ()
  (interactive)
  (let ((buffer (generate-new-buffer "vterm")))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (vterm-mode)
      (setq vterm-term (vterm-new (window-height) (window-width))
            buffer-read-only t)
      (run-with-timer .1 .1 #'vterm-run-timer buffer))))

(defun vterm-run-timer (buffer)
  (interactive)
  (let ((inhibit-redisplay t)
        (inhibit-read-only t))
    (with-current-buffer buffer
      (vterm-update vterm-term))))

(provide 'vterm)
