;;; vterm.el --- This package implements a terminal via libvterm

;;; Commentary:
;;
;; This Emacs module implements a bridge to libvterm to display a terminal in a
;; Emacs buffer.

;;; Code:

(require 'vterm-module)
(require 'subr-x)
(require 'cl-lib)

(defvar vterm-term nil
  "Pointer to struct Term.")
(make-variable-buffer-local 'vterm-term)

(defvar vterm-timer nil
  "Timer to update the term.")
(make-variable-buffer-local 'vterm-timer)

(defvar vterm-keymap-exceptions '("C-x" "C-u" "C-g" "C-h" "M-x" "M-o")
  "Exceptions for vterm-keymap.

If you use a keybinding with a prefix-key that prefix-key cannot
be send to the terminal.")

(define-derived-mode vterm-mode fundamental-mode "VTerm"
  "Mayor mode for vterm buffer."
  (buffer-disable-undo)
  (setq vterm-term (vterm-new (window-height) (window-width))
        buffer-read-only t)
  (setq-local scroll-conservatively 101)
  (setq-local scroll-margin 0)
  (add-hook 'kill-buffer-hook #'vterm-kill-buffer-hook t t)
  (add-hook 'window-size-change-functions #'vterm-window-size-change t t))

;; Keybindings
(define-key vterm-mode-map [t] #'vterm-self-insert)
(dolist (prefix '("M-" "C-"))
  (dolist (char (cl-loop for char from ?a to ?z
                         collect char))
    (let ((key (concat prefix (char-to-string char))))
      (unless (cl-member key vterm-keymap-exceptions)
        (define-key vterm-mode-map (kbd key) #'vterm-self-insert)))))
(dolist (exception vterm-keymap-exceptions)
  (define-key vterm-mode-map (kbd exception) nil))

(defun vterm-self-insert ()
  "Sends invoking key to libvterm."
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
  "Create a new vterm."
  (interactive)
  (let ((buffer (generate-new-buffer "vterm")))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (vterm-mode)
      (setq vterm-timer (run-with-timer 0 .1 #'vterm-run-timer buffer)))))

(defun vterm-run-timer (buffer)
  "Update the vterm BUFFER."
  (interactive)
  (let ((inhibit-redisplay t)
        (inhibit-read-only t))
    (with-current-buffer buffer
      (unless (vterm-update vterm-term)
        (cancel-timer vterm-timer)
        (insert "\nProcess exited!\n\n")))))

(defun vterm-kill-buffer-hook ()
  "Cancel the timer and the vterm."
  (when (eq major-mode 'vterm-mode)
    (cancel-timer vterm-timer)
    (vterm-kill vterm-term)))

(defun vterm-window-size-change (frame)
  "Notify the vterm over size-change in FRAME."
  (dolist (window (window-list frame 1))
    (let ((buffer (window-buffer window)))
      (with-current-buffer buffer
        (when (eq major-mode 'vterm-mode)
          (vterm-set-size vterm-term (window-height) (window-width)))))))

(provide 'vterm)
;;; vterm.el ends here
