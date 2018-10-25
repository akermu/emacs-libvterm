;;; vterm.el --- This package implements a terminal via libvterm

;;; Commentary:
;;
;; This Emacs module implements a bridge to libvterm to display a terminal in a
;; Emacs buffer.

;;; Code:

(defun vterm-module-compile ()
  "This function compiles the vterm-module."
  (interactive)
  (let ((buffer (get-buffer-create " *Install vterm"))
        (default-directory (file-name-directory (locate-library "vterm"))))
    (make-process
     :name "build-vterm-module"
     :buffer buffer
     :command '("sh" "-c" "mkdir -p build; cd build; cmake -DCMAKE_BUILD_TYPE=RelWithDebInfo ..; make")
     :stderr buffer
     :sentinel (lambda (process status)
                 (if (equal status "finished\n")
                     (progn (let ((buffer (process-buffer process)))
                              (bury-buffer buffer)
                              (when-let ((window (get-buffer-window buffer)))
                                (delete-window window)))
                            (message "Sucessfully compiled the emacs-libvterm module."))
                   (message "Compilation of libvterm has failed."))))
    (pop-to-buffer buffer)))

(when (boundp 'vterm-install)
  (vterm-module-compile))

(require 'vterm-module)
(require 'subr-x)
(require 'cl-lib)
(require 'color)

(defcustom vterm-shell (getenv "SHELL")
  "The shell that gets run in the vterm."
  :type 'string
  :group 'vterm)

(defcustom vterm-max-scrollback 1000
  "Maximum 'scrollback' value."
  :type 'number
  :group 'vterm)

(defcustom vterm-keymap-exceptions '("C-x" "C-u" "C-g" "C-h" "M-x" "M-o" "C-v" "M-v")
  "Exceptions for vterm-keymap.

If you use a keybinding with a prefix-key that prefix-key cannot
be send to the terminal."
  :type '(repeat string)
  :group 'vterm)

(defface vterm
  '((t :inherit default))
  "Default face to use in Term mode."
  :group 'vterm)

(defface vterm-color-black
  '((t :foreground "black" :background "black"))
  "Face used to render black color code."
  :group 'vterm)

(defface vterm-color-red
  '((t :foreground "red3" :background "red3"))
  "Face used to render red color code."
  :group 'vterm)

(defface vterm-color-green
  '((t :foreground "green3" :background "green3"))
  "Face used to render green color code."
  :group 'vterm)

(defface vterm-color-yellow
  '((t :foreground "yellow3" :background "yellow3"))
  "Face used to render yellow color code."
  :group 'vterm)

(defface vterm-color-blue
  '((t :foreground "blue2" :background "blue2"))
  "Face used to render blue color code."
  :group 'vterm)

(defface vterm-color-magenta
  '((t :foreground "magenta3" :background "magenta3"))
  "Face used to render magenta color code."
  :group 'vterm)

(defface vterm-color-cyan
  '((t :foreground "cyan3" :background "cyan3"))
  "Face used to render cyan color code."
  :group 'vterm)

(defface vterm-color-white
  '((t :foreground "white" :background "white"))
  "Face used to render white color code."
  :group 'vterm)

(defvar vterm--term nil
  "Pointer to Term.")
(make-variable-buffer-local 'vterm--term)

(defvar vterm--process nil
  "Shell process of current term.")
(make-variable-buffer-local 'vterm--process)

(define-derived-mode vterm-mode fundamental-mode "VTerm"
  "Mayor mode for vterm buffer."
  (buffer-disable-undo)
  (setq vterm--term (vterm--new (window-body-height)
                                (window-body-width)
                                vterm-max-scrollback))
  (cl-loop repeat (1- (window-body-height)) do
           (insert "\n"))

  (setq buffer-read-only t)
  (setq-local scroll-conservatively 101)
  (setq-local scroll-margin 0)

  (add-hook 'window-size-change-functions #'vterm--window-size-change t t)
  (let ((process-environment (append '("TERM=xterm") process-environment)))
    (setq vterm--process (make-process
                          :name "vterm"
                          :buffer (current-buffer)
                          :command `("/bin/sh" "-c" ,(format "stty -nl sane iutf8 rows %d columns %d >/dev/null && exec %s" (window-body-height) (window-body-width) vterm-shell))
                          :coding 'no-conversion
                          :connection-type 'pty
                          :filter #'vterm--filter
                          :sentinel #'ignore))))

;; Keybindings
(define-key vterm-mode-map [t] #'vterm--self-insert)
(define-key vterm-mode-map [mouse-1] nil)
(define-key vterm-mode-map [mouse-2] nil)
(define-key vterm-mode-map [mouse-3] nil)
(define-key vterm-mode-map [mouse-4] #'ignore)
(define-key vterm-mode-map [mouse-5] #'ignore)
(dolist (prefix '("M-" "C-"))
  (dolist (char (cl-loop for char from ?a to ?z
                         collect char))
    (let ((key (concat prefix (char-to-string char))))
      (unless (cl-member key vterm-keymap-exceptions)
        (define-key vterm-mode-map (kbd key) #'vterm--self-insert)))))
(dolist (exception vterm-keymap-exceptions)
  (define-key vterm-mode-map (kbd exception) nil))

(defun vterm--self-insert ()
  "Sends invoking key to libvterm."
  (interactive)
  (when vterm--term
    (let* ((modifiers (event-modifiers last-input-event))
           (shift (memq 'shift modifiers))
           (meta (memq 'meta modifiers))
           (ctrl (memq 'control modifiers)))
      (when-let ((key (key-description (vector (event-basic-type last-input-event))))
                 (inhibit-redisplay t)
                 (inhibit-read-only t))
        (when (equal modifiers '(shift))
          (setq key (upcase key)))
        (vterm--update vterm--term key shift meta ctrl)))))

(defun vterm ()
  "Create a new vterm."
  (interactive)
  (let ((buffer (generate-new-buffer "vterm")))
    (with-current-buffer buffer
      (vterm-mode))
    (switch-to-buffer buffer)))

(defun vterm-other-window ()
  "Create a new vterm."
  (interactive)
  (let ((buffer (generate-new-buffer "vterm")))
    (with-current-buffer buffer
      (vterm-mode))

    (pop-to-buffer buffer)))

(defun vterm--flush-output (output)
  "Sends the virtual terminal's OUTPUT to the shell."
  (process-send-string vterm--process output))

(defun vterm--filter (process input)
  "I/O Event. Feeds PROCESS's INPUT to the virtual terminal.

Then triggers a redraw from the module."
  (with-current-buffer (process-buffer process)
    (vterm--write-input vterm--term input)
    (let ((inhibit-read-only t)
          (inhibit-redisplay t))
      (vterm--update vterm--term))))

(defun vterm--window-size-change (frame)
  "Callback triggered by a size change of the FRAME.

Feeds the size change to the virtual terminal."
  (dolist (window (window-list frame))
    (with-current-buffer (window-buffer window)
      (when (and (processp vterm--process)
                 (process-live-p vterm--process))
        (let ((height (window-body-height window))
              (width (window-body-width window)))
          (set-process-window-size vterm--process height width)
          (vterm--set-size vterm--term height width))))))

(defun vterm--face-color-hex (face attr)
  "Return the color of the FACE's ATTR as a hex string."
  (if (< emacs-major-version 26)
      (apply #'color-rgb-to-hex (color-name-to-rgb (face-attribute face attr nil 'default)))
    (apply #'color-rgb-to-hex (append (color-name-to-rgb (face-attribute face attr nil 'default)) '(2)))))


(defun vterm--delete-lines (line-num count &optional delete-whole-line)
  "Delete lines from line-num. If option ‘kill-whole-line’ is non-nil,
 then this command kills the whole line including its terminating newline"
  (ignore-errors
    (save-excursion
      (when (vterm--goto-line line-num)
        (delete-region (point) (point-at-eol))
        (when delete-whole-line
          (when (looking-at "\n")
            (delete-char 1))
          (when (and (eobp) (looking-back "\n"))
            (delete-char -1))

          )
        (cl-loop repeat (1- count) do
                 (when (or delete-whole-line (forward-line 1))
                   (delete-region (point) (point-at-eol))
                   (when delete-whole-line
                     (when (looking-at "\n")
                       (delete-char 1))
                     (when (and (eobp) (looking-back "\n"))
                       (delete-char -1)))
                   ))))))



(defun vterm--recenter(&optional arg)
  (when (get-buffer-window)
    (with-current-buffer (window-buffer)
      (when vterm--term
        (recenter arg)))))

(defun vterm--goto-line(n)
  "If move succ return t"
  (ignore-errors
    (goto-char (point-min))
    (let ((succ (eq 0 (forward-line (1- n)))))
      succ)))

(defun vterm--buffer-line-num()
  (ignore-errors
  (save-excursion
    (goto-char (point-max))
    (line-number-at-pos))))

(defun vterm--forward-char(n )
  (ignore-errors
    (forward-char n)))

(provide 'vterm)
;;; vterm.el ends here
