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

(defcustom vterm-keymap-exceptions '("C-c" "C-x" "C-u" "C-g" "C-h" "M-x" "M-o" "C-v" "M-v")
  "Exceptions for vterm-keymap.

If you use a keybinding with a prefix-key, add that prefix-key to
this list. Note that after doing so that prefix-key cannot be sent
to the terminal anymore."
  :type '(repeat string)
  :group 'vterm)

(defcustom vterm-exit-hook nil
  "Shell exit hook.

This hook applies only to new vterms, created after setting this
value with `add-hook'.

Note that this hook will not work if another package like
`shell-pop' sets its own sentinel to the `vterm' process."
  :type 'hook
  :group 'vterm)

(defcustom vterm-set-title-hook nil
  "Shell set title hook.

those functions are called one by one, with 1 arguments.
`vterm-set-title-hook' should be a symbol, a hook variable.
The value of HOOK may be nil, a function, or a list of functions.
for example
(defun vterm--rename-buffer-as-title (title)
  (rename-buffer (format \"vterm %s\" title)))
(add-hook 'vterm-set-title-hook 'vterm--rename-buffer-as-title)

see http://tldp.org/HOWTO/Xterm-Title-4.html about how to set terminal title
for different shell. "
  :type 'hook
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
                          :sentinel (when vterm-exit-hook #'vterm--sentinel)))))

;; Keybindings
(define-key vterm-mode-map [tab]                       #'vterm--self-insert)
(define-key vterm-mode-map [backspace]                 #'vterm--self-insert)
(define-key vterm-mode-map [M-backspace]               #'vterm--self-insert)
(define-key vterm-mode-map [return]                    #'vterm--self-insert)
(define-key vterm-mode-map [left]                      #'vterm--self-insert)
(define-key vterm-mode-map [right]                     #'vterm--self-insert)
(define-key vterm-mode-map [up]                        #'vterm--self-insert)
(define-key vterm-mode-map [down]                      #'vterm--self-insert)
(define-key vterm-mode-map [home]                      #'vterm--self-insert)
(define-key vterm-mode-map [end]                       #'vterm--self-insert)
(define-key vterm-mode-map [escape]                    #'vterm--self-insert)
(define-key vterm-mode-map [remap self-insert-command] #'vterm--self-insert)
(define-key vterm-mode-map (kbd "C-c C-c")             #'vterm-send-ctrl-c)

;; Function keys and most of C- and M- bindings
(mapcar (lambda (key)
          (define-key vterm-mode-map (kbd key) #'vterm--self-insert))
        (append (cl-loop for number from 1 to 12
                         collect (format "<f%i>" number))
                (cl-loop for prefix in '("C-" "M-")
                         append (cl-loop for char from ?a to ?z
                                         for key = (format "%s%c" prefix char)
                                         unless (member key vterm-keymap-exceptions)
                                         collect key))))

(defun vterm--self-insert ()
  "Sends invoking key to libvterm."
  (interactive)
  (when vterm--term
    (let* ((modifiers (event-modifiers last-input-event))
           (shift (memq 'shift modifiers))
           (meta (memq 'meta modifiers))
           (ctrl (memq 'control modifiers)))
      (when-let ((key (key-description (vector (event-basic-type last-input-event)))))
        (vterm-send-key key shift meta ctrl)))))

(defun vterm-send-key (key &optional shift meta ctrl)
  "Sends KEY to libvterm with optional modifiers SHIFT, META and CTRL."
  (when vterm--term
    (let ((inhibit-redisplay t)
          (inhibit-read-only t))
      (when (and shift (not meta) (not ctrl))
        (setq key (upcase key)))
      (vterm--update vterm--term key shift meta ctrl))))

(defun vterm-send-ctrl-c ()
  "Sends C-c to the libvterm."
  (interactive)
  (vterm-send-key "c" nil nil t))

;;;###autoload
(defun vterm ()
  "Create a new vterm."
  (interactive)
  (let ((buffer (generate-new-buffer "vterm")))
    (with-current-buffer buffer
      (vterm-mode))
    (switch-to-buffer buffer)))

;;;###autoload
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
  (let ((inhibit-redisplay t)
        (inhibit-read-only t))
    (with-current-buffer (process-buffer process)
      (vterm--write-input vterm--term input)
      (vterm--update vterm--term))))

(defun vterm--sentinel (process event)
  "Sentinel of vterm PROCESS."
  (when (not (process-live-p process))
    (let ((buf (process-buffer process)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (run-hooks 'vterm-exit-hook))))))

(defun vterm--window-size-change (frame)
  "Callback triggered by a size change of the FRAME.

Feeds the size change to the virtual terminal."
  (dolist (window (window-list frame))
    (with-current-buffer (window-buffer window)
      (when (and (processp vterm--process)
                 (process-live-p vterm--process))
        (let ((height (window-body-height window))
              (width (window-body-width window))
              (inhibit-read-only t))
          (set-process-window-size vterm--process height width)
          (vterm--set-size vterm--term height width))))))

(defun vterm--face-color-hex (face attr)
  "Return the color of the FACE's ATTR as a hex string."
  (if (< emacs-major-version 26)
      (apply #'color-rgb-to-hex (color-name-to-rgb (face-attribute face attr nil 'default)))
    (apply #'color-rgb-to-hex (append (color-name-to-rgb (face-attribute face attr nil 'default)) '(2)))))


(defun vterm--delete-lines (line-num count &optional delete-whole-line)
  "Delete COUNT lines from LINE-NUM.

 If option DELETE-WHOLE-LINE is non-nil, then this command kills
 the whole line including its terminating newline"
  (save-excursion
    (when (vterm--goto-line line-num)
      (delete-region (point) (point-at-eol count))
      (when (and delete-whole-line
                 (looking-at "\n"))
        (delete-char 1)))))

(defun vterm--goto-line(n)
  "Go to line N and return true on success."
  (goto-char (point-min))
  (let ((succ (eq 0 (forward-line (1- n)))))
    succ))

(defun vterm--buffer-line-num()
  "Return the maximum line number."
  (line-number-at-pos (point-max)))

(defun vterm--set-title (title)
  "Run the `vterm--set-title-hook' with TITLE as argument."
  (run-hook-with-args 'vterm-set-title-hook title))


(provide 'vterm)
;;; vterm.el ends here
