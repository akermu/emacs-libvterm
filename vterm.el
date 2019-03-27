;;; vterm.el --- This package implements a terminal via libvterm

;;; Commentary:
;;
;; This Emacs module implements a bridge to libvterm to display a terminal in a
;; Emacs buffer.

;;; Code:

(require 'ansi-color)

(defvar vterm-install-buffer-name " *Install vterm"
  "Name of the buffer used for compiling vterm-module.")

;;;###autoload
(defun vterm-module-compile ()
  "This function compiles the vterm-module."
  (interactive)
  (let ((default-directory (file-name-directory (locate-library "vterm"))))
    (unless (file-executable-p (concat default-directory "vterm-module.so" ))
      (let* ((buffer (get-buffer-create vterm-install-buffer-name))
             (status (call-process "sh" nil buffer t "-c"
                                   "mkdir -p build;                             \
                                    cd build;                                   \
                                    cmake -DCMAKE_BUILD_TYPE=RelWithDebInfo ..; \
                                    make")))
        (if (eq status 0)
            (message "Compilation of emacs-libvterm module succeeded")
          (pop-to-buffer vterm-install-buffer-name)
          (error "Compilation of emacs-libvterm module failed!"))))))

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

(defcustom vterm-keymap-exceptions '("C-c" "C-x" "C-u" "C-g" "C-h" "M-x" "M-o" "C-v" "M-v" "C-y")
  "Exceptions for vterm-keymap.

If you use a keybinding with a prefix-key, add that prefix-key to
this list. Note that after doing so that prefix-key cannot be sent
to the terminal anymore."
  :type '(repeat string)
  :group 'vterm)

(defcustom vterm-exit-functions nil
  "Shell exit hook.

This hook applies only to new vterms, created after setting this
value with `add-hook'.

Note that this hook will not work if another package like
`shell-pop' sets its own sentinel to the `vterm' process."
  :type 'hook
  :group 'vterm)

(defcustom vterm-set-title-functions nil
  "Shell set title hook.

those functions are called one by one, with 1 arguments.
`vterm-set-title-functions' should be a symbol, a hook variable.
The value of HOOK may be nil, a function, or a list of functions.
for example
(defun vterm--rename-buffer-as-title (title)
  (rename-buffer (format \"vterm %s\" title)))
(add-hook 'vterm-set-title-functions 'vterm--rename-buffer-as-title)

see http://tldp.org/HOWTO/Xterm-Title-4.html about how to set terminal title
for different shell. "
  :type 'hook
  :group 'vterm)

(defvar vterm--term nil
  "Pointer to Term.")
(make-variable-buffer-local 'vterm--term)

(defvar vterm--process nil
  "Shell process of current term.")
(make-variable-buffer-local 'vterm--process)

(defvar vterm-old-mode-map nil "Saves the old keymap when in char mode.")

(define-derived-mode vterm-mode fundamental-mode "VTerm"
  "Major mode for vterm buffer."
  (buffer-disable-undo)
  (setq vterm--term (vterm--new (window-body-height)
                                (window-body-width)
                                vterm-max-scrollback))

  (setq buffer-read-only t)
  (setq-local scroll-conservatively 101)
  (setq-local scroll-margin 0)
  (make-local-variable 'vterm-old-mode-map)
  (vterm-char-mode)

  (add-hook 'window-size-change-functions #'vterm--window-size-change t t)
  (let ((process-environment (append '("TERM=xterm"
                                       "INSIDE_EMACS=vterm"
                                       "LINES"
                                       "COLUMNS")
                                     process-environment))
        (process-adaptive-read-buffering nil))
    (setq vterm--process
          (make-process
           :name "vterm"
           :buffer (current-buffer)
           :command `("/bin/sh" "-c"
                      ,(format "stty -nl sane iutf8 rows %d columns %d >/dev/null && exec %s"
                               (window-body-height)
                               (window-body-width)
                               vterm-shell))
           :coding 'no-conversion
           :connection-type 'pty
           :filter #'vterm--filter
           :sentinel (when vterm-exit-functions #'vterm--sentinel)))))
(defvar vterm-common-map  (make-sparse-keymap))
(defvar vterm-raw-map  (make-sparse-keymap))

;; Keybindings
(define-key vterm-raw-map    [tab]                       #'vterm--self-insert)
(define-key vterm-raw-map    [return]                    #'vterm--self-insert)
(define-key vterm-raw-map    [left]                      #'vterm--self-insert)
(define-key vterm-raw-map    [right]                     #'vterm--self-insert)
(define-key vterm-raw-map    [up]                        #'vterm--self-insert)
(define-key vterm-raw-map    [down]                      #'vterm--self-insert)
(define-key vterm-raw-map    [home]                      #'vterm--self-insert)
(define-key vterm-raw-map    [end]                       #'vterm--self-insert)
(define-key vterm-raw-map    [escape]                    #'vterm--self-insert)
(define-key vterm-raw-map    (kbd "C-c C-y")             #'vterm--self-insert)
(define-key vterm-raw-map    (kbd "C-c C-c")             #'vterm-send-ctrl-c)
(define-key vterm-raw-map    (kbd "C-SPC")               #'vterm--self-insert)

(define-key vterm-common-map [M-backspace]               #'vterm--self-insert)
(define-key vterm-common-map [backspace]                 #'vterm--self-insert)
(define-key vterm-common-map [remap yank]                #'vterm-yank)
(define-key vterm-common-map (kbd "C-_")                 #'vterm--self-insert)
(define-key vterm-common-map (kbd "C-/")                 #'vterm-undo)
(define-key vterm-common-map [remap self-insert-command] #'vterm--self-insert)

;; Function keys and most of C- and M- bindings
(mapcar (lambda (key)
          (define-key vterm-raw-map (kbd key) #'vterm--self-insert))
        (append (cl-loop for number from 1 to 12
                         for key = (format "<f%i>" number)
                         unless (member key vterm-keymap-exceptions)
                         collect key)
                (cl-loop for prefix in '("C-" "M-")
                         append (cl-loop for char from ?a to ?z
                                         for key = (format "%s%c" prefix char)
                                         unless (member key vterm-keymap-exceptions)
                                         collect key))))

(set-keymap-parent vterm-raw-map vterm-common-map)
(set-keymap-parent vterm-mode-map vterm-common-map)

(defmacro vterm-in-char-mode () '(eq (current-local-map) vterm-raw-map))
(defmacro vterm-in-line-mode () '(not (vterm-in-char-mode)))

(defun vterm-char-mode()
  (interactive)
  (when (vterm-in-line-mode)
    (setq vterm-old-mode-map (current-local-map))
    (use-local-map vterm-raw-map))
  (vterm-update-mode-line))

(defun vterm-line-mode()
  (interactive)
  (when (vterm-in-char-mode)
    (use-local-map vterm-old-mode-map))
  (vterm-update-mode-line))

(defun vterm-update-mode-line ()
  (let ((vterm-mode
         (if (vterm-in-char-mode)
             (propertize "char"
                         'help-echo "mouse-1: Switch to line mode"
                         'mouse-face 'mode-line-highlight
                         'local-map
                         '(keymap
                           (mode-line keymap (down-mouse-1 . vterm-line-mode))))
           (propertize "line"
                       'help-echo "mouse-1: Switch to char mode"
                       'mouse-face 'mode-line-highlight
                       'local-map
                       '(keymap
                         (mode-line keymap (down-mouse-1 . vterm-char-mode)))))))

    (setq mode-line-process
          (list ": " vterm-mode " %s")))
  (force-mode-line-update))

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

(defun vterm-undo ()
  "Sends C-_ to the libvterm"
  (interactive)
  (vterm-send-key "_" nil nil t))

(defun vterm-yank ()
  "Implementation of `yank' (paste) in vterm."
  (interactive)
  (vterm-send-string (current-kill 0)
                     (not current-prefix-arg)))

(defun vterm-send-string (string &optional paste-p)
  "Send the string STRING to vterm."
  (when vterm--term
    (when paste-p
      (vterm--update vterm--term "<start_paste>" nil nil nil))
    (dolist (char (string-to-list string))
      (vterm--update vterm--term (char-to-string char) nil nil nil))
    (when paste-p
      (vterm--update vterm--term "<end_paste>" nil nil nil))))

(defvar vterm--redraw-timer nil)
(make-variable-buffer-local 'vterm--redraw-timer)

(defvar vterm-timer-delay 0.01
  "Delay for refreshing the terminal buffer after receiving updates from
libvterm. Improves performance when receiving large bursts of data.
If nil, never delay")

(defun vterm--invalidate()
  (if vterm-timer-delay
      (unless vterm--redraw-timer
        (setq vterm--redraw-timer
              (run-with-timer vterm-timer-delay nil
                              #'vterm--delayed-redraw (current-buffer))))
    (vterm--delayed-redraw (current-buffer))))

(defun vterm--delayed-redraw(buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-redisplay t)
            (inhibit-read-only t))
        (when vterm--term
          (vterm--redraw vterm--term)))
      (setq vterm--redraw-timer nil))))

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
  (let ((buf (process-buffer process)))
    (run-hook-with-args 'vterm-exit-functions
                        (if (buffer-live-p buf) buf nil))))

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
  (count-lines (point-min) (point-max)))

(defun vterm--set-title (title)
  "Run the `vterm--set-title-hook' with TITLE as argument."
  (run-hook-with-args 'vterm-set-title-functions title))


(provide 'vterm)
;;; vterm.el ends here
