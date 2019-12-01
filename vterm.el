;;; vterm.el --- This package implements a terminal via libvterm -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2019  by Lukas Fürmetz
;;
;; Author: Lukas Fürmetz <fuermetz@mailbox.org>
;; Version: 0.0.1
;; URL: https://github.com/akermu/emacs-libvterm
;; Keywords: terminals
;; Package-Requires: ((emacs "25.1"))


;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; This Emacs module implements a bridge to libvterm to display a terminal in a
;; Emacs buffer.

;;; Installation

;; And add this to your `init.el`:

;; ```
;; (add-to-list 'load-path "path/to/emacs-libvterm")
;; (require 'vterm)
;; ```


;;; Code:

(unless module-file-suffix
  (error "VTerm needs module support. Please compile your Emacs with the --with-modules option!"))

(require 'term)

(defvar vterm-install-buffer-name " *Install vterm"
  "Name of the buffer used for compiling vterm-module.")

;;;###autoload
(defun vterm-module-compile ()
  "This function compiles the vterm-module."
  (interactive)
  (let ((default-directory (file-name-directory (file-truename (locate-library "vterm")))))
    (unless (file-executable-p (concat default-directory "vterm-module.so" ))
      (let* ((buffer (get-buffer-create vterm-install-buffer-name))
             status)
        (pop-to-buffer vterm-install-buffer-name)
        (setq status (call-process "sh" nil buffer t "-c"
                                   "mkdir -p build;                             \
                                    cd build;                                   \
                                    cmake -DCMAKE_BUILD_TYPE=RelWithDebInfo ..; \
                                    make") )
        (if (eq status 0)
            (message "Compilation of emacs-libvterm module succeeded")
          (error "Compilation of emacs-libvterm module failed!"))))))


(unless (require 'vterm-module nil t)
  (vterm-module-compile)
  (require 'vterm-module))

(require 'subr-x)
(require 'cl-lib)
(require 'color)
(require 'compile)

(defcustom vterm-shell shell-file-name
  "The shell that gets run in the vterm."
  :type 'string
  :group 'vterm)

(defcustom vterm-max-scrollback 1000
  "Maximum 'scrollback' value."
  :type 'number
  :group 'vterm)

(defcustom vterm-keymap-exceptions '("C-c" "C-x" "C-u" "C-g" "C-h" "M-x" "M-o" "C-v" "M-v" "C-y" "M-y")
  "Exceptions for vterm-keymap.

If you use a keybinding with a prefix-key, add that prefix-key to
this list. Note that after doing so that prefix-key cannot be sent
to the terminal anymore."
  :type '(repeat string)
  :set (lambda (sym val)
         (set sym val)
         (when (fboundp 'vterm--exclude-keys)
           (vterm--exclude-keys val)))
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
for different shell"
  :type 'hook
  :group 'vterm)

(defcustom vterm-term-environment-variable "xterm-256color"
  "TERM value for terminal."
  :type 'string
  :group 'vterm)

(defcustom vterm-eval-cmds '(("find-file" find-file)
                             ("message" message)
                             ("vterm-clear-scrollback" vterm-clear-scrollback))
  "Map of commands to functions. To be used by vterm--eval.

Avoid using EVAL on input arguments, as it could allow a third
party to commandeer your editor."
  :type '(alist :key-type string)
  :group 'vterm)

(defface vterm-color-default
  `((t :inherit default))
  "The default normal color and bright color.
The foreground color is used as ANSI color 0 and the background
color is used as ANSI color 7."
  :group 'vterm)

(defface vterm-color-black
  `((t :inherit term-color-black))
  "Face used to render black color code.
The foreground color is used as ANSI color 0 and the background
color is used as ANSI color 8."
  :group 'vterm)

(defface vterm-color-red
  `((t :inherit term-color-red))
  "Face used to render red color code.
The foreground color is used as ANSI color 1 and the background
color is used as ANSI color 9."
  :group 'vterm)

(defface vterm-color-green
  `((t :inherit term-color-green))
  "Face used to render green color code.
The foreground color is used as ANSI color 2 and the background
color is used as ANSI color 10."
  :group 'vterm)

(defface vterm-color-yellow
  `((t :inherit term-color-yellow))
  "Face used to render yellow color code.
The foreground color is used as ANSI color 3 and the background
color is used as ANSI color 11."
  :group 'vterm)

(defface vterm-color-blue
  `((t :inherit term-color-blue))
  "Face used to render blue color code.
The foreground color is used as ANSI color 4 and the background
color is used as ANSI color 12."
  :group 'vterm)

(defface vterm-color-magenta
  `((t :inherit term-color-magenta))
  "Face used to render magenta color code.
The foreground color is used as ansi color 5 and the background
color is used as ansi color 13."
  :group 'vterm)

(defface vterm-color-cyan
  `((t :inherit term-color-cyan))
  "Face used to render cyan color code.
The foreground color is used as ansi color 6 and the background
color is used as ansi color 14."
  :group 'vterm)

(defface vterm-color-white
  `((t :inherit term-color-white))
  "Face used to render white color code.
The foreground color is used as ansi color 7 and the background
color is used as ansi color 15."
  :group 'vterm)

(defvar vterm-color-palette
  [vterm-color-black
   vterm-color-red
   vterm-color-green
   vterm-color-yellow
   vterm-color-blue
   vterm-color-magenta
   vterm-color-cyan
   vterm-color-white]
  "Color palette for the foreground and background.")

(defvar-local vterm--term nil
  "Pointer to Term.")

(defvar-local vterm--process nil
  "Shell process of current term.")

(defvar-local vterm--redraw-timer nil)

(defvar vterm-timer-delay 0.01
  "Delay for refreshing the buffer after receiving updates from libvterm.
Improves performance when receiving large bursts of data.
If nil, never delay")

(define-derived-mode vterm-mode fundamental-mode "VTerm"
  "Major mode for vterm buffer."
  (buffer-disable-undo)
  (setq vterm--term (vterm--new (window-body-height)
                                (- (window-body-width) (vterm--get-margin-width))
                                vterm-max-scrollback))

  (setq buffer-read-only t)
  (setq-local scroll-conservatively 101)
  (setq-local scroll-margin 0)
  (let ((process-environment (append `(,(concat "TERM="
						                        vterm-term-environment-variable)
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
                      ,(format "stty -nl sane iutf8 erase ^? rows %d columns %d >/dev/null && exec %s"
                               (window-body-height)
                               (- (window-body-width) (vterm--get-margin-width))
                               vterm-shell))
           :coding 'no-conversion
           :connection-type 'pty
           :filter #'vterm--filter
           :sentinel (when vterm-exit-functions #'vterm--sentinel))))
  (vterm--set-pty-name vterm--term (process-tty-name vterm--process))
  (process-put vterm--process 'adjust-window-size-function
               #'vterm--window-adjust-process-window-size)
  (setq next-error-function 'vterm-next-error-function))

(defun vterm--compilation-setup ()
  "function to setup `compilation-shell-minor-mode' for vterm.
`'compilation-shell-minor-mode' would change the value of
local variable `next-error-function',so we should call this function in
`compilation-shell-minor-mode-hook'."
  (when (eq major-mode 'vterm-mode)
    (setq next-error-function 'vterm-next-error-function)))

(add-hook 'compilation-shell-minor-mode-hook #'vterm--compilation-setup)

;;;###autoload
(defun vterm-next-error-function (n &optional reset)
  "Advance to the next error message and visit the file where the error was.
This is the value of `next-error-function' in Compilation buffers."
  (interactive "p")
  (let* ((pt (point))
         (msg (compilation-next-error (or n 1) nil
				                      (or compilation-current-error
					                      compilation-messages-start
					                      (point-min))))
         (default-directory default-directory)
         (pwd (vterm--get-pwd)))
    (when pwd
      (setq default-directory pwd))
    (goto-char pt)
    (compilation-next-error-function n reset)))

;; Function keys and most of C- and M- bindings
(defun vterm--exclude-keys (exceptions)
  (mapc (lambda (key)
          (define-key vterm-mode-map (kbd key) nil))
        exceptions)
  (mapc (lambda (key)
          (define-key vterm-mode-map (kbd key) #'vterm--self-insert))
        (append (cl-loop for number from 1 to 12
                         for key = (format "<f%i>" number)
                         unless (member key exceptions)
                         collect key)
                (cl-loop for prefix in '("C-" "M-")
                         append (cl-loop for char from ?a to ?z
                                         for key = (format "%s%c" prefix char)
                                         unless (member key exceptions)
                                         collect key)))))

(vterm--exclude-keys vterm-keymap-exceptions)

;; Keybindings
(define-key vterm-mode-map [tab]                       #'vterm-send-tab)
(define-key vterm-mode-map (kbd "TAB")                 #'vterm-send-tab)
(define-key vterm-mode-map [backtab]                   #'vterm--self-insert)
(define-key vterm-mode-map [backspace]                 #'vterm-send-backspace)
(define-key vterm-mode-map (kbd "DEL")                 #'vterm-send-backspace)
(define-key vterm-mode-map [M-backspace]               #'vterm-send-meta-backspace)
(define-key vterm-mode-map (kbd "M-DEL")               #'vterm-send-meta-backspace)
(define-key vterm-mode-map [return]                    #'vterm-send-return)
(define-key vterm-mode-map (kbd "RET")                 #'vterm-send-return)
(define-key vterm-mode-map [left]                      #'vterm-send-left)
(define-key vterm-mode-map [right]                     #'vterm-send-right)
(define-key vterm-mode-map [up]                        #'vterm-send-up)
(define-key vterm-mode-map [down]                      #'vterm-send-down)
(define-key vterm-mode-map [prior]                     #'vterm-send-prior)
(define-key vterm-mode-map [next]                      #'vterm-send-next)
(define-key vterm-mode-map [home]                      #'vterm--self-insert)
(define-key vterm-mode-map [end]                       #'vterm--self-insert)
(define-key vterm-mode-map [escape]                    #'vterm--self-insert)
(define-key vterm-mode-map [remap yank]                #'vterm-yank)
(define-key vterm-mode-map [remap yank-pop]            #'vterm-yank-pop)
(define-key vterm-mode-map [remap mouse-yank-primary]  #'vterm-yank-primary)
(define-key vterm-mode-map (kbd "C-SPC")               #'vterm--self-insert)
(define-key vterm-mode-map (kbd "S-SPC")               #'vterm-send-space)
(define-key vterm-mode-map (kbd "C-_")                 #'vterm--self-insert)
(define-key vterm-mode-map (kbd "C-/")                 #'vterm-undo)
(define-key vterm-mode-map (kbd "M-.")                 #'vterm-send-meta-dot)
(define-key vterm-mode-map (kbd "M-,")                 #'vterm-send-meta-comma)
(define-key vterm-mode-map (kbd "C-c C-y")             #'vterm--self-insert)
(define-key vterm-mode-map (kbd "C-c C-c")             #'vterm-send-ctrl-c)
(define-key vterm-mode-map (kbd "C-c C-l")             #'vterm-clear-scrollback)
(define-key vterm-mode-map (kbd "C-\\")                #'vterm-send-ctrl-slash)
(define-key vterm-mode-map (kbd "C-c C-g")             #'vterm-send-ctrl-g)
(define-key vterm-mode-map (kbd "C-c C-u")             #'vterm-send-ctrl-u)
(define-key vterm-mode-map [remap self-insert-command] #'vterm--self-insert)

(define-key vterm-mode-map (kbd "C-c C-t")             #'vterm-copy-mode)

(defvar vterm-copy-mode-map (make-sparse-keymap)
  "Minor mode map for `vterm-copy-mode'.")
(define-key vterm-copy-mode-map (kbd "C-c C-t")        #'vterm-copy-mode)
(define-key vterm-copy-mode-map [return]               #'vterm-copy-mode-done)
(define-key vterm-copy-mode-map (kbd "RET")            #'vterm-copy-mode-done)

(defvar-local vterm--copy-saved-point nil)

(define-minor-mode vterm-copy-mode
  "Toggle vterm copy mode."
  :group 'vterm
  :lighter " VTermCopy"
  :keymap vterm-copy-mode-map
  (if vterm-copy-mode
      (progn                            ;enable vterm-copy-mode
        (use-local-map nil)
        (vterm-send-stop)
        (setq vterm--copy-saved-point (point)))
    (if vterm--copy-saved-point
        (goto-char vterm--copy-saved-point))
    (use-local-map vterm-mode-map)
    (vterm-send-start)))

(defun vterm-copy-mode-done ()
  "Save the active region to the kill ring and exit `vterm-copy-mode'."
  (interactive)
  (unless vterm-copy-mode
    (user-error "This command is effective only in vterm-copy-mode"))
  (unless (region-active-p)
    (user-error "No region is active"))
  (kill-ring-save (region-beginning) (region-end))
  (vterm-copy-mode -1))

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
      (when (and (not (symbolp last-input-event)) shift (not meta) (not ctrl))
        (setq key (upcase key)))
      (vterm--update vterm--term key shift meta ctrl))))

(defun vterm-send (key)
  "Sends KEY to libvterm. KEY can be anything ‘kbd’ understands."
  (let* ((event (listify-key-sequence (kbd key)))
         (modifiers (event-modifiers event))
         (base (event-basic-type event)))
    (vterm-send-key (char-to-string base)
                    (memq 'shift modifiers)
                    (memq 'meta modifiers)
                    (memq 'control modifiers))))

(defun vterm-send-start ()
  "Output from the system is started when the system receives START."
  (interactive)
  (vterm-send-key "<start>"))

(defun vterm-send-stop ()
  "Output from the system is stopped when the system receives STOP."
  (interactive)
  (vterm-send-key "<stop>"))

(defun vterm-send-return ()
  "Sends C-m to the libvterm."
  (interactive)
  (if (vterm--get-icrnl vterm--term)
      (process-send-string vterm--process "\C-j")
    (process-send-string vterm--process "\C-m")))

(defun vterm-send-tab ()
  "Sends `<tab>' to the libvterm."
  (interactive)
  (vterm-send-key "<tab>"))

(defun vterm-send-space ()
  "Sends `<space>' to the libvterm."
  (interactive)
  (vterm-send-key " "))

(defun vterm-send-backspace ()
  "Sends `<backspace>' to the libvterm."
  (interactive)
  (vterm-send-key "<backspace>"))

(defun vterm-send-meta-backspace ()
  "Sends `M-<backspace>' to the libvterm."
  (interactive)
  (vterm-send-key "<backspace>" nil t))

(defun vterm-send-up ()
  "Sends `<up>' to the libvterm."
  (interactive)
  (vterm-send-key "<up>"))

(defun vterm-send-down ()
  "Sends `<down>' to the libvterm."
  (interactive)
  (vterm-send-key "<down>"))

(defun vterm-send-left()
  "Sends `<left>' to the libvterm."
  (interactive)
  (vterm-send-key "<left>"))

(defun vterm-send-right()
  "Sends `<right>' to the libvterm."
  (interactive)
  (vterm-send-key "<right>"))

(defun vterm-send-prior()
  "Sends `<prior>' to the libvterm."
  (interactive)
  (vterm-send-key "<prior>"))

(defun vterm-send-next()
  "Sends `<next>' to the libvterm."
  (interactive)
  (vterm-send-key "<next>"))

(defun vterm-send-meta-dot()
  "Sends `M-.' to the libvterm."
  (interactive)
  (vterm-send-key "." nil t))

(defun vterm-send-meta-comma()
  "Sends `M-,' to the libvterm."
  (interactive)
  (vterm-send-key "," nil t))

(defun vterm-send-ctrl-c ()
  "Sends `C-c' to the libvterm."
  (interactive)
  (vterm-send-key "c" nil nil t))

(defun vterm-send-ctrl-slash ()
  (interactive)
  (vterm-send-key "\\" nil nil t))

(defun vterm-send-ctrl-g ()
  (interactive)
  (vterm-send-key "g" nil nil t))

(defun vterm-send-ctrl-u ()
  (interactive)
  (vterm-send-key "u" nil nil t))

(defun vterm-clear-scrollback ()
  "Sends `<clear-scrollback>' to the libvterm."
  (interactive)
  (vterm-send-key "<clear_scrollback>"))

(defun vterm-undo ()
  "Sends `C-_' to the libvterm."
  (interactive)
  (vterm-send-key "_" nil nil t))


(defun vterm-yank (&optional arg)
  "Implementation of `yank' (paste) in vterm."
  (interactive "P")
  (let ((inhibit-read-only t))
    (cl-letf (((symbol-function 'insert-for-yank)
               #'(lambda(str) (vterm-send-string str t))))
      (yank arg))))

(defun vterm-yank-primary ()
  "Implementation of `mouse-yank-primary' in vterm."
  (interactive)
  (let ((inhibit-read-only t)
        (primary (gui-get-primary-selection)))
    (cl-letf (((symbol-function 'insert-for-yank)
               #'(lambda(str) (vterm-send-string str t))))
      (insert-for-yank primary))))

(defun vterm-yank-pop(&optional arg)
  "Implementation of `yank-pop' in vterm."
  (interactive "p")
  (let ((inhibit-read-only t)
        (yank-undo-function #'(lambda(_start _end) (vterm-undo))))
    (cl-letf (((symbol-function 'insert-for-yank)
               #'(lambda(str) (vterm-send-string str t))))
      (yank-pop arg))))

(defun vterm-send-string (string &optional paste-p)
  "Send the string STRING to vterm.
Optional argument PASTE-P paste-p."
  (when vterm--term
    (when paste-p
      (vterm--update vterm--term "<start_paste>" nil nil nil))
    (dolist (char (string-to-list string))
      (vterm--update vterm--term (char-to-string char) nil nil nil))
    (when paste-p
      (vterm--update vterm--term "<end_paste>" nil nil nil))))

(defun vterm--invalidate()
  "The terminal buffer is invalidated, the buffer needs redrawing."
  (if vterm-timer-delay
      (unless vterm--redraw-timer
        (setq vterm--redraw-timer
              (run-with-timer vterm-timer-delay nil
                              #'vterm--delayed-redraw (current-buffer))))
    (vterm--delayed-redraw (current-buffer))))

(defun vterm--delayed-redraw(buffer)
  "Redraw the terminal buffer .
Argument BUFFER the terminal buffer."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-redisplay t)
            (inhibit-read-only t))
        (setq vterm--redraw-timer nil)
        (when vterm--term
          (when (and (require 'display-line-numbers nil 'noerror)
                     (get-buffer-window buffer t)
                     (ignore-errors (display-line-numbers-update-width)))
            (window--adjust-process-windows))
          (vterm--redraw vterm--term))))))

;;;###autoload
(defun vterm (&optional buffer-name)
  "Create a new vterm."
  (interactive)
  (let ((buffer (generate-new-buffer (or buffer-name "vterm"))))
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
        (inhibit-read-only t)
        (buf (process-buffer process)))
    (when (buffer-live-p buf)
      (with-current-buffer  buf
        (vterm--write-input vterm--term input)
        (vterm--update vterm--term)))))

(defun vterm--sentinel (process _event)
  "Sentinel of vterm PROCESS.
Argument EVENT process event."
  (let ((buf (process-buffer process)))
    (run-hook-with-args 'vterm-exit-functions
                        (if (buffer-live-p buf) buf nil))))

(defun vterm--window-adjust-process-window-size (process windows)
  "Adjust process window size considering the width of line number."
  (let* ((size (funcall window-adjust-process-window-size-function
                        process windows))
         (width (car size))
         (height (cdr size))
         (inhibit-read-only t))
    (setq width (- width (vterm--get-margin-width)))
    (when (and (processp process)
               (process-live-p process)
               (> width 0)
               (> height 0))
      (vterm--set-size vterm--term height width)
      (cons width height))))

(defun vterm--get-margin-width ()
  "Get margin width of vterm buffer when `display-line-numbers-mode' is enabled."
  (let ((width 0))
    (when (and (boundp 'display-line-numbers)
               display-line-numbers)
      (setq width (+ (or display-line-numbers-width 0) 2)))
    width))

(defun vterm--delete-lines (line-num count &optional delete-whole-line)
  "Delete COUNT lines from LINE-NUM.
if LINE-NUM is negative backward-line from end of buffer.
 If option DELETE-WHOLE-LINE is non-nil, then this command kills
 the whole line including its terminating newline"
  (save-excursion
    (when (vterm--goto-line line-num)
      (delete-region (point) (point-at-eol count))
      (when (and delete-whole-line
                 (looking-at "\n"))
        (delete-char 1)))))

(defun vterm--goto-line(n)
  "Go to line N and return true on success.
if N is negative backward-line from end of buffer."
  (cond
   ((> n 0)
    (goto-char (point-min))
    (eq 0 (forward-line (1- n))))
   (t
    (goto-char (point-max))
    (eq 0 (forward-line n)))))

(defun vterm--set-title (title)
  "Run the `vterm--set-title-hook' with TITLE as argument."
  (run-hook-with-args 'vterm-set-title-functions title))

(defun vterm--set-directory (path)
  "Set `default-directory' to PATH."
  (let ((dir (vterm--get-directory path)))
    (when dir (setq default-directory dir))))

(defun vterm--get-directory (path)
  "Get  normalized directory to PATH."
  (when path
    (let (directory)
      (if (string-match "^\\(.*?\\)@\\(.*?\\):\\(.*?\\)$" path)
          (progn
            (let ((user (match-string 1 path))
                  (host (match-string 2 path))
                  (dir (match-string 3 path)))
              (if (and (string-equal user user-login-name)
                       (string-equal host (system-name)))
                  (progn
                    (when (file-directory-p dir)
                      (setq directory (file-name-as-directory dir))))
                (setq directory (file-name-as-directory (concat "/-:" path))))))
        (when (file-directory-p path)
          (setq directory (file-name-as-directory path))))
      directory)))

(defun vterm--get-pwd (&optional linenum)
  "Get working directory at LINENUM."
  (let ((raw-pwd (vterm--get-pwd-raw
                  vterm--term
                  (or linenum (line-number-at-pos)))))
    (when raw-pwd
      (vterm--get-directory raw-pwd))))

(defun vterm--get-color(index)
  "Get color by index from `vterm-color-palette'.
Argument INDEX index of color."
  (cond
   ((and (>= index 0)(< index 8))
    (face-foreground
     (elt vterm-color-palette index)
     nil 'default))
   ((and (>= index 8 )(< index 16))
    (face-background
     (elt vterm-color-palette (% index 8))
     nil 'default))
   ((= index -1)               ;-1 foreground
    (face-foreground 'vterm-color-default nil 'default))
   (t                                   ;-2 background
    (face-background 'vterm-color-default nil 'default))))

(defun vterm--eval(str)
  "Parse STR for command and arguments and call command defined in vterm-eval-cmds.

All passed in arguments are strings and forwarded as string to
the called functions."
  (let* ((parts (split-string-and-unquote str))
         (command (car parts))
         (args (cdr parts))
         (f (assoc command vterm-eval-cmds)))
    (if f
        (apply (cadr f) args)
      (message "Failed to find command: %s" command))))

;;; Convenience

(defun vterm--rename-buffer-as-title (title)
  "Rename buffer name as the xterm TITLE."
  (rename-buffer (format "vterm @ %s" title) t))

(defvar vterm--toggle-prev-buffer nil
  "Used by ‘vterm-toggle’.")

(defun vterm--buffer-list ()
  "Return a list of vterm buffers."
  (cl-remove-if-not (lambda (buf)
                      (provided-mode-derived-p
                       (buffer-local-value 'major-mode buf)
                       'vterm-mode))
                    (buffer-list)))

(defun vterm--cd (dir)
  "Cd to DIR."
  (when (derived-mode-p 'vterm-mode)
    (vterm-send-key "u" nil nil t)
    (execute-kbd-macro (format "cd %s" dir))))

(defun vterm-toggle (&optional arg)
  "Toggle vterm.

ARG: C-u: open the vterm buffer with the same dir of current buffer
If there exists an Vterm buffer with current directory, use that,
otherwise create one.

C-u C-u: same as C-u, but reuse a existing vterm buffer instead of
creating one."
  (interactive "p")
  (if (derived-mode-p 'vterm-mode)
      ;; toggle off
      (if vterm--toggle-prev-buffer
          (switch-to-buffer vterm--toggle-prev-buffer)
        (user-error "No previous buffer to jump back to"))
    ;; toggle on
    (let ((buffer-list (vterm--buffer-list)))
      (cond
       ((or (eq arg 4) ; C-u
            (eq arg 16)) ; C-u C-u
        ;; open in current dir
        (let* ((dir default-directory)
               (buffer-with-same-dir
                (catch 'found
                  (dolist (buffer buffer-list nil)
                    (when (equal dir (buffer-local-value
                                      'default-directory
                                      buffer))
                      (throw 'found buffer))))))
          ;; found the buffer with the same dir
          (setq vterm--toggle-prev-buffer
                (current-buffer))
          (if buffer-with-same-dir
              (switch-to-buffer buffer-with-same-dir)
            ;; or not and maybe create a new one
            (switch-to-buffer
             (if (eq arg 16)
                 (progn (message "No valid vterm buffer found, reuse one.")
                        (car buffer-list))
               (message "No valid vterm buffer found, create a new one.")
               (vterm)))
            (vterm--cd dir))))
       ;; simply open
       (t (setq vterm--toggle-prev-buffer (current-buffer))
          (switch-to-buffer (or (car buffer-list)
                                (vterm))))))))

(provide 'vterm)
;;; vterm.el ends here
