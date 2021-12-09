;;; vterm.el --- Fully-featured terminal emulator -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2020 by Lukas Fürmetz & Contributors
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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; Emacs-libvterm (vterm) is fully-fledged terminal emulator based on an
;; external library (libvterm) loaded as a dynamic module.  As a result of using
;; compiled code (instead of elisp), emacs-libvterm is fully capable, fast, and
;; it can seamlessly handle large outputs.

;;; Installation

;; Emacs-libvterm requires support for loading modules.  You can check if your
;; Emacs supports modules by inspecting the variable module-file-suffix.  If it
;; nil, than, you need to recompile Emacs or obtain a copy of Emacs with this
;; option enabled.

;; Emacs-libvterm requires CMake and libvterm.  If libvterm is not available,
;; emacs-libvterm will downloaded and compiled.  In this case, libtool is
;; needed.

;; The reccomended way to install emacs-libvterm is from MELPA.

;;; Usage

;; To open a terminal, simply use the command M-x vterm.

;;; Tips and tricks

;; Adding some shell-side configuration enables a large set of additional
;; features, including, directory tracking, prompt recognition, message passing.

;;; Code:

(require 'term/xterm)

(unless module-file-suffix
  (error "VTerm needs module support.  Please compile Emacs with
  the --with-modules option!"))

;;; Compilation of the module

(defcustom vterm-module-cmake-args ""
  "Arguments given to CMake to compile vterm-module.

Currently, vterm defines the following flags (in addition to the
ones already available in CMake):

`USE_SYSTEM_LIBVTERM'.  Set it to `Off' to use the vendored version of
libvterm instead of the one installed on your system.

This string is given verbatim to CMake, so it has to have the
correct syntax.  An example of meaningful value for this variable
is `-DUSE_SYSTEM_LIBVTERM=Off'."
  :type 'string
  :group 'vterm)

(defcustom vterm-always-compile-module nil
  "If not nil, if `vterm-module' is not found, compile it without asking.

When `vterm-always-compile-module' is nil, vterm will ask for
confirmation before compiling."
  :type  'boolean
  :group 'vterm)

(defvar vterm-install-buffer-name " *Install vterm* "
  "Name of the buffer used for compiling vterm-module.")

(defun vterm-module--cmake-is-available ()
  "Return t if cmake is available.
CMake is needed to build vterm, here we check that we can find
the executable."

  (unless (executable-find "cmake")
    (error "Vterm needs CMake to be compiled.  Please, install CMake"))
  t)

;;;###autoload
(defun vterm-module-compile ()
  "Compile vterm-module."
  (interactive)
  (when (vterm-module--cmake-is-available)
    (let* ((vterm-directory
            (shell-quote-argument
             ;; NOTE: This is a workaround to fix an issue with how the Emacs
             ;; feature/native-comp branch changes the result of
             ;; `(locate-library "vterm")'. See emacs-devel thread
             ;; https://lists.gnu.org/archive/html/emacs-devel/2020-07/msg00306.html
             ;; for a discussion.
             (file-name-directory (locate-library "vterm.el" t))))
           (make-commands
            (concat
             "cd " vterm-directory "; \
             mkdir -p build; \
             cd build; \
             cmake -G 'Unix Makefiles' "
             vterm-module-cmake-args
             " ..; \
             make; \
             cd -"))
           (buffer (get-buffer-create vterm-install-buffer-name)))
      (pop-to-buffer buffer)
      (compilation-mode)
      (if (zerop (let ((inhibit-read-only t))
                   (call-process "sh" nil buffer t "-c" make-commands)))
          (message "Compilation of `emacs-libvterm' module succeeded")
        (error "Compilation of `emacs-libvterm' module failed!")))))

;; If the vterm-module is not compiled yet, compile it
(unless (require 'vterm-module nil t)
  (if (or vterm-always-compile-module
          (y-or-n-p "Vterm needs `vterm-module' to work.  Compile it now? "))
      (progn
        (vterm-module-compile)
        (require 'vterm-module))
    (error "Vterm will not work until `vterm-module' is compiled!")))

;;; Dependencies

;; Generate this list with:
;; awk -F\" '/bind_function*/ {print "(declare-function", $2, "\"vterm-module\")"}' vterm-module.c
(declare-function vterm--new "vterm-module")
(declare-function vterm--update "vterm-module")
(declare-function vterm--redraw "vterm-module")
(declare-function vterm--write-input "vterm-module")
(declare-function vterm--set-size "vterm-module")
(declare-function vterm--set-pty-name "vterm-module")
(declare-function vterm--get-pwd-raw "vterm-module")
(declare-function vterm--reset-point "vterm-module")
(declare-function vterm--get-icrnl "vterm-module")

(require 'subr-x)
(require 'find-func)
(require 'cl-lib)
(require 'term)
(require 'color)
(require 'compile)
(require 'face-remap)
(require 'tramp)
(require 'bookmark)

;;; Options

(defcustom vterm-shell shell-file-name
  "The shell that gets run in the vterm."
  :type 'string
  :group 'vterm)

(defcustom vterm-tramp-shells '(("docker" "/bin/sh"))
  "The shell that gets run in the vterm for tramp.

`vterm-tramp-shells' has to be a list of pairs of the format:
\(TRAMP-METHOD SHELL)"
  :type '(alist :key-type string :value-type string)
  :group 'vterm)

(defcustom vterm-buffer-name "*vterm*"
  "The basename used for vterm buffers.
This is the default name used when running `vterm' or
`vterm-other-window'.

With a numeric prefix argument to `vterm', the buffer name will
be the value of this variable followed by the number.  For
example, with the numeric prefix argument 2, the buffer would be
named \"*vterm*<2>\"."
  :type 'string
  :group 'vterm)

(defcustom vterm-max-scrollback 1000
  "Maximum 'scrollback' value.

The maximum allowed is 100000.  This value can modified by
changing the SB_MAX variable in vterm-module.h and recompiling
the module."
  :type 'number
  :group 'vterm)

(defcustom vterm-min-window-width 80
  "Minimum window width."
  :type 'number
  :group 'vterm)

(defcustom vterm-kill-buffer-on-exit t
  "If not nil vterm buffers are killed when the attached process is terminated.

If `vterm-kill-buffer-on-exit' is set to t, when the process
associated to a vterm buffer quits, the buffer is killed.  When
nil, the buffer will still be available as if it were in
`fundamental-mode'."
  :type  'boolean
  :group 'vterm)

(define-obsolete-variable-alias 'vterm-clear-scrollback
  'vterm-clear-scrollback-when-clearing "0.0.1")

(define-obsolete-variable-alias 'vterm-use-vterm-prompt
  'vterm-use-vterm-prompt-detection-method "0.0.1")

(defcustom vterm-clear-scrollback-when-clearing nil
  "If not nil `vterm-clear' clears both screen and scrollback.

The scrollback is everything that is not current visible on
screen in vterm buffers.

If `vterm-clear-scrollback-when-clearing' is nil, `vterm-clear'
clears only the screen, so the scrollback is accessible moving
the point up."
  :type 'number
  :group 'vterm)

(defcustom vterm-keymap-exceptions
  '("C-c" "C-x" "C-u" "C-g" "C-h" "C-l" "M-x" "M-o" "C-y" "M-y")
  "Exceptions for `vterm-keymap'.

If you use a keybinding with a prefix-key, add that prefix-key to
this list.  Note that after doing so that prefix-key cannot be sent
to the terminal anymore.

The mapping is done by the macro `vterm-define-key', and the
function `vterm--exclude-keys' removes the keybindings defined in
`vterm-keymap-exceptions'."
  :type '(repeat string)
  :set (lambda (sym val)
         (set sym val)
         (when (and (fboundp 'vterm--exclude-keys)
                    (boundp 'vterm-mode-map))
           (vterm--exclude-keys vterm-mode-map val)))
  :group 'vterm)

(defcustom vterm-exit-functions nil
  "List of functions called when a vterm process exits.

Each function is called with two arguments: the vterm buffer of
the process if any, and a string describing the event passed from
the sentinel.

This hook applies only to new vterms, created after setting this
value with `add-hook'.

Note that this hook will not work if another package like
`shell-pop' sets its own sentinel to the `vterm' process."
  :type 'hook
  :group 'vterm)

(make-obsolete-variable 'vterm-set-title-functions
                        "This variable was substituted by `vterm-buffer-name-string'."
                        "0.0.1")

(defcustom vterm-buffer-name-string nil
  "Format string for the title of vterm buffers.

If `vterm-buffer-name-string' is nil, vterm will not set the
title of its buffers.  If not nil, `vterm-buffer-name-string' has
to be a format control string (see `format') containing one
instance of %s which will be substituted with the string TITLE.
The argument TITLE is provided by the shell.  This requires shell
side configuration.

For example, if `vterm-buffer-name-string' is set to \"vterm %s\",
and the shell properly configured to set TITLE=$(pwd), than vterm
buffers will be named \"vterm\" followed by the current path.

See URL http://tldp.org/HOWTO/Xterm-Title-4.html for additional
information on the how to configure the shell."
  :type 'string
  :group 'vterm)

(defcustom vterm-term-environment-variable "xterm-256color"
  "TERM value for terminal."
  :type 'string
  :group 'vterm)

(defcustom vterm-environment nil
  "List of extra environment variables to the vterm shell processes only.

demo: '(\"env1=v1\" \"env2=v2\")"
  :type '(repeat string)
  :group 'vterm)


(defcustom vterm-enable-manipulate-selection-data-by-osc52 nil
  "Support OSC 52 MANIPULATE SELECTION DATA.

Support copy text to emacs kill ring and system clipboard by using OSC 52.
For example: send base64 encoded 'foo' to kill ring: echo -en '\e]52;c;Zm9v\a',
tmux can share its copy buffer to terminals by supporting osc52(like iterm2
 xterm) you can enable this feature for tmux by :
set -g set-clipboard on         #osc 52 copy paste share with iterm
set -ga terminal-overrides ',xterm*:XT:Ms=\E]52;%p1%s;%p2%s\007'
set -ga terminal-overrides ',screen*:XT:Ms=\E]52;%p1%s;%p2%s\007'

The clipboard querying/clearing functionality offered by OSC 52 is not
implemented here,And for security reason, this feature is disabled
by default."
  :type 'boolean
  :group 'vterm)

;; TODO: Improve doc string, it should not point to the readme but it should
;;       be self-contained.
(defcustom vterm-eval-cmds '(("find-file" find-file)
                             ("message" message)
                             ("vterm-clear-scrollback" vterm-clear-scrollback))
  "Whitelisted Emacs functions that can be executed from vterm.

You can execute Emacs functions directly from vterm buffers.  To do this,
you have to escape the name of the function and its arguments with \e]51;E.

See Message passing in README.

The function you want to execute has to be in `vterm-eval-cmds'.

`vterm-eval-cmds' has to be a list of pairs of the format:
\(NAME-OF-COMMAND-IN-SHELL EMACS-FUNCTION)

The need for an explicit map is to avoid arbitrary code execution."
  :type '(alist :key-type string)
  :group 'vterm)

(defcustom vterm-disable-underline nil
  "When not-nil, underline text properties are ignored.

This means that vterm will render underlined text as if it was not
underlined."
  :type  'boolean
  :group 'vterm)

(defcustom vterm-disable-inverse-video nil
  "When not-nil, inverse video text properties are ignored.

This means that vterm will render reversed video text as if it was not
such."
  :type  'boolean
  :group 'vterm)

(define-obsolete-variable-alias 'vterm-disable-bold-font
  'vterm-disable-bold "0.0.1")

(defcustom vterm-disable-bold-font nil
  "When not-nil, bold text properties are ignored.

This means that vterm will render bold with the default face weight."
  :type  'boolean
  :group 'vterm)

(defcustom vterm-set-bold-hightbright nil
  "When not-nil, using hightbright colors for bolded text, see #549."
  :type  'boolean
  :group 'vterm)

(defcustom vterm-ignore-blink-cursor t
  "When t,vterm will ignore request from application to turn on/off cursor blink.

If nil, cursor in any window may begin to blink or not blink because
`blink-cursor-mode`is a global minor mode in Emacs,
you can use `M-x blink-cursor-mode` to toggle."
  :type 'boolean
  :group 'vterm)

(defcustom vterm-copy-exclude-prompt t
  "When not-nil, the prompt is not included by `vterm-copy-mode-done'."
  :type 'boolean
  :group 'vterm)

(defcustom vterm-use-vterm-prompt-detection-method t
  "When not-nil, the prompt is detected through the shell.

Vterm needs to know where the shell prompt is to enable all the
available features.  There are two supported ways to do this.
First, the shell can inform vterm on the location of the prompt.
This requires shell-side configuration: the escape code 51;A is
used to set the current directory and prompt location.  This
detection method is the most-reliable.  To use it, you have
to change your shell prompt to print 51;A.

The second method is using a regular expression. This method does
not require any shell-side configuration. See
`term-prompt-regexp', for more information."
  :type 'boolean
  :group 'vterm)

(defcustom vterm-bookmark-check-dir t
  "When set to non-nil, also restore directory when restoring a vterm bookmark."
  :type 'boolean
  :group 'vterm)

;;; Faces

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

(defface vterm-color-underline
  `((t :inherit default))
  "Face used to render cells with underline attribute.
Only foreground is used."
  :group 'vterm)

(defface vterm-color-inverse-video
  `((t :inherit default))
  "Face used to render cells with inverse video attribute.
Only background is used."
  :group 'vterm)

;;; Variables

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
(defvar-local vterm--redraw-immididately nil)
(defvar-local vterm--linenum-remapping nil)
(defvar-local vterm--prompt-tracking-enabled-p nil)
(defvar-local vterm--insert-function (symbol-function #'insert))
(defvar-local vterm--delete-char-function (symbol-function #'delete-char))
(defvar-local vterm--delete-region-function (symbol-function #'delete-region))
(defvar-local vterm--undecoded-bytes nil)

(defvar vterm-timer-delay 0.1
  "Delay for refreshing the buffer after receiving updates from libvterm.

A larger delary improves performance when receiving large bursts
of data.  If nil, never delay.  The units are seconds.")

;;; Keybindings

;; We have many functions defined by vterm-define-key.  Later, we will bind some
;; of the functions.  If the following is not evaluated during compilation, the compiler
;; will complain that some functions are not defined (eg, vterm-send-C-c)
(eval-and-compile
  (defmacro vterm-define-key (key)
    "Define a command that sends KEY with modifiers C and M to vterm."
    (declare (indent defun)
             (doc-string 3))
    `(defun ,(intern (format "vterm-send-%s" key))()
       ,(format "Sends %s to the libvterm."  key)
       (interactive)
       (vterm-send-key ,(char-to-string (get-byte (1- (length key)) key))
                       ,(let ((case-fold-search nil))
                          (or (string-match-p "[A-Z]$" key)
                              (string-match-p "S-" key)))
                       ,(string-match-p "M-" key)
                       ,(string-match-p "C-" key))))

  (mapc (lambda (key)
          (eval `(vterm-define-key ,key)))
        (cl-loop for prefix in '("M-")
                 append (cl-loop for char from ?A to ?Z
                                 for key = (format "%s%c" prefix char)
                                 collect key)))
  (mapc (lambda (key)
          (eval `(vterm-define-key ,key)))
        (cl-loop for prefix in '("C-" "M-" "C-S-")
                 append (cl-loop for char from ?a to ?z
                                 for key = (format "%s%c" prefix char)
                                 collect key))))

;; Function keys and most of C- and M- bindings
(defun vterm--exclude-keys (map exceptions)
  "Remove EXCEPTIONS from the keys bound by `vterm-define-keys'.

Exceptions are defined by `vterm-keymap-exceptions'."
  (mapc (lambda (key)
          (define-key map (kbd key) nil))
        exceptions)
  (mapc (lambda (key)
          (define-key map (kbd key) #'vterm--self-insert))
        (cl-loop for number from 1 to 12
                 for key = (format "<f%i>" number)
                 unless (member key exceptions)
                 collect key))
  (mapc (lambda (key)
          (define-key map (kbd key)
            (intern (format "vterm-send-%s" key))))
        (cl-loop for prefix in '("M-")
                 append (cl-loop for char from ?A to ?Z
                                 for key = (format "%s%c" prefix char)
                                 unless (member key exceptions)
                                 collect key)))
  (mapc (lambda (key)
          (define-key map (kbd key)
            (intern (format "vterm-send-%s" key))))
        (cl-loop for prefix in '("C-" "M-" "C-S-" )
                 append (cl-loop for char from ?a to ?z
                                 for key = (format "%s%c" prefix char)
                                 unless (member key exceptions)
                                 collect key)))
  (mapc (lambda (key)
          (define-key map (kbd key) 'ignore))
        (cl-loop for prefix in '("C-M-" "C-M-S-")
                 append (cl-loop for char from ?a to ?z
                                 for key = (format "%s%c" prefix char)
                                 unless (member key exceptions)
                                 collect key))))

(defun vterm-xterm-paste (event)
  "Handle xterm paste EVENT in vterm."
  (interactive "e")
  (with-temp-buffer
    (xterm-paste event)
    (kill-new (buffer-string)))
  (vterm-yank))

(defvar vterm-mode-map
  (let ((map (make-sparse-keymap)))
    (vterm--exclude-keys map vterm-keymap-exceptions)
    (define-key map (kbd "C-]")                 #'vterm--self-insert)
    (define-key map (kbd "M-<")                 #'vterm--self-insert)
    (define-key map (kbd "M->")                 #'vterm--self-insert)
    (define-key map [tab]                       #'vterm-send-tab)
    (define-key map (kbd "TAB")                 #'vterm-send-tab)
    (define-key map [backtab]                   #'vterm--self-insert)
    (define-key map [backspace]                 #'vterm-send-backspace)
    (define-key map (kbd "DEL")                 #'vterm-send-backspace)
    (define-key map [delete]                    #'vterm-send-delete)
    (define-key map [M-backspace]               #'vterm-send-meta-backspace)
    (define-key map (kbd "M-DEL")               #'vterm-send-meta-backspace)
    (define-key map [C-backspace]               #'vterm-send-meta-backspace)
    (define-key map [return]                    #'vterm-send-return)
    (define-key map (kbd "RET")                 #'vterm-send-return)
    (define-key map [C-left]                    #'vterm-send-M-b)
    (define-key map [M-left]                    #'vterm-send-M-b)
    (define-key map [C-right]                   #'vterm-send-M-f)
    (define-key map [M-right]                   #'vterm-send-M-f)
    (define-key map [C-up]                      #'vterm-send-up)
    (define-key map [C-down]                    #'vterm-send-down)
    (define-key map [left]                      #'vterm-send-left)
    (define-key map [right]                     #'vterm-send-right)
    (define-key map [up]                        #'vterm-send-up)
    (define-key map [down]                      #'vterm-send-down)
    (define-key map [prior]                     #'vterm-send-prior)
    (define-key map [S-prior]                   #'scroll-down-command)
    (define-key map [next]                      #'vterm-send-next)
    (define-key map [S-next]                    #'scroll-up-command)
    (define-key map [home]                      #'vterm--self-insert)
    (define-key map [end]                       #'vterm--self-insert)
    (define-key map [C-home]                    #'vterm--self-insert)
    (define-key map [C-end]                     #'vterm--self-insert)
    (define-key map [escape]                    #'vterm--self-insert)
    (define-key map [remap yank]                #'vterm-yank)
    (define-key map [remap xterm-paste]         #'vterm-xterm-paste)
    (define-key map [remap yank-pop]            #'vterm-yank-pop)
    (define-key map [remap mouse-yank-primary]  #'vterm-yank-primary)
    (define-key map (kbd "C-SPC")               #'vterm--self-insert)
    (define-key map (kbd "S-SPC")               #'vterm-send-space)
    (define-key map (kbd "C-_")                 #'vterm--self-insert)
    (define-key map (kbd "C-/")                 #'vterm-undo)
    (define-key map (kbd "M-.")                 #'vterm-send-meta-dot)
    (define-key map (kbd "M-,")                 #'vterm-send-meta-comma)
    (define-key map (kbd "C-c C-y")             #'vterm--self-insert)
    (define-key map (kbd "C-c C-c")             #'vterm-send-C-c)
    (define-key map (kbd "C-c C-l")             #'vterm-clear-scrollback)
    (define-key map (kbd "C-l")                 #'vterm-clear)
    (define-key map (kbd "C-\\")                #'vterm-send-ctrl-slash)
    (define-key map (kbd "C-c C-g")             #'vterm-send-C-g)
    (define-key map (kbd "C-c C-u")             #'vterm-send-C-u)
    (define-key map [remap self-insert-command] #'vterm--self-insert)
    (define-key map (kbd "C-c C-r")             #'vterm-reset-cursor-point)
    (define-key map (kbd "C-c C-n")             #'vterm-next-prompt)
    (define-key map (kbd "C-c C-p")             #'vterm-previous-prompt)
    (define-key map (kbd "C-c C-t")             #'vterm-copy-mode)
    map))

(defvar vterm-copy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-t")        #'vterm-copy-mode)
    (define-key map [return]               #'vterm-copy-mode-done)
    (define-key map (kbd "RET")            #'vterm-copy-mode-done)
    (define-key map (kbd "C-c C-r")        #'vterm-reset-cursor-point)
    (define-key map (kbd "C-a")            #'vterm-beginning-of-line)
    (define-key map (kbd "C-e")            #'vterm-end-of-line)
    (define-key map (kbd "C-c C-n")        #'vterm-next-prompt)
    (define-key map (kbd "C-c C-p")        #'vterm-previous-prompt)
    map))


;;; Mode

(define-derived-mode vterm-mode fundamental-mode "VTerm"
  "Major mode for vterm buffer."
  (buffer-disable-undo)
  (and (boundp 'display-line-numbers)
       (let ((font-height (expt text-scale-mode-step text-scale-mode-amount)))
         (setq vterm--linenum-remapping
               (face-remap-add-relative 'line-number :height font-height))))
  (let ((process-environment (append vterm-environment
                                     `(,(concat "TERM="
                                                vterm-term-environment-variable)
                                       ,(concat "EMACS_VTERM_PATH="
                                                (file-name-directory (find-library-name "vterm")))
                                       "INSIDE_EMACS=vterm"
                                       "LINES"
                                       "COLUMNS")
                                     process-environment))
        ;; TODO: Figure out why inhibit is needed for curses to render correctly.
        (inhibit-eol-conversion nil)
        (coding-system-for-read 'binary)
        (process-adaptive-read-buffering nil)
        (width (max (- (window-body-width) (vterm--get-margin-width))
                    vterm-min-window-width)))
    (setq vterm--term (vterm--new (window-body-height)
                                  width vterm-max-scrollback
                                  vterm-disable-bold-font
                                  vterm-disable-underline
                                  vterm-disable-inverse-video
                                  vterm-ignore-blink-cursor
                                  vterm-set-bold-hightbright))
    (setq buffer-read-only t)
    (setq-local scroll-conservatively 101)
    (setq-local scroll-margin 0)
    (setq-local hscroll-margin 0)
    (setq-local hscroll-step 1)
    (setq-local truncate-lines t)


    ;; Disable all automatic fontification
    (setq-local font-lock-defaults '(nil t))

    (add-function :filter-return
                  (local 'filter-buffer-substring-function)
                  #'vterm--filter-buffer-substring)
    (setq vterm--process
          (make-process
           :name "vterm"
           :buffer (current-buffer)
           :command
           `("/bin/sh" "-c"
             ,(format
               "stty -nl sane %s erase ^? rows %d columns %d >/dev/null && exec %s"
               ;; Some stty implementations (i.e. that of *BSD) do not
               ;; support the iutf8 option.  to handle that, we run some
               ;; heuristics to work out if the system supports that
               ;; option and set the arg string accordingly. This is a
               ;; gross hack but FreeBSD doesn't seem to want to fix it.
               ;;
               ;; See: https://bugs.freebsd.org/bugzilla/show_bug.cgi?id=220009
               (if (eq system-type 'berkeley-unix) "" "iutf8")
               (window-body-height)
               width (vterm--get-shell)))
           ;; :coding 'no-conversion
           :connection-type 'pty
           :file-handler t
           :filter #'vterm--filter
           ;; The sentinel is needed if there are exit functions or if
           ;; vterm-kill-buffer-on-exit is set to t.  In this latter case,
           ;; vterm--sentinel will kill the buffer
           :sentinel (when (or vterm-exit-functions
                               vterm-kill-buffer-on-exit)
                       #'vterm--sentinel))))

  ;; Change major-mode is not allowed
  ;; Vterm interfaces with an underlying process. Changing the major
  ;; mode can break this, leading to segmentation faults.
  (add-hook 'change-major-mode-hook
            (lambda () (interactive)
              (user-error "You cannot change major mode in vterm buffers")) nil t)

  (vterm--set-pty-name vterm--term (process-tty-name vterm--process))
  (process-put vterm--process 'adjust-window-size-function
               #'vterm--window-adjust-process-window-size)
  ;; Support to compilation-shell-minor-mode
  ;; Is this necessary? See vterm--compilation-setup
  (setq next-error-function 'vterm-next-error-function)
  (setq-local bookmark-make-record-function 'vterm--bookmark-make-record))

(defun vterm--get-shell ()
  "Get the shell that gets run in the vterm."
  (if (ignore-errors (file-remote-p default-directory))
      (with-parsed-tramp-file-name default-directory nil
        (or (cadr (assoc method vterm-tramp-shells))
            vterm-shell))
    vterm-shell))

(defun vterm--bookmark-make-record ()
  "Create a vterm bookmark.

Notes down the current directory and buffer name."
  `(nil
    (handler . vterm--bookmark-handler)
    (thisdir . ,default-directory)
    (buf-name . ,(buffer-name))
    (defaults . nil)))


;;;###autoload
(defun vterm--bookmark-handler (bmk)
  "Handler to restore a vterm bookmark BMK.

If a vterm buffer of the same name does not exist, the function will create a
new vterm buffer of the name. It also checks the current directory and sets
it to the bookmarked directory if needed."
  (let* ((thisdir (bookmark-prop-get bmk 'thisdir))
         (buf-name (bookmark-prop-get bmk 'buf-name))
         (buf (get-buffer buf-name))
         (thismode (and buf (with-current-buffer buf major-mode))))
    ;; create if no such vterm buffer exists
    (when (or (not buf) (not (eq thismode 'vterm-mode)))
      (setq buf (generate-new-buffer buf-name))
      (with-current-buffer buf
        (when vterm-bookmark-check-dir
          (setq default-directory thisdir))
        (vterm-mode)))
    ;; check the current directory
    (with-current-buffer buf
      (when (and vterm-bookmark-check-dir
                 (not (string-equal default-directory thisdir)))
        (when vterm-copy-mode
          (vterm-copy-mode-done nil))
        (vterm-insert (concat "cd " thisdir))
        (vterm-send-return)))
    ;; set to this vterm buf
    (set-buffer buf)))

(defun vterm--compilation-setup ()
  "Function to enable the option `compilation-shell-minor-mode' for vterm.
`'compilation-shell-minor-mode' would change the value of local
variable `next-error-function', so we should call this function in
`compilation-shell-minor-mode-hook'."
  (when (eq major-mode 'vterm-mode)
    (setq next-error-function 'vterm-next-error-function)))

(add-hook 'compilation-shell-minor-mode-hook #'vterm--compilation-setup)

;;;###autoload
(defun vterm-next-error-function (n &optional reset)
  "Advance to the next error message and visit the file where the error was.
This is the value of `next-error-function' in Compilation
buffers.  Prefix arg N says how many error messages to move
forwards (or backwards, if negative).

Optional argument RESET clears all the errors."
  (interactive "p")
  (let* ((pt (point))
         (default-directory default-directory)
         (pwd (vterm--get-pwd)))
    (when pwd
      (setq default-directory pwd))
    (goto-char pt)
    (compilation-next-error-function n reset)))

;;; Copy Mode

(define-minor-mode vterm-copy-mode
  "Toggle `vterm-copy-mode'.

When `vterm-copy-mode' is enabled, the terminal will not display
additional output received from the underlying process and will
behave similarly to buffer in `fundamental-mode'.  This mode is
typically used to copy text from vterm buffers.

A conventient way to exit `vterm-copy-mode' is with
`vterm-copy-mode-done', which copies the selected text and exit
`vterm-copy-mode'."
  :group 'vterm
  :lighter " VTermCopy"
  :keymap vterm-copy-mode-map
  (if (equal major-mode 'vterm-mode)
      (if vterm-copy-mode
          (progn                            ;enable vterm-copy-mode
            (use-local-map nil)
            (vterm-send-stop))
        (vterm-reset-cursor-point)
        (use-local-map vterm-mode-map)
        (vterm-send-start))
    (user-error "You cannot enable vterm-copy-mode outside vterm buffers")))

(defun vterm-copy-mode-done (arg)
  "Save the active region or line to the kill ring and exit `vterm-copy-mode'.

If a region is defined then that region is killed, with no region then
current line is killed from start to end.

The option `vterm-copy-exclude-prompt' controls if the prompt
should be included in a line copy.  Using the universal prefix ARG
will invert `vterm-copy-exclude-prompt' for that call."
  (interactive "P")
  (unless vterm-copy-mode
    (user-error "This command is effective only in vterm-copy-mode"))
  (unless (use-region-p)
    (goto-char (vterm--get-beginning-of-line))
    ;; Are we excluding the prompt?
    (if (or (and vterm-copy-exclude-prompt (not arg))
            (and (not vterm-copy-exclude-prompt) arg))
        (goto-char (max (or (vterm--get-prompt-point) 0)
                        (vterm--get-beginning-of-line))))
    (set-mark (point))
    (goto-char (vterm--get-end-of-line)))
  (kill-ring-save (region-beginning) (region-end))
  (vterm-copy-mode -1))

;;; Commands

(defun vterm--self-insert ()
  "Send invoking key to libvterm."
  (interactive)
  (when vterm--term
    (let* ((modifiers (event-modifiers last-command-event))
           (shift (memq 'shift modifiers))
           (meta (memq 'meta modifiers))
           (ctrl (memq 'control modifiers))
           (raw-key (event-basic-type last-command-event))
           (ev-keys))
      (if input-method-function
          (let ((inhibit-read-only t))
            (setq ev-keys (funcall input-method-function raw-key))
            (when (listp ev-keys)
              (dolist (k ev-keys)
                (when-let ((key (key-description (vector k))))
                  (vterm-send-key key shift meta ctrl)))))
        (when-let ((key (key-description (vector raw-key))))
          (vterm-send-key key shift meta ctrl))))))

(defun vterm-send-key (key &optional shift meta ctrl accept-proc-output)
  "Send KEY to libvterm with optional modifiers SHIFT, META and CTRL."
  (deactivate-mark)
  (when vterm--term
    (let ((inhibit-redisplay t)
          (inhibit-read-only t))
      (when (and (not (symbolp last-command-event)) shift (not meta) (not ctrl))
        (setq key (upcase key)))
      (vterm--update vterm--term key shift meta ctrl)
      (setq vterm--redraw-immididately t)
      (when accept-proc-output
        (accept-process-output vterm--process vterm-timer-delay nil t)))))

(defun vterm-send (key)
  "Send KEY to libvterm.  KEY can be anything `kbd' understands."
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
  "Send `C-m' to the libvterm."
  (interactive)
  (deactivate-mark)
  (when vterm--term
    (if (vterm--get-icrnl vterm--term)
        (process-send-string vterm--process "\C-j")
      (process-send-string vterm--process "\C-m"))))

(defun vterm-send-tab ()
  "Send `<tab>' to the libvterm."
  (interactive)
  (vterm-send-key "<tab>"))

(defun vterm-send-space ()
  "Send `<space>' to the libvterm."
  (interactive)
  (vterm-send-key " "))

(defun vterm-send-backspace ()
  "Send `<backspace>' to the libvterm."
  (interactive)
  (vterm-send-key "<backspace>"))

(defun vterm-send-delete ()
  "Send `<delete>' to the libvterm."
  (interactive)
  (vterm-send-key "<delete>"))

(defun vterm-send-meta-backspace ()
  "Send `M-<backspace>' to the libvterm."
  (interactive)
  (vterm-send-key "<backspace>" nil t))

(defun vterm-send-up ()
  "Send `<up>' to the libvterm."
  (interactive)
  (vterm-send-key "<up>"))

(defun vterm-send-down ()
  "Send `<down>' to the libvterm."
  (interactive)
  (vterm-send-key "<down>"))

(defun vterm-send-left ()
  "Send `<left>' to the libvterm."
  (interactive)
  (vterm-send-key "<left>"))

(defun vterm-send-right ()
  "Send `<right>' to the libvterm."
  (interactive)
  (vterm-send-key "<right>"))

(defun vterm-send-prior ()
  "Send `<prior>' to the libvterm."
  (interactive)
  (vterm-send-key "<prior>"))

(defun vterm-send-next ()
  "Send `<next>' to the libvterm."
  (interactive)
  (vterm-send-key "<next>"))

(defun vterm-send-meta-dot ()
  "Send `M-.' to the libvterm."
  (interactive)
  (vterm-send-key "." nil t))

(defun vterm-send-meta-comma ()
  "Send `M-,' to the libvterm."
  (interactive)
  (vterm-send-key "," nil t))

(defun vterm-send-ctrl-slash ()
  "Send `C-\' to the libvterm."
  (interactive)
  (vterm-send-key "\\" nil nil t))

(defun vterm-send-escape ()
  "Send `<escape>' to the libvterm."
  (interactive)
  (vterm-send-key "<escape>"))

(defun vterm-clear-scrollback ()
  "Send `<clear-scrollback>' to the libvterm."
  (interactive)
  (vterm-send-key "<clear_scrollback>"))

(defun vterm-clear (&optional arg)
  "Send `<clear>' to the libvterm.

`vterm-clear-scrollback' determines whether
`vterm-clear' should also clear the scrollback or not.

This behavior can be altered by calling `vterm-clear' with a
prefix argument ARG or with \\[universal-argument]."
  (interactive "P")
  (if (or
       (and vterm-clear-scrollback-when-clearing (not arg))
       (and arg (not vterm-clear-scrollback-when-clearing)))
      (vterm-clear-scrollback))
  (vterm-send-C-l))

(defun vterm-undo ()
  "Send `C-_' to the libvterm."
  (interactive)
  (vterm-send-key "_" nil nil t))

(defun vterm-yank (&optional arg)
  "Yank (paste) text in vterm.

Argument ARG is passed to `yank'."
  (interactive "P")
  (deactivate-mark)
  (vterm-goto-char (point))
  (let ((inhibit-read-only t))
    (cl-letf (((symbol-function 'insert-for-yank) #'vterm-insert))
      (yank arg))))

(defun vterm-yank-primary ()
  "Yank text from the primary selection in vterm."
  (interactive)
  (vterm-goto-char (point))
  (let ((inhibit-read-only t)
        (primary (gui-get-primary-selection)))
    (cl-letf (((symbol-function 'insert-for-yank) #'vterm-insert))
      (insert-for-yank primary))))

(defun vterm-yank-pop (&optional arg)
  "Replaced text just yanked with the next entry in the kill ring.

Argument ARG is passed to `yank'"
  (interactive "p")
  (vterm-goto-char (point))
  (let ((inhibit-read-only t)
        (yank-undo-function #'(lambda (_start _end) (vterm-undo))))
    (cl-letf (((symbol-function 'insert-for-yank) #'vterm-insert))
      (yank-pop arg))))

(defun vterm-send-string (string &optional paste-p)
  "Send the string STRING to vterm.
Optional argument PASTE-P paste-p."
  (when vterm--term
    (when paste-p
      (vterm--update vterm--term "<start_paste>" ))
    (dolist (char (string-to-list string))
      (vterm--update vterm--term (char-to-string char)))
    (when paste-p
      (vterm--update vterm--term "<end_paste>")))
  (setq vterm--redraw-immididately t)
  (accept-process-output vterm--process vterm-timer-delay nil t))

(defun vterm-insert (&rest contents)
  "Insert the arguments, either strings or characters, at point.

Provide similar behavior as `insert' for vterm."
  (when vterm--term
    (vterm--update vterm--term "<start_paste>")
    (dolist (c contents)
      (if (characterp c)
          (vterm--update vterm--term (char-to-string c))
        (dolist (char (string-to-list c))
          (vterm--update vterm--term (char-to-string char)))))
    (vterm--update vterm--term "<end_paste>")
    (setq vterm--redraw-immididately t)
    (accept-process-output vterm--process vterm-timer-delay nil t)))

(defun vterm-delete-region (start end)
  "Delete the text between START and END for vterm. "
  (when vterm--term
    (if (vterm-goto-char start)
        (cl-loop repeat (- end start) do
                 (vterm-send-key "<delete>" nil nil nil t))
      (let ((inhibit-read-only nil))
        (vterm--delete-region start end)))))

(defun vterm-goto-char (pos)
  "Set point to POSITION for vterm.

The return value is `t' when point moved successfully.
It will reset to original position if it can't move there."
  (when (and vterm--term
             (vterm-cursor-in-command-buffer-p)
             (vterm-cursor-in-command-buffer-p pos))
    (let ((moved t)
          (origin-point (point))
          pt cursor-pos succ)
      (vterm-reset-cursor-point)
      (setq cursor-pos (point))
      (setq pt cursor-pos)
      (while (and (> pos pt) moved)
        (vterm-send-key "<right>" nil nil nil t)
        (setq moved (not (= pt (point))))
        (setq pt (point)))
      (setq pt (point))
      (setq moved t)
      (while (and (< pos pt) moved)
        (vterm-send-key "<left>" nil nil nil t)
        (setq moved (not (= pt (point))))
        (setq pt (point)))
      (setq succ (= pos (point)))
      (unless succ
        (vterm-goto-char cursor-pos)
        (goto-char origin-point))
      succ)))

;;; Internal

(defun vterm--delete-region(start end)
  "A wrapper for `delete-region'."
  (funcall vterm--delete-region-function start end))

(defun vterm--insert(&rest content)
  "A wrapper for `insert'."
  (apply vterm--insert-function content))

(defun vterm--delete-char(n &optional killflag)
  "A wrapper for `delete-char'."
  (funcall vterm--delete-char-function n killflag))

(defun vterm--invalidate ()
  "The terminal buffer is invalidated, the buffer needs redrawing."
  (if (and (not vterm--redraw-immididately)
           vterm-timer-delay)
      (unless vterm--redraw-timer
        (setq vterm--redraw-timer
              (run-with-timer vterm-timer-delay nil
                              #'vterm--delayed-redraw (current-buffer))))
    (vterm--delayed-redraw (current-buffer))
    (setq vterm--redraw-immididately nil)))

(defun vterm-check-proc (&optional buffer)
  "Check if there is a running process associated to the vterm buffer BUFFER.

BUFFER can be either a buffer or the name of one."
  (let* ((buffer (get-buffer (or buffer (current-buffer))))
         (proc (get-buffer-process buffer)))
    (and proc
         (memq (process-status proc) '(run stop open listen connect))
         (buffer-local-value 'vterm--term buffer))))

(defun vterm--delayed-redraw (buffer)
  "Redraw the terminal buffer.
Argument BUFFER the terminal buffer."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-redisplay t)
            (inhibit-read-only t)
            (windows (get-buffer-window-list)))
        (setq vterm--redraw-timer nil)
        (when vterm--term
          (vterm--redraw vterm--term)
          (unless (zerop (window-hscroll))
            (when (cl-member (selected-window) windows :test #'eq)
              (set-window-hscroll (selected-window) 0))))))))

(defun vterm--selection (targets data)
  "OSC 52 Manipulate Selection Data.
Search Manipulate Selection Data in
 https://invisible-island.net/xterm/ctlseqs/ctlseqs.html ."
  (when vterm-enable-manipulate-selection-data-by-osc52
    (unless (or (string-equal data "?")
                (string-empty-p data))
      (let* ((inhibit-eol-conversion t)
             (decoded-data (decode-coding-string
                            (base64-decode-string data) locale-coding-system))
             (select-enable-clipboard select-enable-clipboard)
             (select-enable-primary select-enable-primary))
        ;; https://invisible-island.net/xterm/ctlseqs/ctlseqs.html
        ;; c , p , q , s , 0 , 1 , 2 , 3 , 4 , 5 , 6 , and 7
        ;; clipboard, primary, secondary, select, or cut buffers 0 through 7
        (unless (string-empty-p targets)
          (setq select-enable-clipboard nil)
          (setq select-enable-primary nil))
        (when (cl-find ?c targets)
          (setq select-enable-clipboard t))
        (when (cl-find ?p targets)
          (setq select-enable-primary t))

        (kill-new decoded-data)
        (message "kill-ring is updated by vterm OSC 52(Manipulate Selection Data)")))))

;;; Entry Points

;;;###autoload
(defun vterm (&optional arg)
  "Create an interactive Vterm buffer.
Start a new Vterm session, or switch to an already active
session.  Return the buffer selected (or created).

With a nonnumeric prefix arg, create a new session.

With a string prefix arg, create a new session with arg as buffer name.

With a numeric prefix arg (as in `C-u 42 M-x vterm RET'), switch
to the session with that number, or create it if it doesn't
already exist.

The buffer name used for Vterm sessions is determined by the
value of `vterm-buffer-name'."
  (interactive "P")
  (vterm--internal #'pop-to-buffer-same-window arg))

;;;###autoload
(defun vterm-other-window (&optional arg)
  "Create an interactive Vterm buffer in another window.
Start a new Vterm session, or switch to an already active
session.  Return the buffer selected (or created).

With a nonnumeric prefix arg, create a new session.

With a string prefix arg, create a new session with arg as buffer name.

With a numeric prefix arg (as in `C-u 42 M-x vterm RET'), switch
to the session with that number, or create it if it doesn't
already exist.

The buffer name used for Vterm sessions is determined by the
value of `vterm-buffer-name'."
  (interactive "P")
  (vterm--internal #'pop-to-buffer arg))

(defun vterm--internal (pop-to-buf-fun &optional arg)
  (cl-assert vterm-buffer-name)
  (let ((buf (cond ((numberp arg)
                    (get-buffer-create (format "%s<%d>"
                                               vterm-buffer-name
                                               arg)))
                   ((stringp arg) (generate-new-buffer arg))
                   (arg (generate-new-buffer vterm-buffer-name))
                   (t
                    (get-buffer-create vterm-buffer-name)))))
    (cl-assert (and buf (buffer-live-p buf)))
    (funcall pop-to-buf-fun buf)
    (with-current-buffer buf
      (unless (derived-mode-p 'vterm-mode)
        (vterm-mode)))
    buf))

;;; Internal

(defun vterm--flush-output (output)
  "Send the virtual terminal's OUTPUT to the shell."
  (process-send-string vterm--process output))
;; Terminal emulation
;; This is the standard process filter for term buffers.
;; It emulates (most of the features of) a VT100/ANSI-style terminal.

;; References:
;; [ctlseqs]: http://invisible-island.net/xterm/ctlseqs/ctlseqs.html
;; [ECMA-48]: https://www.ecma-international.org/publications/standards/Ecma-048.htm
;; [vt100]: https://vt100.net/docs/vt100-ug/chapter3.html

(defconst vterm-control-seq-regexp
  (concat
   ;; A control character,
   "\\(?:[\r\n\000\007\t\b\016\017]\\|"
   ;; a C1 escape coded character (see [ECMA-48] section 5.3 "Elements
   ;; of the C1 set"),
   "\e\\(?:[DM78c]\\|"
   ;; another Emacs specific control sequence for term.el,
   "AnSiT[^\n]+\n\\|"
   ;; another Emacs specific control sequence for vterm.el
   ;; printf "\e]%s\e\\"
   "\\][^\e]+\e\\\\\\|"
   ;; or an escape sequence (section 5.4 "Control Sequences"),
   "\\[\\([\x30-\x3F]*\\)[\x20-\x2F]*[\x40-\x7E]\\)\\)")
  "Regexp matching control sequences handled by term.el.")

(defconst vterm-control-seq-prefix-regexp
  "[\032\e]")

(defun vterm--filter (process input)
  "I/O Event.  Feeds PROCESS's INPUT to the virtual terminal.

Then triggers a redraw from the module."
  (let ((inhibit-redisplay t)
        (inhibit-eol-conversion t)
        (inhibit-read-only t)
        (buf (process-buffer process))
        (i 0)
        (str-length (length input))
        decoded-substring
        funny)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        ;; borrowed from term.el
        ;; Handle non-control data.  Decode the string before
        ;; counting characters, to avoid garbling of certain
        ;; multibyte characters (https://github.com/akermu/emacs-libvterm/issues/394).
        ;; same bug of term.el https://debbugs.gnu.org/cgi/bugreport.cgi?bug=1006
        (when vterm--undecoded-bytes
          (setq input (concat vterm--undecoded-bytes input))
          (setq vterm--undecoded-bytes nil)
          (setq str-length (length input)))
        (while (< i str-length)
          (setq funny (string-match vterm-control-seq-regexp input i))
          (let ((ctl-end (if funny (match-end 0)
                           (setq funny (string-match vterm-control-seq-prefix-regexp input i))
                           (if funny
                               (setq vterm--undecoded-bytes
                                     (substring input funny))
                             (setq funny str-length))
                           ;; The control sequence ends somewhere
                           ;; past the end of this string.
                           (1+ str-length))))
            (when (> funny i)
              ;; Handle non-control data.  Decode the string before
              ;; counting characters, to avoid garbling of certain
              ;; multibyte characters (emacs bug#1006).
              (setq decoded-substring
                    (decode-coding-string
                     (substring input i funny)
                     locale-coding-system t))
              ;; Check for multibyte characters that ends
              ;; before end of string, and save it for
              ;; next time.
              (when (= funny str-length)
                (let ((partial 0)
                      (count (length decoded-substring)))
                  (while (and (< partial count)
                              (eq (char-charset (aref decoded-substring
                                                      (- count 1 partial)))
                                  'eight-bit))
                    (cl-incf partial))
                  (when (> count partial 0)
                    (setq vterm--undecoded-bytes
                          (substring decoded-substring (- partial)))
                    (setq decoded-substring
                          (substring decoded-substring 0 (- partial)))
                    (cl-decf str-length partial)
                    (cl-decf funny partial))))
              (ignore-errors (vterm--write-input vterm--term decoded-substring))
              (setq i funny))
            (when (<= ctl-end str-length)
              (ignore-errors (vterm--write-input vterm--term (substring input i ctl-end))))
            (setq i ctl-end)))
        (vterm--update vterm--term)))))

(defun vterm--sentinel (process event)
  "Sentinel of vterm PROCESS.
Argument EVENT process event."
  (let ((buf (process-buffer process)))
    (run-hook-with-args 'vterm-exit-functions
                        (if (buffer-live-p buf) buf nil)
                        event)
    (if (and vterm-kill-buffer-on-exit (buffer-live-p buf))
        (kill-buffer buf))))

(defun vterm--text-scale-mode (&optional _argv)
  "Fix `line-number' height for scaled text."
  (and text-scale-mode
       (equal major-mode 'vterm-mode)
       (boundp 'display-line-numbers)
       (let ((height (expt text-scale-mode-step
                           text-scale-mode-amount)))
         (when vterm--linenum-remapping
           (face-remap-remove-relative vterm--linenum-remapping))
         (setq vterm--linenum-remapping
               (face-remap-add-relative 'line-number :height height))))
  (window--adjust-process-windows))

(advice-add #'text-scale-mode :after #'vterm--text-scale-mode)

(defun vterm--window-adjust-process-window-size (process windows)
  "Adjust width of window WINDOWS associated to process PROCESS.

`vterm-min-window-width' determines the minimum width allowed."
  ;; We want `vterm-copy-mode' to resemble a fundamental buffer as much as
  ;; possible.  Hence, we must not call this function when the minor mode is
  ;; enabled, otherwise the buffer would be redrawn, messing around with the
  ;; position of the point.
  (unless vterm-copy-mode
    (let* ((size (funcall window-adjust-process-window-size-function
                          process windows))
           (width (car size))
           (height (cdr size))
           (inhibit-read-only t))
      (setq width (- width (vterm--get-margin-width)))
      (setq width (max width vterm-min-window-width))
      (when (and (processp process)
                 (process-live-p process)
                 (> width 0)
                 (> height 0))
        (vterm--set-size vterm--term height width)
        (cons width height)))))

(defun vterm--get-margin-width ()
  "Get margin width of vterm buffer when `display-line-numbers-mode' is enabled."
  (let ((width 0)
        (max-line-num (+ (frame-height) vterm-max-scrollback)))
    (when (bound-and-true-p display-line-numbers)
      (setq width (+ width 4
                     (string-width (number-to-string max-line-num)))))
    width))

(defun vterm--delete-lines (line-num count &optional delete-whole-line)
  "Delete COUNT lines from LINE-NUM.
If LINE-NUM is negative backward-line from end of buffer.
If option DELETE-WHOLE-LINE is non-nil, then this command kills
the whole line including its terminating newline"
  (save-excursion
    (when (vterm--goto-line line-num)
      (vterm--delete-region (point) (point-at-eol count))
      (when (and delete-whole-line
                 (looking-at "\n"))
        (vterm--delete-char 1)))))

(defun vterm--goto-line (n)
  "Go to line N and return true on success.
If N is negative backward-line from end of buffer."
  (cond
   ((> n 0)
    (goto-char (point-min))
    (eq 0 (forward-line (1- n))))
   (t
    (goto-char (point-max))
    (eq 0 (forward-line n)))))

(defun vterm--set-title (title)
  "Use TITLE to set the buffer name according to `vterm-buffer-name-string'."
  (when vterm-buffer-name-string
    (rename-buffer (format vterm-buffer-name-string title) t)))

(defun vterm--set-directory (path)
  "Set `default-directory' to PATH."
  (let ((dir (vterm--get-directory path)))
    (when dir (setq default-directory dir))))

(defun vterm--get-directory (path)
  "Get normalized directory to PATH."
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
  (when vterm--term
    (let ((raw-pwd (vterm--get-pwd-raw
                    vterm--term
                    (or linenum (line-number-at-pos)))))
      (when raw-pwd
        (vterm--get-directory raw-pwd)))))

(defun vterm--get-color (index)
  "Get color by index from `vterm-color-palette'.
Argument INDEX index of the terminal color.
Special values for INDEX are:
-11 foreground for cells with underline attribute, foreground of
the `vterm-color-underline' face is used in this case.
-12 background for cells with inverse video attribute, background
of the `vterm-color-inverse-video' face is used in this case."
  (cond
   ((and (>= index 0) (< index 8))
    (face-foreground
     (elt vterm-color-palette index)
     nil 'default))
   ((and (>= index 8) (< index 16))
    (face-background
     (elt vterm-color-palette (% index 8))
     nil 'default))
   ((= index -11)
    (face-foreground 'vterm-color-underline nil 'default))
   ((= index -12)
    (face-background 'vterm-color-inverse-video nil 'default))
   (t
    nil)))

(defun vterm--eval (str)
  "Check if string STR is `vterm-eval-cmds' and execute command.

All passed in arguments are strings and forwarded as string to
the called functions."
  (let* ((parts (split-string-and-unquote str))
         (command (car parts))
         (args (cdr parts))
         (f (assoc command vterm-eval-cmds)))
    (if f
        (apply (cadr f) args)
      (message "Failed to find command: %s" command))))

;; TODO: Improve doc string, it should not point to the readme but it should
;;       be self-contained.
(defun vterm--prompt-tracking-enabled-p ()
  "Return t if tracking the prompt is enabled.

Prompt tracking need shell side configurations.

For zsh user, this is done by PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'.

The shell send semantic information about where the prompt ends via properly
escaped sequences to Emacs.

More information see `Shell-side configuration' and `Directory tracking'
in README."
  (or vterm--prompt-tracking-enabled-p
      (save-excursion
        (setq vterm--prompt-tracking-enabled-p
              (next-single-property-change (point-min) 'vterm-prompt)))))

(defun vterm-next-prompt (n)
  "Move to end of Nth next prompt in the buffer."
  (interactive "p")
  (if (and vterm-use-vterm-prompt-detection-method
           (vterm--prompt-tracking-enabled-p))
      (let ((pt (point))
            (promp-pt (vterm--get-prompt-point)))
        (when promp-pt (goto-char promp-pt))
        (cl-loop repeat (or n 1) do
                 (setq pt (next-single-property-change (point-at-bol 2) 'vterm-prompt))
                 (when pt (goto-char pt))))
    (term-next-prompt n)))

(defun vterm-previous-prompt (n)
  "Move to end of Nth previous prompt in the buffer."
  (interactive "p")
  (if (and vterm-use-vterm-prompt-detection-method
           (vterm--prompt-tracking-enabled-p))
      (let ((pt (point))
            (prompt-pt (vterm--get-prompt-point)))
        (when prompt-pt
          (goto-char prompt-pt)
          (when (> pt (point))
            (setq n (1- (or n 1))))
          (cl-loop repeat n do
                   (setq pt (previous-single-property-change (1- (point)) 'vterm-prompt))
                   (when pt (goto-char (1- pt))))))
    (term-previous-prompt n)))

(defun vterm--get-beginning-of-line ()
  "Find the start of the line, bypassing line wraps."
  (save-excursion
    (beginning-of-line)
    (while (and (not (bobp))
                (get-text-property (1- (point)) 'vterm-line-wrap))
      (forward-char -1)
      (beginning-of-line))
    (point)))

(defun vterm--get-end-of-line ()
  "Find the start of the line, bypassing line wraps."
  (save-excursion
    (end-of-line)
    (while (get-text-property (point) 'vterm-line-wrap)
      (forward-char)
      (end-of-line))
    (point)))

;; TODO: Improve doc string, it should not point to the readme but it should
;;       be self-contained.
(defun vterm--get-prompt-point ()
  "Get the position of the end of current prompt.
More information see `vterm--prompt-tracking-enabled-p' and
`Directory tracking and Prompt tracking'in README."
  (let ((end-point (vterm--get-end-of-line))
        prompt-point)
    (save-excursion
      (if (and vterm-use-vterm-prompt-detection-method
               (vterm--prompt-tracking-enabled-p))
          (if (get-text-property end-point 'vterm-prompt)
              end-point
            (setq prompt-point (previous-single-property-change end-point 'vterm-prompt))
            (when prompt-point (setq prompt-point (1- prompt-point))))
        (goto-char end-point)
        (if (search-backward-regexp term-prompt-regexp nil t)
            (goto-char (match-end 0))
          (vterm--get-beginning-of-line))))))

(defun vterm--at-prompt-p ()
  "Return t if the cursor position is at shell prompt."
  (= (point) (or (vterm--get-prompt-point) 0)))

(defun vterm-cursor-in-command-buffer-p (&optional pt)
  "Check whether cursor in command buffer area."
  (save-excursion
    (vterm-reset-cursor-point)
    (let ((promp-pt (vterm--get-prompt-point))
          eol)
      (when promp-pt
        (goto-char promp-pt)
        (setq eol (vterm--get-end-of-line))
        (<= promp-pt (or pt (vterm--get-cursor-point)) eol)))))

(defun vterm-beginning-of-line ()
  "Move point to the beginning of the line.

Move the point to the first character after the shell prompt on this line.
If the point is already there, move to the beginning of the line.
Effectively toggle between the two positions."
  (interactive)
  (if (vterm--at-prompt-p)
      (goto-char (vterm--get-beginning-of-line))
    (goto-char (max (or (vterm--get-prompt-point) 0)
                    (vterm--get-beginning-of-line)))))

(defun vterm-end-of-line ()
  "Move point to the end of the line, bypassing line wraps."
  (interactive)
  (goto-char (vterm--get-end-of-line)))

(defun vterm-reset-cursor-point ()
  "Make sure the cursor at the right position."
  (interactive)
  (when vterm--term
    (let ((inhibit-read-only t))
      (vterm--reset-point vterm--term))))

(defun vterm--get-cursor-point ()
  "Get term cursor position."
  (when vterm--term
    (save-excursion
      (vterm-reset-cursor-point))))

(defun vterm--remove-fake-newlines ()
  "Filter out injected newlines were injected when rendering the terminal.

These newlines were tagged with 'vterm-line-wrap property so we
can find them and remove them."
  (goto-char (point-min))
  (let (fake-newline)
    (while (setq fake-newline (next-single-property-change (point)
                                                           'vterm-line-wrap))
      (goto-char fake-newline)
      (cl-assert (eq ?\n (char-after)))
      (let ((inhibit-read-only t))
        (vterm--delete-char 1)))))


(defun vterm--filter-buffer-substring (content)
  "Filter string CONTENT of fake/injected newlines."
  (with-temp-buffer
    (vterm--insert content)
    (vterm--remove-fake-newlines)
    (buffer-string)))


(provide 'vterm)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; vterm.el ends here
