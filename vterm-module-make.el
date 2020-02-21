;;; vterm-module-make.el --- make vterm-module in elisp  -*- lexical-binding: t; -*-

(require 'files)

(defcustom vterm-module-cmake-args ""
  "Arguments given to CMake to compile vterm-module.

Currently, vterm defines the following flags (in addition to the
ones already available in CMake):

`USE_SYSTEM_LIBVTERM'. Set it to `yes' to use the version of
libvterm installed on your system.

This string is given verbatim to CMake, so it has to have the
correct syntax. An example of meaningful value for this variable
is `-DUSE_SYSTEM_LIBVTERM=yes'."
  :type 'string
  :group 'vterm)

(defvar vterm-install-buffer-name " *Install vterm* "
  "Name of the buffer used for compiling vterm-module.")

(defun vterm-module--cmake-is-available ()
  "Return t if cmake is available.
CMake is needed to build vterm, here we check that we can find
the executable."

  (unless (executable-find "cmake")
    (error "Vterm needs CMake to be compiled. Please, install CMake"))
  t)

;;;###autoload
(defun vterm-module-compile ()
  "Compile vterm-module."
  (interactive)
  (when (vterm-module--cmake-is-available)
    (let* ((vterm-directory
            (shell-quote-argument
             (file-name-directory (file-truename (locate-library "vterm")))))
           (make-commands
            (concat
             "cd " vterm-directory "; \
             mkdir -p build; \
             cd build; \
             cmake "
             vterm-module-cmake-args
             " ..; \
             make; \
             cd -"))
           (buffer (get-buffer-create vterm-install-buffer-name)))
      (pop-to-buffer buffer)
      (if (zerop (call-process "sh" nil buffer t "-c" make-commands))
          (message "Compilation of `emacs-libvterm' module succeeded")
        (error "Compilation of `emacs-libvterm' module failed!")))))

(provide 'vterm-module-make)
