;;; vterm-module-make.el --- make vterm-module in elisp  -*- lexical-binding: t; -*-

(require 'files)

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
  "This function compiles the vterm-module."
  (interactive)
  (when (vterm-module--cmake-is-available)
  (let ((default-directory
          (file-name-directory (file-truename (locate-library "vterm"))))
        (make-commands
         "mkdir -p build; \
          cd build; \
          cmake \
            -DCMAKE_BUILD_TYPE=RelWithDebInfo \
            ..; \
          make"))
    (unless (file-executable-p (concat default-directory "vterm-module.so"))
      (let* ((buffer (get-buffer-create vterm-install-buffer-name)))
        (pop-to-buffer vterm-install-buffer-name)
        (if (zerop (call-process "sh" nil buffer t "-c" make-commands))
            (message "Compilation of emacs-libvterm module succeeded")
          (error "Compilation of emacs-libvterm module failed!")))))))

(or (require 'vterm-module nil t)
    (vterm-module-compile))

(provide 'vterm-module-make)
