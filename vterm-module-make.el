;;; vterm-module-make.el --- make vterm-module in elisp  -*- lexical-binding: t; -*-

(require 'files)

(defvar vterm-install-buffer-name " *Install vterm"
  "Name of the buffer used for compiling vterm-module.")

;;;###autoload
(defun vterm-module-compile ()
  "This function compiles the vterm-module."
  (interactive)
  (let* ((default-directory
           (file-name-directory (file-truename (locate-library "vterm"))))
         ;; Ask the user if they want to compile with the version of libvterm
         ;; on the system. It has to be available! (We don't perform checks)
         (compile-with-system-libvterm
          (if (y-or-n-p
                   "Do you want to use the system libvterm? (It has to be installed)")
              "yes"
              "no"))
         (make-commands
          (concat
           "mkdir -p build;"
           "cd build;"
           "cmake" "-DUSE_SYSTEM_LIBVTERM=" compile-with-system-libvterm " "
           "-DCMAKE_BUILD_TYPE=RelWithDebInfo" "..;"
           "make")))
    (unless (file-executable-p (concat default-directory "vterm-module.so"))
      (let* ((buffer (get-buffer-create vterm-install-buffer-name)))
        (pop-to-buffer vterm-install-buffer-name)
        (if (zerop (call-process "sh" nil buffer t "-c" make-commands))
            (message "Compilation of emacs-libvterm module succeeded")
          (error "Compilation of emacs-libvterm module failed!"))))))

(or (require 'vterm-module nil t)
    (vterm-module-compile))

(provide 'vterm-module-make)
