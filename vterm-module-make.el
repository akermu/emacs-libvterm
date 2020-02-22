;;; vterm-module-make.el --- make vterm-module in elisp  -*- lexical-binding: t; -*-

(require 'files)

(defvar vterm-install-buffer-name " *Install vterm"
  "Name of the buffer used for compiling vterm-module.")

(defun vterm-module-compilation-requirements-satisfied ()
  "Return t if the requirements for the successful compilation are satisfied.

This means checking if cmake is available, if it is newer than
version 3.11.0, and if libtool is available."

  ;; Check if cmake is available
  (unless (executable-find "cmake")
    (error "Vterm needs to be compiled but a dependency, cmake, is missing"))

  ;; Now we can assume cmake is available, but is it new enough?
  ;; We need a minimum version, at the moment 3.11.0, we check this
  ;; by invoking cmake --version and matching the number inside
  ;; At the moment, the output of this command looks like
  ;;
  ;; cmake version 3.14.6
  ;;
  ;; CMake suite maintained and supported by Kitware (kitware.com/cmake).
  ;;
  ;; So, we extract the version from the first line.
  (let* ((cmake-min-required-version "3.11.0")
        (cmake-version-output (shell-command-to-string "cmake --version"))
        (cmake-version
         (progn
           (string-match "^cmake version \\([0-9.]+\\)" cmake-version-output)
           (match-string 1 cmake-version-output))))
    (when
        (string< cmake-version cmake-min-required-version)
    (error (format
            "Your current version of cmake (%s) is too old for vterm (required: %s)"
            cmake-version
            cmake-min-required-version))))

  ;; Check if libtool (on macOS glibtool) is available
  (unless (or (executable-find "libtool") (executable-find "glibtool"))
    (error "Vterm needs to be compiled but a dependency, libtool, is missing"))

  ;; If we are here it means that all the dependencies are satisfied.
  ;; We can return t, anything else should have thrown an error.
  t)


;;;###autoload
(defun vterm-module-compile ()
  "This function compiles the vterm-module."
  (interactive)
  (when (vterm-module-compilation-requirements-satisfied)
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
