(require 'vterm-module)

(defvar vterm-process nil
  "The shell process.")

(define-derived-mode vterm-mode fundamental-mode "VTerm"
  "TODO: Documentation.")

(defun vterm-new ()
  (interactive)
  (let ((buffer (generate-new-buffer "vterm")))
    (with-current-buffer buffer
      (vterm-mode)
      (setq vterm-process
            (make-process :name "vterm-shell"
                          :buffer buffer
                          :command (list (getenv "SHELL"))
                          :coding 'utf-8
                          :connection-type 'pipe
                          :filter #'vterm-process-filter
                          :sentinel #'vterm-process-sentinel)))
    (pop-to-buffer buffer)))

(defun vterm-process-filter (process output)
  (vterm-input-write output))

(defun vterm-process-sentinel (process event)
  )

(provide 'vterm)
