(require 'vterm-module)

(defvar vterm-vterm nil
  "Pointer to vterm struct.")
(make-variable-buffer-local 'vterm-vterm)

(defvar vterm-process nil
  "The shell process.")
(make-variable-buffer-local 'vterm-process)

(define-derived-mode vterm-mode fundamental-mode "VTerm"
  "TODO: Documentation."
  (add-hook 'post-self-insert-hook #'vterm-send-char nil t))

(define-key vterm-mode-map (kbd "RET") #'vterm-send-enter)

(defun vterm-send-char ()
  (let ((char (buffer-substring-no-properties (1- (point)) (point))))
    (vterm-send-key vterm-vterm char)))

(defun vterm-send-enter ()
  (interactive)
  (vterm-send-key vterm-vterm "ENTER"))

(defun vterm-write-stdin (str)
  (process-send-string vterm-process (base64-decode-string str)))

(defun vterm-create ()
  (interactive)
  (let ((buffer (generate-new-buffer "vterm")))
    (with-current-buffer buffer
      (vterm-mode)
      (setq vterm-vterm (vterm-new))
      (setq vterm-process
            (make-process :name "vterm-shell"
                          :buffer buffer
                          :command (list (getenv "SHELL"))
                          :coding 'no-conversion
                          :connection-type 'pty
                          :filter #'vterm-process-filter
                          :sentinel #'vterm-process-sentinel)))
    (pop-to-buffer buffer)))

(defun vterm-process-filter (process output)
  (with-current-buffer (process-buffer process)
    (vterm-input-write vterm-vterm output)))

(defun vterm-process-sentinel (process event)
  )

(provide 'vterm)
