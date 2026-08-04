;;; vterm-osc8-test.el --- Tests for OSC 8 hyperlink support -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Tests for OSC 8 hyperlink support: linked spans get the clickable
;; `ansi-osc-hyperlink' button treatment (browse-url-data property,
;; browse-url activation).
;;
;; Run from the repository root, with the module already built (the
;; default build vendors and patches libvterm; a system libvterm
;; without VTERM_ATTR_URI compiles the feature out and these tests
;; will fail rather than skip):
;;
;;   emacs -batch -L . -l test/vterm-osc8-test.el -f ert-run-tests-batch-and-exit
;;
;; The tests skip themselves when the dynamic module is not built,
;; ansi-osc is unavailable (Emacs < 29.1), or /bin/cat is missing; the
;; end-to-end test additionally needs /bin/sh.  Most tests feed escape
;; sequences straight to the terminal parser, so no shell round-trip
;; is involved.

;;; Code:

(require 'ert)

(defvar vterm-always-compile-module)
(setq vterm-always-compile-module nil)

(defvar vterm-osc8-test--available
  ;; Probe the module directly: requiring `vterm' without it would
  ;; prompt (or error) instead of skipping.  ansi-osc is required
  ;; because linked spans are observed via its button convention.
  (and (require 'vterm-module nil t)
       (progn (require 'vterm) t)
       (require 'ansi-osc nil t)
       (file-executable-p "/bin/cat"))
  "Non-nil when the module, ansi-osc, and a quiet shell are available.")

(defmacro vterm-osc8-test--with-vterm (&rest body)
  "Run BODY in a fresh vterm buffer whose shell is a quiet /bin/cat."
  (declare (indent 0))
  `(let* ((vterm-shell "/bin/cat")
          (buf (generate-new-buffer " *vterm-osc8-test*")))
     (unwind-protect
         (with-current-buffer buf
           (vterm-mode)
           ,@body)
       (let ((kill-buffer-query-functions nil))
         (when (buffer-live-p buf)
           (kill-buffer buf))))))

(defun vterm-osc8-test--feed (str)
  "Feed STR to the terminal parser and redraw."
  (let ((inhibit-read-only t))
    (vterm--write-input vterm--term str)
    (vterm--update vterm--term)
    (vterm--redraw vterm--term)))

(defun vterm-osc8-test--links ()
  "Return a list of (START END HREF TEXT) for all linked spans.
HREF is the span's `browse-url-data' property."
  (save-excursion
    (let (res (pos (point-min)))
      (while (< pos (point-max))
        (let* ((href (get-text-property pos 'browse-url-data))
               (end (or (next-single-property-change pos 'browse-url-data)
                        (point-max))))
          (when href
            (push (list pos end href
                        (buffer-substring-no-properties pos end))
                  res))
          (setq pos end)))
      (nreverse res))))

(defun vterm-osc8-test--find (href &optional text)
  "Find the link span carrying HREF (and, if given, label TEXT)."
  (seq-find (lambda (l)
              (and (equal (nth 2 l) href)
                   (or (null text) (equal (nth 3 l) text))))
            (vterm-osc8-test--links)))

;;; Link spans

(ert-deftest vterm-osc8-labeled-bel ()
  "A BEL-terminated OSC 8 pair labels exactly the enclosed span."
  (skip-unless vterm-osc8-test--available)
  (vterm-osc8-test--with-vterm
    (vterm-osc8-test--feed
     "pre \e]8;;https://example.com/a\aClick here\e]8;;\a post\r\n")
    (let ((l (vterm-osc8-test--find "https://example.com/a")))
      (should l)
      (should (equal (nth 3 l) "Click here")))))

(ert-deftest vterm-osc8-labeled-st ()
  "ST-terminated OSC 8 sequences work like BEL-terminated ones."
  (skip-unless vterm-osc8-test--available)
  (vterm-osc8-test--with-vterm
    (vterm-osc8-test--feed
     "\e]8;;https://example.com/st\e\\ST-link\e]8;;\e\\\r\n")
    (should (vterm-osc8-test--find "https://example.com/st" "ST-link"))))

(ert-deftest vterm-osc8-id-param ()
  "The id= parameter is accepted (and ignored: URIs are interned)."
  (skip-unless vterm-osc8-test--available)
  (vterm-osc8-test--with-vterm
    (vterm-osc8-test--feed
     "\e]8;id=4021563000;https://example.com/pull/1\aexample#1\e]8;;\a\r\n")
    (should (vterm-osc8-test--find "https://example.com/pull/1"
                                   "example#1"))))

(ert-deftest vterm-osc8-adjacent-links-split ()
  "Two back-to-back links split into two spans at the exact boundary."
  (skip-unless vterm-osc8-test--available)
  (vterm-osc8-test--with-vterm
    (vterm-osc8-test--feed
     "\e]8;;https://a.example/\aAAA\e]8;;https://b.example/\aBBB\e]8;;\a\r\n")
    (let ((la (vterm-osc8-test--find "https://a.example/" "AAA"))
          (lb (vterm-osc8-test--find "https://b.example/" "BBB")))
      (should la)
      (should lb)
      (should (= (nth 1 la) (nth 0 lb))))))

(ert-deftest vterm-osc8-fragmented-uri ()
  "A URI split across separate input writes is reassembled."
  (skip-unless vterm-osc8-test--available)
  (vterm-osc8-test--with-vterm
    (vterm-osc8-test--feed "\e]8;;https://frag.example/loooo")
    (vterm-osc8-test--feed "oooong\aFRAG\e]8;;\a\r\n")
    (should (vterm-osc8-test--find "https://frag.example/loooooooong"
                                   "FRAG"))))

(ert-deftest vterm-osc8-wrapped-link ()
  "A link whose label autowraps keeps the property on every row."
  (skip-unless vterm-osc8-test--available)
  (vterm-osc8-test--with-vterm
    (let ((long-label (make-string 150 ?x)))
      (vterm-osc8-test--feed
       (concat "\e]8;;https://wrap.example/\a" long-label "\e]8;;\a\r\n"))
      (let* ((spans (seq-filter
                     (lambda (l) (equal (nth 2 l) "https://wrap.example/"))
                     (vterm-osc8-test--links)))
             (total (apply #'+ (mapcar
                                (lambda (l)
                                  (length (replace-regexp-in-string
                                           "\n" "" (nth 3 l))))
                                spans))))
        (should (= total 150))))))

(ert-deftest vterm-osc8-link-off ()
  "Text after an empty-URI terminator carries no link properties."
  (skip-unless vterm-osc8-test--available)
  (vterm-osc8-test--with-vterm
    (vterm-osc8-test--feed
     "\e]8;;https://off.example/\aON\e]8;;\aOFFTEXT\r\n")
    (should (vterm-osc8-test--find "https://off.example/" "ON"))
    (goto-char (point-min))
    (should (search-forward "OFFTEXT" nil t))
    (should-not (get-text-property (match-beginning 0) 'browse-url-data))))

(ert-deftest vterm-osc8-dedup-reuse ()
  "Reusing a URI (interning) still labels later spans correctly."
  (skip-unless vterm-osc8-test--available)
  (vterm-osc8-test--with-vterm
    (vterm-osc8-test--feed
     "\e]8;;https://example.com/a\aOnce\e]8;;\a\r\n")
    (vterm-osc8-test--feed
     "\e]8;id=999;https://example.com/a\aAgain\e]8;;\a\r\n")
    (should (vterm-osc8-test--find "https://example.com/a" "Once"))
    (should (vterm-osc8-test--find "https://example.com/a" "Again"))))

(ert-deftest vterm-osc8-overlong-uri-dropped ()
  "URIs longer than the cap are treated as no-link, not truncated."
  (skip-unless vterm-osc8-test--available)
  (vterm-osc8-test--with-vterm
    (vterm-osc8-test--feed
     (concat "\e]8;;https://long.example/" (make-string 2100 ?z)
             "\aTOOLONG\e]8;;\a\r\n"))
    (goto-char (point-min))
    (should (search-forward "TOOLONG" nil t))
    (should-not (get-text-property (match-beginning 0) 'browse-url-data))))

(ert-deftest vterm-osc8-colored-link ()
  "Face and link properties coexist on the same span."
  (skip-unless vterm-osc8-test--available)
  (vterm-osc8-test--with-vterm
    (vterm-osc8-test--feed
     "\e[31m\e]8;;https://red.example/\aREDLINK\e]8;;\a\e[0m\r\n")
    (let ((l (vterm-osc8-test--find "https://red.example/" "REDLINK")))
      (should l)
      (should (get-text-property (nth 0 l) 'font-lock-face)))))

(ert-deftest vterm-osc8-scrollback-persists ()
  "Link properties survive when the span scrolls off-screen."
  (skip-unless vterm-osc8-test--available)
  (vterm-osc8-test--with-vterm
    (vterm-osc8-test--feed
     "\e]8;;https://example.com/a\aClick here\e]8;;\a\r\n")
    (dotimes (i 60)
      (vterm-osc8-test--feed (format "filler-%d\r\n" i)))
    (should (vterm-osc8-test--find "https://example.com/a" "Click here"))))

;;; Neighbouring OSC handlers must keep working

(ert-deftest vterm-osc8-osc52-clipboard-still-works ()
  "OSC 52 clipboard writes still reach the kill ring."
  (skip-unless vterm-osc8-test--available)
  (vterm-osc8-test--with-vterm
    (let ((kill-ring nil)
          (kill-ring-yank-pointer nil)
          (vterm-enable-manipulate-selection-data-by-osc52 t))
      (vterm-osc8-test--feed
       (concat "\e]52;c;" (base64-encode-string "osc52-payload") "\a"))
      (should (equal (current-kill 0) "osc52-payload")))))

(ert-deftest vterm-osc8-osc51-pwd-still-works ()
  "OSC 51 directory tracking still records the pwd."
  (skip-unless vterm-osc8-test--available)
  (vterm-osc8-test--with-vterm
    ;; Emit some output first so point tracks the cursor row (in a
    ;; virgin buffer point rests at the bottom of the empty screen).
    (vterm-osc8-test--feed "hello\r\n\e]51;A/tmp/osc51dir\a")
    (should (equal (vterm--get-pwd-raw vterm--term
                                       (line-number-at-pos (point)))
                   "/tmp/osc51dir"))))

;;; Button behavior

(ert-deftest vterm-osc8-button-properties ()
  "A linked span is an `ansi-osc-hyperlink' button."
  (skip-unless vterm-osc8-test--available)
  (vterm-osc8-test--with-vterm
    (vterm-osc8-test--feed
     "\e]8;;https://example.com/a\aClick here\e]8;;\a\r\n")
    (let* ((l (vterm-osc8-test--find "https://example.com/a" "Click here"))
           (pos (nth 0 l)))
      (should l)
      (should (equal (get-text-property pos 'browse-url-data)
                     "https://example.com/a"))
      (should (button-at pos))
      (should (eq (button-type (button-at pos)) 'ansi-osc-hyperlink))
      (should (eq (get-char-property pos 'mouse-face) 'highlight)))))

(ert-deftest vterm-osc8-activation-calls-browse-url ()
  "Activating a link invokes `browse-url' with the exact URI."
  (skip-unless vterm-osc8-test--available)
  (vterm-osc8-test--with-vterm
    (vterm-osc8-test--feed
     "\e]8;;https://b.example/\aBBB\e]8;;\a\r\n")
    (let* ((l (vterm-osc8-test--find "https://b.example/" "BBB"))
           (opened nil)
           (browse-url-browser-function
            (lambda (url &rest _) (setq opened url))))
      (should l)
      ;; browse-url-button-open resolves point through the selected
      ;; window (mouse-set-point), so display the buffer there.
      (save-window-excursion
        (set-window-buffer (selected-window) (current-buffer))
        (set-window-point (selected-window) (nth 0 l))
        (goto-char (nth 0 l))
        (browse-url-button-open))
      (should (equal opened "https://b.example/")))))

(ert-deftest vterm-osc8-link-keymap ()
  "Mouse and C-c RET activate the link; plain RET stays unbound.
RET must keep going to the terminal process."
  (skip-unless vterm-osc8-test--available)
  (vterm-osc8-test--with-vterm
    (vterm-osc8-test--feed
     "\e]8;;https://example.com/a\aClick here\e]8;;\a\r\n")
    (let* ((l (vterm-osc8-test--find "https://example.com/a" "Click here"))
           (map (and l (get-char-property (nth 0 l) 'keymap))))
      (should (keymapp map))
      (should (eq (lookup-key map (kbd "C-c RET")) 'browse-url-button-open))
      (should (eq (lookup-key map (kbd "<mouse-2>")) 'browse-url-button-open))
      (should (eq (lookup-key map (kbd "<follow-link>")) 'mouse-face))
      (should-not (lookup-key map (kbd "RET"))))))

(ert-deftest vterm-osc8-help-echo ()
  "The hover tooltip names the target URI."
  (skip-unless vterm-osc8-test--available)
  (vterm-osc8-test--with-vterm
    (vterm-osc8-test--feed
     "\e]8;;https://example.com/a\aClick here\e]8;;\a\r\n")
    (let* ((l (vterm-osc8-test--find "https://example.com/a" "Click here"))
           (he (and l (get-char-property (nth 0 l) 'help-echo)))
           (msg (and (functionp he)
                     (funcall he nil (current-buffer) (nth 0 l)))))
      (should (stringp msg))
      (should (string-match-p (regexp-quote "https://example.com/a") msg)))))

(ert-deftest vterm-osc8-no-stray-buttons ()
  "Unlinked text carries none of the link properties."
  (skip-unless vterm-osc8-test--available)
  (vterm-osc8-test--with-vterm
    (vterm-osc8-test--feed
     "\e]8;;https://off.example/\aON\e]8;;\aOFFTEXT\r\n")
    (goto-char (point-min))
    (should (search-forward "OFFTEXT" nil t))
    (let ((pos (match-beginning 0)))
      (should-not (get-text-property pos 'browse-url-data))
      (should-not (button-at pos)))))

;;; End-to-end through a real shell on the pty

(ert-deftest vterm-osc8-pty-end-to-end ()
  "OSC 8 emitted by a real process on the pty produces a working link."
  (skip-unless vterm-osc8-test--available)
  (skip-unless (file-executable-p "/bin/sh"))
  (let* ((vterm-shell "/bin/sh")
         (buf (generate-new-buffer " *vterm-osc8-e2e*")))
    (unwind-protect
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (vterm-mode)
            (vterm-send-string
             (concat "printf 'pre \\033]8;;https://example.com/e2e\\007"
                     "CLICKME\\033]8;;\\007 post\\n'"))
            (vterm-send-return)
            (let ((deadline (+ (float-time) 10)))
              (while (and (< (float-time) deadline)
                          (not (save-excursion
                                 (goto-char (point-min))
                                 (search-forward "CLICKME" nil t))))
                (accept-process-output vterm--process 0.1)
                (sit-for 0.05))))
          (goto-char (point-min))
          (should (search-forward "CLICKME" nil t))
          (let ((pos (match-beginning 0)))
            (should (equal (get-text-property pos 'browse-url-data)
                           "https://example.com/e2e"))
            (should (eq (button-type (button-at pos))
                        'ansi-osc-hyperlink))))
      (let ((kill-buffer-query-functions nil))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(provide 'vterm-osc8-test)
;;; vterm-osc8-test.el ends here
