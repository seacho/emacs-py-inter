;;; pyembed.el

(defgroup pyembed nil
  "Embedded Python in Emacs."
  :group 'languages)

(defvar pyembed--initialized nil)

(defvar pyembed-buffer-name "*Python-Embedded*")

(defun pyembed--ensure-initialized ()
  "Ensure Python interpreter is initialized."
  (unless pyembed--initialized
    (condition-case err
        (progn
          (pyembed--initialize)
          (setq pyembed--initialized t))
      (error
       (message "Failed to initialize embedded Python: %s" err)
       (error "PyEmbed init failed")))))

;;;###autoload
(defun pyembed-start ()
  "Start embedded Python REPL."
  (interactive)
  (pyembed--ensure-initialized)
  (switch-to-buffer (get-buffer-create pyembed-buffer-name))
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert "Embedded Python REPL (CPython via Emacs module)\n")
  (insert "Type Python code and press RET.\n\n")
  (goto-char (point-max))
  (read-only-mode 1))

(defun pyembed--repl-send ()
  "Send current line to embedded Python."
  (interactive)
  (let ((line (buffer-substring-no-properties
               (line-beginning-position)
               (line-end-position))))
    (unless (string-empty-p line)
      (pyembed--ensure-initialized)
      (let ((result (pyembed--eval line)))
        (with-current-buffer pyembed-buffer-name
          (setq buffer-read-only nil)
          (goto-char (point-max))
          (insert "\n>>> " line "\n")
          (insert result "\n")
          (goto-char (point-max))
          (read-only-mode 1))))))

;; Minor mode for REPL buffer
(define-minor-mode pyembed-repl-mode
  "Minor mode for PyEmbed REPL."
  :lighter " PyEmbed"
  (if pyembed-repl-mode
      (progn
        (setq-local comint-input-sender 'pyembed--repl-send)
        (setq-local comint-prompt-regexp "^>>> ")
        (setq-local comint-get-old-input 'comint-get-line))
    ))

;; Auto-enable in REPL buffer
(add-hook 'pyembed-start-hook
          (lambda () (pyembed-repl-mode 1)))

(provide 'pyembed)
