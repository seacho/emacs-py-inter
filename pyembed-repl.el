;;; pyembed-repl.el --- REPL for embedded Python -*- lexical-binding: t; -*-

(require 'pyembed)
(require 'pyembed-venv)

(defvar pyembed-repl-buffer "*PyEmbed-REPL*")

(define-derived-mode pyembed-repl-mode fundamental-mode "PyEmbed-REPL"
  "REPL for embedded Python."
  (setq-local truncate-lines t)
  (setq-local buffer-read-only nil))

(defun pyembed-repl ()
  (interactive)
  (unless pyembed--initialized
    (pyembed-init))
  (let ((buf (get-buffer-create pyembed-repl-buffer)))
    (with-current-buffer buf
      (unless (derived-mode-p 'pyembed-repl-mode)
        (pyembed-repl-mode))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert ">>> "))
    (pop-to-buffer buf)))

(defun pyembed-repl-return ()
  (interactive)
  (let* ((line (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position)))
         (input (string-trim-left line ".*> "))
         (out (py-repl-input input)))
    (goto-char (point-max))
    (insert "\n")
    (cond
     ((string-prefix-p "... " out)
      (insert "... "))
     (t
      (when (> (length out) 0)
        (insert out "\n"))
      (insert ">>> ")))))

(define-key pyembed-repl-mode-map (kbd "RET") #'pyembed-repl-return)

(provide 'pyembed-repl)
;;; pyembed-repl.el ends here
