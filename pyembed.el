;;; pyembed.el --- Embedded Python core -*- lexical-binding: t; -*-

(defgroup pyembed nil
  "Embedded Python via Emacs dynamic module."
  :group 'tools)

(defcustom pyembed-module-path
  (expand-file-name "pyembed.so" (file-name-directory load-file-name))
  "Path to pyembed dynamic module."
  :type 'file
  :group 'pyembed)

(defvar pyembed--initialized nil)

(defun pyembed--load-module ()
  (unless (featurep 'pyembed-core)
    (unless (file-exists-p pyembed-module-path)
      (error "[pyembed] module not found: %s" pyembed-module-path))
    (module-load pyembed-module-path)
    (require 'pyembed-core)))

(defun pyembed-init-runtime (&optional venv)
  "Initialize embedded Python runtime once.
Optional VENV is a virtualenv root directory."
  (pyembed--load-module)
  (when pyembed--initialized
    (error "[pyembed] Python runtime already initialized"))
  (let ((ok (if venv (py-init venv) (py-init))))
    (unless ok
      (error "[pyembed] py-init failed")))
  (setq pyembed--initialized t))

(defun pyembed-exec (code)
  "Execute Python code using embedded runtime."
  (unless pyembed--initialized
    (error "[pyembed] Python not initialized"))
  (py-exec code))

(defun pyembed-last-error ()
  (interactive)
  (let ((e (py-last-error)))
    (if (string-empty-p e)
        (message "[pyembed] no error")
      (with-help-window "*PyEmbed Error*"
        (princ e)))))

(provide 'pyembed)
;;; pyembed.el ends here
