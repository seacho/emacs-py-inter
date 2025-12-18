;;; pyembed-venv.el --- Virtualenv bootstrap for pyembed -*- lexical-binding: t; -*-

(require 'pyembed)

(defgroup pyembed-venv nil
  "Virtualenv support for embedded Python."
  :group 'pyembed)

(defcustom pyembed-venv-explicit nil
  "Explicit venv path for pyembed.
If nil, auto-detect."
  :type '(choice (const :tag "Auto" nil) directory)
  :group 'pyembed-venv)

(defun pyembed--detect-venv ()
  "Detect virtualenv path."
  (or
   ;; 1. explicit
   pyembed-venv-explicit

   ;; 2. pyvenv
   (when (bound-and-true-p pyvenv-virtual-env)
     pyvenv-virtual-env)

   ;; 3. .venv / venv in project
   (when-let ((root (or (locate-dominating-file default-directory ".venv")
                         (locate-dominating-file default-directory "venv"))))
     (let ((a (expand-file-name ".venv" root))
           (b (expand-file-name "venv" root)))
       (cond
        ((file-directory-p a) a)
        ((file-directory-p b) b))))

   nil))

(defun pyembed-init ()
  "Initialize embedded Python with detected venv."
  (interactive)
  (let ((venv (pyembed--detect-venv)))
    (if venv
        (progn
          (message "[pyembed] init with venv: %s" venv)
          (pyembed-init-runtime venv))
      (progn
        (message "[pyembed] init without venv")
        (pyembed-init-runtime)))))

(provide 'pyembed-venv)
;;; pyembed-venv.el ends here
