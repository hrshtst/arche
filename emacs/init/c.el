;;;; C and C++ and assembly language setting

;; Refer:
;;   - https://eklitzke.org/smarter-emacs-clang-format
(defun clang-format-buffer-smart ()
  "Reformat buffer if .clang-format exists in the projectile root."
  (when (f-exists? (expand-file-name ".clang-format" (projectile-project-root)))
    (clang-format-buffer)))

(defun clang-format-buffer-smart-on-save ()
  "Add auto-save hook for clang-format-buffer-smart."
  (add-hook 'before-save-hook 'clang-format-buffer-smart nil t))

(defun my/c-mode-hook ()
  (c-set-style "bsd")
  (setq c-basic-offset 2)
  (setq c-auto-align-backslashes nil)
  (setq indent-tabs-mode nil)
  (setq comment-style 'indent)
  (c-toggle-auto-newline t)
  (c-toggle-hungry-state t)
  (setq-local company-backends '((company-c-headers company-irony company-yasnippet company-dabbrev)))
  (clang-format-buffer-smart-on-save))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(add-hook 'c-mode-hook 'my/c-mode-hook)
(add-hook 'c++-mode-hook 'my/c-mode-hook)
(add-hook 'c++-mode-hook 'google-set-c-style)
(add-hook 'c++-mode-hook 'google-make-newline-indent)

;; gdb
(setq gdb-many-windows t)
(add-hook 'gdb-mode-hook '(lambda () (gud-tooltip-mode t)))
