;;;; C and C++ and assembly language setting

(defun my/c-mode-hook ()
  (c-set-style "bsd")
  (setq c-basic-offset 2)
  (setq c-auto-align-backslashes nil)
  (setq indent-tabs-mode nil)
  (setq comment-style 'indent)
  (c-toggle-auto-newline t)
  (c-toggle-hungry-state t))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(add-hook 'c-mode-hook 'my/c-mode-hook)
(add-hook 'c++-mode-hook 'my/c-mode-hook)

;;
;; gdb
;;
(setq gdb-many-windows t)
(add-hook 'gdb-mode-hook '(lambda () (gud-tooltip-mode t)))
