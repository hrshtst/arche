;;;; C and C++ and assembly language setting

(defun my/c-mode-hook ()
  (c-set-style "k&r")
  (setq c-basic-offset 2)
  (setq indent-tabs-mode nil)
  (setq comment-style 'indent)
  (c-toggle-electric-state -1)
  (c-toggle-auto-newline t)
  (c-toggle-hungry-state t))

(add-hook 'c-mode-hook 'my/c-mode-hook)
(add-hook 'c++-mode-hook 'my/c-mode-hook)
