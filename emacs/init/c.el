;;;; C and C++ and assembly language setting

(defun my/c-mode-hook ()
  (c-set-style "k&r")
  (setq c-basic-offset 2)
  (setq indent-tabs-mode nil)
  (c-toggle-electric-state -1))

(add-hook 'c-mode-hook 'my/c-mode-hook)
(add-hook 'c++-mode-hook 'my/c-mode-hook)
