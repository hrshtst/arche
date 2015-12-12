;; persp-mode
(with-eval-after-load "persp-mode"
  (setq wg-morph-on nil)
  (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))
(require 'persp-mode)
