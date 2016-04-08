;; persp-mode
(custom-set-variables
 '(persp-auto-resume-time 1.0))
(with-eval-after-load "persp-mode"
  (setq wg-morph-on nil)
  (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))
(require 'persp-mode)
