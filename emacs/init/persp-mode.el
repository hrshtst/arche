;; persp-mode
(custom-set-variables
 '(persp-auto-resume-time 1.0))

(add-to-list 'command-switch-alist
             (cons "persp-q"
                   #'(lambda (p)
                       (setq persp-auto-resume-time -1
                             persp-auto-save-opt 0))))

(with-eval-after-load "persp-mode"
  (setq wg-morph-on nil)
  (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))
(require 'persp-mode)
