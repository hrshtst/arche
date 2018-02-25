;; jedi
(defun my/python-jedi-mode-hook ()
  '(lambda ()
     ;; jedi
     (setq jedi:setup-function nil)        ; Fix not to call jedi:ac-setup
     (setq jedi:mode-function nil)         ; when calling jedi:setup
     (jedi:setup)
     (setq jedi:complete-on-dot t)
     (setq jedi:environment-root "env")
     (setq jedi:environment-virtualenv
           (append python-environment-virtualenv
                   '("--python" "python3")))
     (setq jedi:tooltip-method nil)
     (setq-local company-backends '(company-jedi company-dabbrev))))

(add-hook 'python-mode-hook 'my/python-jedi-mode-hook)

;; (eval-after-load "jedi"
;;   '(progn
;;      (setq jedi:server-command
;;            (list python-python-command jedi:server-script))))

(with-eval-after-load 'python
  ;; binding
  (define-key python-mode-map (kbd "C-M-i") 'company-complete)
  (define-key python-mode-map (kbd "M-t") 'jedi:goto-definition)
  (define-key python-mode-map (kbd "C-t") 'jedi:goto-definition-pop-marker))
