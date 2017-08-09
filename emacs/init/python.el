;; python
(defun my/python-mode-hook ()
  ;; python common
  (setq python-python-command "python3")
  (setq indent-tabs-mode nil)
  (setq indent-level 4)
  (setq python-indent 4)
  (setq tab-width 4)
  ;; jedi
  (jedi:setup)
  (setq jedi:complete-on-dot t)
  (setq jedi:environment-root "env")
  (setq jedi:environment-virtualenv
        (append python-environment-virtualenv
                '("--python" "python3"))))

(eval-after-load "jedi"
  '(progn
     (setq jedi:server-command
           (list python-python-command jedi:server-script))))

(with-eval-after-load 'python
  (add-hook 'python-mode-hook 'my/python-mode-hook)

  ;; binding
  (define-key python-mode-map (kbd "M-.") 'jedi:complete)
  (define-key python-mode-map (kbd "M-t") 'jedi:goto-definition)
  (define-key python-mode-map (kbd "C-t") 'jedi:goto-definition-pop-marker)

  (smartrep-define-key
      python-mode-map "C-c" '(("h" . 'python-indent-shift-left)
                              ("l" . 'python-indent-shift-right))))

;; yasnippet
;; (defun ac-python-mode-setup ()
;;   (setq ac-sources (append '(ac-source-yasnippet) ac-sources)))
;; (add-hook 'python-mode-hook 'ac-python-mode-setup)
