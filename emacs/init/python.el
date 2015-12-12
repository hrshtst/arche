;; python
(defun my/python-mode-hook ()
  (jedi:setup))

(with-eval-after-load 'python
  (add-hook 'python-mode-hook 'my/python-mode-hook)

  ;; binding
  (define-key python-mode-map (kbd "M-.") 'jedi:complete)
  (define-key python-mode-map (kbd "M-t") 'jedi:goto-definition)
  (define-key python-mode-map (kbd "C-t") 'jedi:goto-definition-pop-marker)

  (smartrep-define-key
      python-mode-map "C-c" '(("h" . 'python-indent-shift-left)
                              ("l" . 'python-indent-shift-right))))

;; jedi
(custom-set-variables
 '(jedi:complete-on-dot t))
