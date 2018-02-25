;; python
(defun my/python-mode-hook ()
  ;; python common
  (setq python-python-command "python3")
  (setq indent-tabs-mode nil)
  (setq indent-level 4)
  (setq python-indent 4)
  (setq tab-width 4))

(add-hook 'python-mode-hook 'my/python-mode-hook)

(with-eval-after-load 'python
  ;; binding
  (smartrep-define-key
      python-mode-map "C-c" '(("h" . 'python-indent-shift-left)
                              ("l" . 'python-indent-shift-right))))

;; yasnippet
;; (defun ac-python-mode-setup ()
;;   (setq ac-sources (append '(ac-source-yasnippet) ac-sources)))
;; (add-hook 'python-mode-hook 'ac-python-mode-setup)
