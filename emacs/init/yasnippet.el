;; yasnippet

;; enable yasnippet as minor mode in some specific modes
(dolist (hook '(c-mode-hook
                c++-mode-hook
                emacs-lisp-mode-hook
                html-mode-hook
                python-mode-hook
                go-mode-hook
                sh-mode-hook
                markdown-mode-hook
                makefile-mode-hook
                text-mode))
  (add-hook hook 'yas-minor-mode))

(custom-set-variables
 '(yas-snippet-dirs (list (concat user-emacs-directory "snippets")
                          yas-installed-snippets-dir)))

(with-eval-after-load 'yasnippet
  (define-key yas-keymap (kbd "<tab>") nil)
  (yas-reload-all))
