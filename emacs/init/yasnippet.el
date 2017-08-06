;; yasnippet
(custom-set-variables
 '(yas-snippet-dirs (list (concat user-emacs-directory "snippets")
                          yas-installed-snippets-dir))
 '(yas-alias-to-yas/prefix-p nil))

(with-eval-after-load 'yasnippet
  (yas-global-mode 1))
