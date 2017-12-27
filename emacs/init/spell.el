;; aspell
(custom-set-variables
 '(ispell-program-name "aspell")
 '(ispell-grep-command "grep")
 '(flyspell-issue-welcome-flag nil)
 '(flyspell-use-meta-tab nil))
(with-eval-after-load 'ispell
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))
