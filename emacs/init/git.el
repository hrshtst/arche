;; git
;; enable auto-revert-mode
(global-auto-revert-mode 1)

;; git-gutter
(global-git-gutter-mode +1)

(custom-set-variables
 '(git-gutter:verbosity 4)
 '(git-gutter:modified-sign " ")
 '(git-gutter:deleted-sign " "))

(add-hook 'focus-in-hook 'git-gutter:update-all-windows)
