;; git
;; enable auto-revert-mode
(global-auto-revert-mode 1)

;; git-gutter
(global-git-gutter-mode +1)

(add-hook 'focus-in-hook 'git-gutter:update-all-windows)
