;;; search
;; ignore case
(setq-default case-fold-search t)

;; anzu
(global-anzu-mode +1)
(custom-set-variables
 '(anzu-mode-lighter "")
 '(anzu-deactivate-region t)
 '(anzu-use-migemo t)
 '(anzu-search-threshold 1000)
 '(anzu-replace-threshold 50)
 '(anzu-replace-to-string-separator " => "))
