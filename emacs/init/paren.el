;; show paren
(require 'smartparens-config)
(smartparens-global-mode)
(show-smartparens-global-mode)

;; if smartparens is too slow, consider the following
;; (show-paren-mode 1)
;; (custom-set-variables
;;  '(show-paren-delay 0)
;;  '(show-paren-style 'expression)
;;  '(parens-require-spaces nil))
;; (ad-disable-advice 'delete-backward-char 'before 'sp-delete-pair-advice)
;; (ad-activate 'delete-backward-char)
