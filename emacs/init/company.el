;; company-mode
(custom-set-variables
 '(company-selection-wrap-around t)
 '(company-minimum-prefix-length 2)
 '(company-idle-delay 0.5))

(with-eval-after-load 'company
  (global-set-key (kbd "C-M-i") 'company-complete)

  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates)
  (define-key company-active-map (kbd "C-i") 'company-complete-selection)

  (define-key lisp-interaction-mode-map (kbd "C-M-i") 'company-elisp)
  (define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete)

  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous))

(add-hook 'after-init-hook 'global-company-mode)
