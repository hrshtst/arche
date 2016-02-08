;; use space instead of tab
(setq-default indent-tabs-mode nil)

;; show whitespaces trailing lines
(setq-default show-trailing-whitespace t)

;; clean-up whitespaces when saving a file
(add-hook 'before-save-hook 'whitespace-cleanup)
;; fix whitespace-cleanup
;; http://stackoverflow.com/questions/7349487/emacs-different-tab-indent-settings-in-different-modes
(defadvice whitespace-cleanup (around whitespace-cleanup-indent-tab
				      activate)
  "Fix whitespace-cleanup indent-tabs-mode bug"
  (let ((whitespace-indent-tabs-mode indent-tabs-mode)
	(whitespace-tab-width tab-width))
    ad-do-it))

;; enable aggressive-indent-mode
;; https://github.com/Malabarba/aggressive-indent-mode
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'c-mode-hook #'aggressive-indent-mode)
(add-hook 'c++-mode-hook #'aggressive-indent-mode)
