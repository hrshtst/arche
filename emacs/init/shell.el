;; shell
(defun my/sh-mode-hook ()
  (setq sh-basic-offset 2)
  (setq sh-indentation 2))

(add-hook 'sh-mode-hook 'my/sh-mode-hook)

;; (defun ac-sh-mode-setup ()
;;   (setq ac-sources (append '(ac-source-yasnippet) ac-sources)))
;; (add-hook 'sh-mode-hook 'ac-sh-mode-setup)
