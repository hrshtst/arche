;; shell
(defun ac-sh-mode-setup ()
  (setq ac-sources (append '(ac-source-yasnippet) ac-sources)))
(add-hook 'sh-mode-hook 'ac-sh-mode-setup)
