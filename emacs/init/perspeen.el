(setq perspeen-keymap-prefix (kbd "C-z"))
(define-key perspeen-mode-map (kbd "C-z SPC") 'perspeen-goto-last-ws)
(define-key perspeen-mode-map (kbd "C-z .") 'perspeen-rename-ws)
(define-key perspeen-mode-map (kbd "C-z h") 'perspeen-tab-prev)
(define-key perspeen-mode-map (kbd "C-z l") 'perspeen-tab-next)
(define-key perspeen-mode-map (kbd "C-z C-l") 'helm-perspeen)

(set-face-attribute 'perspeen-tab--header-line-active nil
                    :background "#e7c547"
                    :foreground "black")
