;; elscreen
(elscreen-start)
(global-set-key (kbd "C-z C-z") 'elscreen-toggle)
(global-set-key (kbd "C-z .") 'elscreen-screen-nickname)
(global-set-key (kbd "C-z C-l") 'helm-elscreen)

(custom-set-variables
 '(elscreen-tab-display-kill-screen nil)
 '(elscreen-tab-display-control nil)
)

(elscreen-toggle-display-tab)

;; elscreen-separate-buffer-list-mode
(elscreen-separate-buffer-list-mode 1)
