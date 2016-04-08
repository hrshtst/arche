;; elscreen
(elscreen-start)
(global-set-key (kbd "C-z C-z") 'elscreen-toggle)
(global-set-key (kbd "C-z .") 'elscreen-screen-nickname)
(global-set-key (kbd "C-z C-l") 'helm-elscreen)
(global-set-key (kbd "C-<tab>") 'elscreen-next)
(global-set-key (kbd "<C-S-iso-lefttab>") 'elscreen-previous)

(custom-set-variables
 '(elscreen-tab-display-kill-screen nil)
 '(elscreen-tab-display-control nil)
)

(elscreen-toggle-display-tab)
