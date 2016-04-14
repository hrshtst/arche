;; elscreen
(defun elscreen-create-and-nickname (nickname)
  "Create a new screen, switch to it and set nickname."
  (interactive "sSet window title to: ")
  (elscreen-create)
  (cond
   ((zerop (length nickname))
    (elscreen-delete-screen-nickname (elscreen-get-current-screen)))
   (t
    (elscreen-set-screen-nickname (elscreen-get-current-screen) nickname)))
  (elscreen-notify-screen-modification 'force))

(elscreen-start)
(global-set-key (kbd "C-z C-z") 'elscreen-toggle)
(global-set-key (kbd "C-z .") 'elscreen-screen-nickname)
(global-set-key (kbd "C-z C") 'elscreen-create-and-nickname)
(global-set-key (kbd "C-z C-l") 'helm-elscreen)
(global-set-key (kbd "C-z s") 'elscreen-swap)

(custom-set-variables
 '(elscreen-tab-display-kill-screen nil)
 '(elscreen-tab-display-control nil)
)

(elscreen-toggle-display-tab)

;; elscreen-separate-buffer-list-mode
(elscreen-separate-buffer-list-mode 1)

;; elscreen-persist
(require 'elscreen-persist)
(global-set-key (kbd "C-z C-s") 'elscreen-persist-store)
(global-set-key (kbd "C-z C-r") 'elscreen-persist-restore)
