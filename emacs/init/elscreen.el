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
(global-set-key (kbd "C-z SPC") 'elscreen-toggle)
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
(autoload 'elscreen-persist-store "elscreen-persist/elscreen-persist" "\
Store the screens, window configurations and frame parameters.
\(fn)" t nil)

(defun elscreen-persist-sotre-message ()
  (message "elscreen configurations are stored!"))
;; (advice-add 'elscreen-persist-store-message :after #'elscreen-persist-store)

(defun elscreen-persist-resotre-message ()
  (message "elscreen configurations are restored!"))
;; (advice-add 'elscreen-persist-restore-message :after #'elscreen-persist-restore)

(defadvice elscreen-persist-store (after elscreen-persist-sotre-message-advice)
  (elscreen-persist-sotre-message))
(ad-activate 'elscreen-persist-store 'elscreen-persist-store-message-advice)

(defadvice elscreen-persist-restore (after elscreen-persist-resotre-message-advice)
  (elscreen-persist-resotre-message))
(ad-activate 'elscreen-persist-restore 'elscreen-persist-restore-message-advice)

(global-set-key (kbd "C-z C-s") 'elscreen-persist-store)
(global-set-key (kbd "C-z C-r") 'elscreen-persist-restore)
