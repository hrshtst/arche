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

(defun elscreen-swap-previous()
  "Interchange screens selected currently and previous."
  (interactive)
  (cond
   ((elscreen-one-screen-p)
    (elscreen-message "There is only one screen, cannot swap"))
   (t
    (let* ((screen-list (sort (elscreen-get-screen-list) '>))
           (previous-screen
            (or (nth 1 (memq (elscreen-get-current-screen) screen-list))
               (car screen-list)))
           (current-screen (elscreen-get-current-screen))
           (current-screen-property
            (elscreen-get-screen-property current-screen))
           (previous-screen-property
            (elscreen-get-screen-property previous-screen)))
      (elscreen-set-screen-property current-screen previous-screen-property)
      (elscreen-set-screen-property previous-screen current-screen-property)
      (elscreen-goto-internal (elscreen-get-current-screen)))))
  (elscreen-previous))

(defun elscreen-swap-next()
  "Interchange screens selected currently and next."
  (interactive)
  (cond
   ((elscreen-one-screen-p)
    (elscreen-message "There is only one screen, cannot swap"))
   (t
    (let* ((screen-list (sort (elscreen-get-screen-list) '<))
           (next-screen
            (or (nth 1 (memq (elscreen-get-current-screen) screen-list))
               (car screen-list)))
           (current-screen (elscreen-get-current-screen))
           (current-screen-property
            (elscreen-get-screen-property current-screen))
           (next-screen-property
            (elscreen-get-screen-property next-screen)))
      (elscreen-set-screen-property current-screen next-screen-property)
      (elscreen-set-screen-property next-screen current-screen-property)
      (elscreen-goto-internal (elscreen-get-current-screen)))))
     (elscreen-next))
