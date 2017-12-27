;; setting for helm-swoop
(custom-set-variables
 '(helm-multi-swoop-edit-save t)
 '(helm-swoop-split-with-multiple-windows nil)
 '(helm-swoop-split-direction 'split-window-vertically)
 '(helm-swoop-speed-or-color nil)
 '(helm-swoop-move-to-line-cycle t)
 '(helm-swoop-use-line-number-face t)
 '(helm-swoop-use-fuzzy-match nil))

;; keybindings
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

(with-eval-after-load 'helm-swoop
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
  (define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)
  (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
  (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line))

