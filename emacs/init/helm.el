;; helm

(custom-set-variables
 '(helm-input-idle-delay 0)
 '(helm-exit-idle-delay 0)
 '(helm-candidate-number-limit 500)
 '(helm-ag-insert-at-point 'symbol)
 '(helm-find-files-doc-header "")
 '(helm-command-prefix-key nil)
 '(helm-samewindow nil))

(with-eval-after-load 'helm
  (helm-mode 1)
  (helm-descbinds-mode)

  ;; key bindings
  (define-key helm-map (kbd "C-p")   'helm-previous-line)
  (define-key helm-map (kbd "C-n")   'helm-next-line)
  (define-key helm-map (kbd "C-M-n") 'helm-next-source)
  (define-key helm-map (kbd "C-M-p") 'helm-previous-source)
  ;; Bind delete-backward-char to C-h
  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
  ;; Complimentation with TAB
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action))

(with-eval-after-load 'helm-swoop
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
  (define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)
  (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
  (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line))

(custom-set-variables
 '(helm-multi-swoop-edit-save t)
 '(helm-swoop-split-with-multiple-windows nil)
 '(helm-swoop-split-direction 'split-window-vertically)
 '(helm-swoop-speed-or-color nil)
 '(helm-swoop-move-to-line-cycle t)
 '(helm-swoop-use-line-number-face t)
 '(helm-swoop-use-fuzzy-match nil))
