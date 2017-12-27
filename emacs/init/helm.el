;; helm

(custom-set-variables
 '(helm-input-idle-delay 0)
 '(helm-exit-idle-delay 0)
 '(helm-candidate-number-limit 500)
 '(helm-find-files-doc-header "")
 '(helm-command-prefix-key nil)
 '(helm-full-frame nil)
 ;; '(helm-split-window-in-side-p t)
 '(helm-move-to-line-cycle-in-source t)
 '(helm-ff-search-library-in-sexp t)
 '(helm-ff-file-name-history-use-recentf t)
 '(helm-echo-input-in-header-line t)
 '(helm-autoresize-max-height 0)
 '(helm-autoresize-min-height 20))

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

(add-hook 'helm-minibuffer-set-up-hook
          'spacemacs//helm-hide-minibuffer-maybe)

;; keybindings
(define-key global-map (kbd "C-;")       'helm-mini)
(define-key global-map (kbd "M-x")       'helm-M-x)
(define-key global-map (kbd "C-x C-f")   'helm-find-files)
(define-key global-map (kbd "C-x C-r")   'helm-recentf)
(define-key global-map (kbd "M-y")       'helm-show-kill-ring)
(define-key global-map (kbd "C-c i")     'helm-imenu)
(define-key global-map (kbd "C-c d")     'helm-descbinds)
(define-key global-map (kbd "C-c s")     'helm-ag)
(define-key global-map (kbd "C-c a")     'helm-apropos)
(define-key global-map (kbd "C-x b")     'helm-buffers-list)
(define-key global-map (kbd "C-x C-b")   'helm-for-files)
(define-key global-map (kbd "C-x C-;")   'helm-for-files)
(define-key global-map (kbd "C-c C-SPC") 'helm-all-mark-rings)
(define-key global-map (kbd "M-z")       'helm-resume)

(with-eval-after-load 'helm
  (helm-autoresize-mode 1)
  (helm-mode 1)
  (helm-descbinds-mode)

  ;; key bindings
  (define-key helm-map (kbd "C-p")   'helm-previous-line)
  (define-key helm-map (kbd "C-n")   'helm-next-line)
  (define-key helm-map (kbd "C-M-n") 'helm-next-source)
  (define-key helm-map (kbd "C-M-p") 'helm-previous-source)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i")   'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")   'helm-select-action)
  ;; Bind delete-backward-char to C-h
  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
  ;; Complimentation with TAB
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action))
