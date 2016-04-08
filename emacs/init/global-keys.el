;; global key settings

;; toggle fullscreen/maximized
(global-set-key [f11] 'toggle-frame-fullscreen)
(global-set-key [C-f11] 'toggle-frame-maximized)
;; toggle lines to be truncated
(global-set-key (kbd "C-c t") 'toggle-truncate-lines)
;; backspace and help
(keyboard-translate ?\C-h ?\C-?)              ; Backspace
(global-set-key "\M-?" 'help-for-help)        ; Help
;; invoke rectangle-mark-mode by C-<Return>
(global-set-key [C-return] 'rectangle-mark-mode)

;; goto-line
(global-set-key "\M-g \M-g" (lambda (x) (interactive "nLine: ") (goto-line x)))

;; control buffers
(define-key global-map (kbd "C-q") nil)
(smartrep-define-key
    global-map "C-q" '(("n" . (scroll-other-window 1))
                       ("p" . (scroll-other-window -1))
                       ("N" . 'scroll-other-window)
                       ("P" . (scroll-other-window '-))
                       ("a" . (beginning-of-buffer-other-window 0))
                       ("e" . (end-of-buffer-other-window 0))))

;; helm
(define-key global-map (kbd "C-;")       'helm-mini)
(define-key global-map (kbd "M-x")       'helm-M-x)
(define-key global-map (kbd "C-x C-f")   'helm-find-files)
(define-key global-map (kbd "C-x C-r")   'helm-recentf)
(define-key global-map (kbd "M-y")       'helm-show-kill-ring)
(define-key global-map (kbd "C-c i")     'helm-imenu)
(define-key global-map (kbd "C-x b")     'helm-buffers-list)
(define-key global-map (kbd "C-x C-b")   'helm-for-files)
(define-key global-map (kbd "C-x C-;")   'helm-for-files)
(define-key global-map (kbd "C-c C-SPC") 'helm-all-mark-rings)

;; magit
(define-key global-map (kbd "C-x g") 'magit-status)

;; flycheck
(define-key global-map (kbd "M-l") 'my/flycheck-list-errors)
(smartrep-define-key
    global-map "M-g" '(("M-n" . 'flycheck-next-error)
                       ("M-p" . 'flycheck-previous-error)))

;; swap screen
(global-set-key [f2] 'swap-screen)
(global-set-key [S-f2] 'swap-screen-with-cursor)
