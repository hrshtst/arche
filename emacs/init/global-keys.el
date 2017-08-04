;; global key settings
(global-set-key (kbd "M-ESC ESC") 'read-only-mode)
(global-set-key [delete] 'delete-char)
(global-set-key (kbd "C-M-l") 'goto-line)
(global-set-key (kbd "M-'") 'avy-goto-word-1)
(global-set-key (kbd "C-x RET R") 'revert-buffer)

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

;; Ctrl-q map
(defvar my/ctrl-q-map (make-sparse-keymap)
  "My original keymap binded to C-q.")
(defalias 'my/ctrl-q-prefix my/ctrl-q-map)
(define-key global-map (kbd "C-q") 'my/ctrl-q-prefix)
(define-key my/ctrl-q-map (kbd "C-q") 'quoted-insert)

(define-key my/ctrl-q-map (kbd "C-c") 'column-highlight-mode)
(define-key my/ctrl-q-map (kbd "C-a") 'text-scale-adjust)
(define-key my/ctrl-q-map (kbd "C-f") 'flyspell-mode)
(define-key my/ctrl-q-map (kbd "C-m") 'flycheck-mode)

;; control buffers
(define-key global-map (kbd "C-q") nil)
(smartrep-define-key
    global-map "C-q" '(("n" . (scroll-other-window 1))
                       ("p" . (scroll-other-window -1))
                       ("N" . 'scroll-other-window)
                       ("P" . (scroll-other-window '-))
                       ("a" . (beginning-of-buffer-other-window 0))
                       ("e" . (end-of-buffer-other-window 0))
                       ("-" . 'goto-last-change)
                       ("+" . 'goto-last-change-reverse)))

(smartrep-define-key
    undo-tree-map "C-x" '(("u" . 'undo-tree-undo)
                          ("U" . 'undo-tree-redo)))

;; search
(global-set-key (kbd "M-%") 'anzu-query-replace-regexp)
(global-set-key (kbd "ESC M-%") 'anzu-query-replace-at-cursor)
(global-set-key (kbd "C-x %") 'anzu-replace-at-cursor-thing)
(define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
(define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp)

;; helm
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

;; helm-swoop
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

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
