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
;; clean trailing whitespaces
(global-set-key (kbd "C-c w") 'whitespace-cleanup)
;; backspace and help
(keyboard-translate ?\C-h ?\C-?)              ; Backspace
(global-set-key "\M-?" 'help-for-help)        ; Help
;; invoke rectangle-mark-mode by C-<Return>
(global-set-key [C-return] 'rectangle-mark-mode)

;; goto-line
(global-set-key (kbd "M-g M-g") 'avy-goto-line)

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
;; popwin
(define-key my/ctrl-q-map (kbd "SPC") 'popwin:popup-last-buffer)
(define-key my/ctrl-q-map (kbd "s")   'popwin:stick-popup-window)
(define-key my/ctrl-q-map (kbd "f")   'popwin:find-file)
(define-key my/ctrl-q-map (kbd "q")   'popwin:select-popup-window)

;; control buffers
(smartrep-define-key
    global-map "C-q" '(("n" . (scroll-other-window 1))
                       ("p" . (scroll-other-window -1))
                       ("N" . 'scroll-other-window)
                       ("P" . (scroll-other-window '-))
                       ("a" . (beginning-of-buffer-other-window 0))
                       ("e" . (end-of-buffer-other-window 0))
                       ("-" . 'goto-last-change)
                       ("+" . 'goto-last-change-reverse)))
