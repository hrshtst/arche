;; theme
(add-to-list 'custom-theme-load-path
             (file-name-as-directory (concat user-emacs-directory "themes")))
(load-theme 'manoj-dark t)
(global-hl-line-mode t)
;; (set-face-foreground 'highlight nil)
;; (set-face-foreground 'hl-line nil)
;; (set-face-foreground 'fringe "red")
(custom-theme-set-faces
 'manoj-dark
 '(cursor ((t (:background "DeepPink"))))
 '(highlight ((t (:background "gray10" :underline nil))))
 '(hl-line ((t (:background "gray10" :underline nil))))
 '(fringe ((t (:background "grey30" :foreground "red")))))

;; from rubikitch's post
;; http://rubikitch.com/2015/05/14/global-hl-line-mode-timer/
(defun global-hl-line-timer-function ()
  (global-hl-line-unhighlight-all)
  (let ((global-hl-line-mode t))
    (global-hl-line-highlight)))
(setq global-hl-line-timer
      (run-with-idle-timer 0.1 t 'global-hl-line-timer-function))
;; (cancel-timer global-hl-line-timer)

;; make frames transparent
(set-frame-parameter (selected-frame) 'alpha '(95 80))
(add-to-list 'default-frame-alist '(alpha 95 80))
