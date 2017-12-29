;; theme
(add-to-list 'custom-theme-load-path
             (file-name-as-directory (concat user-emacs-directory "themes")))

(require 'color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-bright t)
(custom-theme-set-faces
 'sanityinc-tomorrow-bright
 '(fringe ((t (:foreground "red")))))
(set-face-attribute 'trailing-whitespace nil
                    :background "red")
(set-face-attribute 'region nil
                    :background "#b0c4de")
(global-hl-line-mode t)

;; from rubikitch's post
;; http://rubikitch.com/2015/05/14/global-hl-line-mode-timer/
(defun global-hl-line-timer-function ()
  (global-hl-line-unhighlight-all)
  (let ((global-hl-line-mode t))
    (global-hl-line-highlight)))
(setq global-hl-line-timer
      (run-with-idle-timer 0.1 t 'global-hl-line-timer-function))
(cancel-timer global-hl-line-timer)

;; make frames transparent
(set-frame-parameter (selected-frame) 'alpha '(95 80))
(add-to-list 'default-frame-alist '(alpha 95 80))
