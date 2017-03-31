;; input method
(setq mozc-color "#FFA500")
(defun mozc-change-cursor-color ()
  (if mozc-mode
      (set-buffer-local-cursor-color mozc-color)
    (set-buffer-local-cursor-color nil)))

(add-hook 'input-method-activate-hook
          (lambda () (mozc-change-cursor-color)))
(add-hook 'input-method-inactivate-hook
          (lambda () (mozc-change-cursor-color)))

;; This value should be set before loading `mozc.el'
(custom-set-variables
 '(mozc-candidate-style 'echo-area) ;; overlay is too slow
 '(mozc-leim-title "[も]"))

(when (and (require 'mozc nil t) (executable-find "mozc_emacs_helper"))
  (setq default-input-method "japanese-mozc")
  (global-set-key [?\S-\ ] 'toggle-input-method))

;; use popup style
(require 'mozc-popup)
(setq mozc-candidate-style 'popup)
