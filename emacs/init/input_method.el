;; input method
(defun my/input-method-active-hook ()
  (set-cursor-color "blue"))

(defun my/input-method-inactivate-hook ()
  (set-cursor-color "DeepPink"))

(add-hook 'input-method-activate-hook 'my/input-method-active-hook)
(add-hook 'input-method-inactivate-hook 'my/input-method-inactivate-hook)

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
