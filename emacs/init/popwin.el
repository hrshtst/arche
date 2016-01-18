;; popwin
(require 'popwin)
(popwin-mode 1)
(global-set-key (kbd "C-z") popwin:keymap)

;; basic
(push '("*Help*" :stick t :noselect t) popwin:special-display-config)
(push '("*Compile-Log*" :stick t :noselect t) popwin:special-display-config)

;; python
(push '("*Python*"   :stick t) popwin:special-display-config)
(push '("*Python Help*" :stick t :height 20) popwin:special-display-config)
(push '("*jedi:doc*" :stick t :noselect t) popwin:special-display-config)

;; flycheck
(push '(flycheck-error-list-mode :stick t) popwin:special-display-config)

;; git
(push '("*git-gutter:diff*" :stick t :noselect t :height 20) popwin:special-display-config)

;; yatex
(require 'popwin-yatex)
(push '("*YaTeX-typesetting*") popwin:special-display-config)
(push '("*latex-math-preview-expression*") popwin:special-display-config)

(setq popwin:popup-window-height 0.5)
