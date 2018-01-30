;; popwin
(require 'popwin)
(popwin-mode 1)

;; basic
(push '("*Help*" :stick t :noselect t) popwin:special-display-config)
(push '(compilation-mode :noselect t) popwin:special-display-config)

;; python
(push '("*Python*"   :stick t) popwin:special-display-config)
(push '("*Python Help*" :stick t :height 20) popwin:special-display-config)
(push '("*jedi:doc*" :stick t :noselect t) popwin:special-display-config)

;; flycheck
(push '(flycheck-error-list-mode :stick t) popwin:special-display-config)

;; undo-tree
(push '(" *undo-tree*" :stick t :width 0.3 :position right) popwin:special-display-config)

;; yatex
(when (require 'yatex nil 'noerror)
  (require 'popwin-yatex)
  (push '("*YaTeX-typesetting*") popwin:special-display-config)
  (push '("*latex-math-preview-expression*") popwin:special-display-config))

(setq popwin:popup-window-height 0.5)

;; keybindings
(global-set-key (kbd "C-q l")  'popwin:popup-last-buffer)
(global-set-key (kbd "C-q s") 'popwin:stick-popup-window)
(global-set-key (kbd "C-q f") 'popwin:find-file)
(global-set-key (kbd "C-q SPC") 'popwin:select-popup-window)
