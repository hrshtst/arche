;; diminish
(require 'diminish)

(defmacro rename-major-mode (package-name mode new-name)
 `(eval-after-load ,package-name
   '(defadvice ,mode (after rename-modeline activate)
      (setq mode-name ,new-name))))

(defmacro rename-minor-mode (package mode new-name)
 `(eval-after-load ,package
   '(diminish ',mode ,new-name)))

(rename-major-mode "ruby-mode" ruby-mode "RUBY")
(rename-major-mode "python-mode" python-mode "PY")
(rename-major-mode "markdown-mode" markdown-mode "MD")
(add-hook 'emacs-lisp-mode-hook (lambda() (setq mode-name "ELISP")))

(rename-minor-mode "abbrev" abbrev-mode nil)
(rename-minor-mode "company" company-mode "CMP")
(rename-minor-mode "eldoc" eldoc-mode "DOC")
(rename-minor-mode "elisp-slime-nav" elisp-slime-nav-mode "NAV")
(rename-minor-mode "flycheck" flycheck-mode "Fc")
(rename-minor-mode "flymake" flymake-mode "Fm")
(rename-minor-mode "flyspell" flyspell-mode "Fs")
(rename-minor-mode "helm" helm-mode nil)
(rename-minor-mode "helm-gtags" helm-gtags-mode nil)
(rename-minor-mode "irony" irony-mode nil)
(rename-minor-mode "paredit" paredit-mode "()")
(rename-minor-mode "smartparens" smartparens-mode nil)
(rename-minor-mode "smooth-scroll" smooth-scroll-mode nil)
(rename-minor-mode "undo-tree" undo-tree-mode nil)
(rename-minor-mode "which-key" which-key-mode nil)
(rename-minor-mode "yasnippet" yas-minor-mode " Y")

(setq-default projectile-mode-line
 '(:eval (format " PRJ[%s]" (projectile-project-name))))
