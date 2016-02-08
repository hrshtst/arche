(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path (locate-user-emacs-file "el-get-recipes"))

;; setup
(el-get-bundle emacs-jp/init-loader)

;; input method
(when (executable-find "mozc_emacs_helper")
  (el-get-bundle elpa:mozc)
  (el-get-bundle d5884/mozc-popup
    :depends popup))

;; recentf-ext
(el-get-bundle recentf-ext)

;; pc-bufsw
(el-get-bundle ibukanov/pc-bufsw (pc-bufsw-default-keybindings))

;; helm
(el-get-bundle helm)

;; helm plugins
(el-get-bundle helm-descbinds)
(el-get-bundle helm-gtags)
(el-get-bundle helm-ag)

;; auto-complete
(el-get-bundle auto-complete)

;; yasnippet
(el-get-bundle yasnippet)

;; yatex
(el-get-bundle yatex)

;; latex-math-preview
(el-get-bundle latex-math-preview :type git :url "https://gitlab.com/latex-math-preview/latex-math-preview.git")

;; magit
(el-get-bundle magit)
(el-get-bundle git-gutter)

;; popwin
(el-get-bundle popwin)

;; smartrep
(el-get-bundle myuhe/smartrep.el)

;; flycheck
(el-get-bundle flycheck)
(el-get-bundle flycheck-pos-tip)

;; persp-mode
(el-get-bundle elpa:persp-mode)

;; python
(el-get-bundle jedi)

;; aggressive-indent-mode
(el-get-bundle aggressive-indent-mode)
