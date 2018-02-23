(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(custom-set-variables
 '(el-get-verbose t))
(add-to-list 'el-get-recipe-path (locate-user-emacs-file "el-get-recipes"))

;; setup
(el-get-bundle emacs-jp/init-loader)
(el-get-bundle purcell/exec-path-from-shell)

;; input method
(when (executable-find "mozc_emacs_helper")
  (el-get-bundle ccc)
  (el-get-bundle mozc)
  (el-get-bundle d5884/mozc-popup
    :depends popup))

;; search
(when (executable-find "cmigemo")
  (el-get-bundle migemo))
(el-get-bundle anzu)

;; cursor jump
(el-get-bundle goto-chg)
(el-get-bundle abo-abo/avy)

;; undo
(el-get-bundle undo-tree)

;; dired+
(el-get-bundle dired+)
(el-get-bundle purcell/diredfl)

;; expand-region
(el-get-bundle expand-region)

;; diminish
(el-get-bundle diminish)

;; smartparens
(el-get-bundle smartparens)

;; which-key
(el-get-bundle which-key (which-key-mode))

;; open-junk-file
(el-get-bundle open-junk-file)

;; theme
(el-get-bundle powerline)
(el-get-bundle color-theme-sanityinc-tomorrow)

;; buffer
(el-get-bundle ibukanov/pc-bufsw (pc-bufsw-default-keybindings))
(el-get-bundle popwin)

;; perspeen (multi workspaces)
(el-get-bundle seudut/perspeen
  :features perspeen
  (setq perspeen-use-tab t)
  (perspeen-mode))

;; atomic-chrome
(el-get-bundle atomic-chrome (atomic-chrome-start-server))

;; helm
(el-get-bundle helm)
;; helm plugins
(el-get-bundle helm-swoop)
(el-get-bundle helm-descbinds)
(el-get-bundle helm-gtags)
(el-get-bundle helm-ag)
(el-get-bundle jimo1001/helm-perspeen)

;; company
(el-get-bundle company-mode/company-mode :name company-mode)

;; yasnippet
(el-get-bundle yasnippet)
(el-get-bundle yasnippet-snippets)

;; flycheck
(el-get-bundle flycheck)
(el-get-bundle flycheck/flycheck-popup-tip
  :depends (flycheck popup))
(el-get-bundle flycheck-irony)
(el-get-bundle flycheck/flycheck-google-cpplint
  :depends (flycheck))

;; irony-mode
(el-get-bundle irony-mode)
(el-get-bundle company-irony)

;; projectile
(el-get-bundle projectile)
(el-get-bundle helm-projectile)

;; doxymacs
(el-get-bundle doxymacs)

;; C/C++
(el-get-bundle cmake-mode)
(el-get-bundle google-c-style)
(el-get-bundle clang-format)
(el-get-bundle company-c-headers)
(el-get-bundle ludwigpacifici/modern-cpp-font-lock)

;; python
(el-get-bundle syohex/emacs-company-jedi
  :name company-jedi
  :depends-on (jedi-core company-mode))

;; tex
(el-get-bundle yatex)
(el-get-bundle latex-math-preview
  :type git
  :url "https://gitlab.com/latex-math-preview/latex-math-preview.git")

;; markdown
(el-get-bundle markdown-mode)

;; magit
(el-get-bundle magit)

;; rtags
(el-get-bundle rtags)

;; smartrep
(el-get-bundle myuhe/smartrep.el)

