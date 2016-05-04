;; font
(let ((size (if (>= (x-display-pixel-width) 3000) 18 13.5)))
  (condition-case err
      (let ((myfont (format "Ricty Discord-%d" size)))
        (set-frame-font myfont)
        (add-to-list 'default-frame-alist `(font . ,myfont)))
    (error (message "%s" err))))

;; migemo
(when (executable-find "cmigemo")
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
  (setq migemo-coding-system 'utf-8-unix)
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (load-library "migemo")
  (migemo-init))
