(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(when (executable-find "pandoc")
  (setq markdown-command "pandoc -s --self-contained -t html5 -c ~/.pandoc/github-markdown.css"))
