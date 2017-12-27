;; undo setting
(setq-default undo-no-redo t
              undo-limit 600000
              undo-strong-limit 900000)

;;;; undo-tree
(global-undo-tree-mode)
(define-key undo-tree-map (kbd "C-/") 'undo-tree-undo)
(define-key undo-tree-map (kbd "M-_") 'nil)
(smartrep-define-key
    undo-tree-map "C-x" '(("u" . 'undo-tree-undo)
                          ("U" . 'undo-tree-redo)))
