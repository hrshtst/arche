;; dired
(with-eval-after-load 'dired
  ;; Not create new buffer, if you chenge directory in dired
  (put 'dired-find-alternate-file 'disabled nil)

  ;; binding
  (define-key dired-mode-map (kbd "C-M-u") 'dired-up-directory)
  (define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "a") 'dired-find-file))

(custom-set-variables
 '(dired-dwim-target t)
 '(dired-auto-revert-buffer t)
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'top)
 '(dired-isearch-filenames t))

(custom-set-faces
  '(dired-directory ((t (:foreground "#5fafff" :weight bold))))
  '(dired-symlink ((t (:foreground "#5fd7ff" :weight bold))))
  '(dired-flagged ((t (:background "red" :foreground "#eee" :underline t :weight bold))))
  '(dired-marked ((t (:underline t :weight bold))))
)

(diredfl-global-mode)
