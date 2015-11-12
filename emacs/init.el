(when load-file-name
  (setq-default user-emacs-directory (file-name-directory load-file-name)))

(load (concat user-emacs-directory "init-el-get.el"))

;; init-loader
(custom-set-variables
 '(init-loader-show-log-after-init 'error-only))
(init-loader-load (concat user-emacs-directory "init-loader"))
