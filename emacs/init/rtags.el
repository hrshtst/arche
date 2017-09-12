;; rtags
(custom-set-variables '(rtags-use-helm t))

(require 'rtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (rtags-is-indexed)
              (local-set-key (kbd "M-t") 'rtags-find-symbol-at-point)
              (local-set-key (kbd "M-T") 'rtags-find-symbol)
              (local-set-key (kbd "M-r") 'rtags-find-references-at-point)
              (local-set-key (kbd "M-R") 'rtags-find-references)
              (local-set-key (kbd "C-t") 'rtags-location-stack-back)
              (local-set-key (kbd "C-T") 'rtags-location-stack-forward))))
