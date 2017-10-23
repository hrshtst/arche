;; rtags
(custom-set-variables '(rtags-display-result-backend 'helm))

(defun buffer-local-set-key (key func)
      (interactive "KSet key on this buffer: \naCommand: ")
      (let ((name (format "%s-magic" (buffer-name))))
        (eval
         `(define-minor-mode ,(intern name)
            "Automagically built minor mode to define buffer-local keys."))
        (let* ((mapname (format "%s-map" name))
               (map (intern mapname)))
          (unless (boundp (intern mapname))
            (set map (make-sparse-keymap)))
          (eval
           `(define-key ,map ,key func)))
        (funcall (intern name) t)))

(require 'rtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (rtags-is-indexed)
              (buffer-local-set-key (kbd "M-t") 'rtags-find-symbol-at-point)
              (buffer-local-set-key (kbd "M-T") 'rtags-find-symbol)
              (buffer-local-set-key (kbd "M-r") 'rtags-find-references-at-point)
              (buffer-local-set-key (kbd "M-R") 'rtags-find-references)
              (buffer-local-set-key (kbd "C-t") 'rtags-location-stack-back)
              (buffer-local-set-key (kbd "C-S-t") 'rtags-location-stack-forward))))
