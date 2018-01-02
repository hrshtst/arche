;; flycheck
(defun my/flycheck-list-errors ()
  (interactive)
  (unless (bound-and-true-p flycheck-mode)
    (flycheck-mode +1))
  (call-interactively 'flycheck-list-errors))

(custom-set-variables
 '(flycheck-display-errors-function
   (lambda (errors)
     (let ((messages (mapcar #'flycheck-error-message errors)))
       (popup-tip (mapconcat 'identity messages "\n")))))
 '(flycheck-display-errors-delay 0.5)
 '(flycheck-idle-change-delay 1.0))

(defvar flycheck-c/c++-include-path)
(setq flycheck-c/c++-include-path
      (list "." ".." "./include" "../include" (expand-file-name "~/usr/include") "./gtest/include"))
(defun my/set-flycheck-c/c++-include-path ()
  (setq flycheck-clang-include-path flycheck-c/c++-include-path)
  (setq flycheck-gcc-include-path flycheck-c/c++-include-path))
(add-hook 'c-mode-hook 'my/set-flycheck-c/c++-include-path)
(add-hook 'c++-mode-hook 'my/set-flycheck-c/c++-include-path)
(add-hook 'c++-mode-hook '(lambda ()
                            (setq flycheck-clang-language-standard "c++11")
                            (setq flycheck-gcc-language-standard "c++11")))

;; keybinding
(define-key global-map (kbd "M-l") 'my/flycheck-list-errors)
(define-key flycheck-mode-map (kbd "M-n") 'flycheck-next-error)
(define-key flycheck-mode-map (kbd "M-p") 'flycheck-previous-error)
(smartrep-define-key
    global-map "M-g" '(("M-n" . 'flycheck-next-error)
                       ("M-p" . 'flycheck-previous-error)))

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
(add-hook 'after-init-hook #'global-flycheck-mode)
