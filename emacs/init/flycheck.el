;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

(defun my/flycheck-list-errors ()
  (interactive)
  (unless (bound-and-true-p flycheck-mode)
    (flycheck-mode +1))
  (call-interactively 'flycheck-list-errors))

(custom-set-variables
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

;; simply highlight the whole line
(setq flycheck-highlighting-mode 'lines)

(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode)
  (flycheck-irony-setup))
