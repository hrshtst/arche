;; setting for doxymacs
(custom-set-variables
 '(doxymacs-doxygen-style "C++"))

(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))

(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
(add-hook 'c-mode-common-hook 'doxymacs-mode)
