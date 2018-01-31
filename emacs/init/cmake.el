;; cmake-mode
;; http://syohex.hatenablog.com/entry/2017/02/22/213210
(defvar my/company-cmake-prefix nil)
(defun my/company-completion-started (unused)
  (let ((prefix (company-grab-symbol)))
    (setq my/company-cmake-prefix (and (not (string-empty-p prefix)) (string= prefix (upcase prefix))))))

(defun my/company-completion-finished (result)
  (when my/company-cmake-prefix
    (delete-char (- (length result)))
    (insert (upcase result))))

(defun my/cmake-mode-hook ()
  (add-hook 'company-completion-started-hook #'my/company-completion-started nil t)
  (add-hook 'company-completion-finished-hook #'my/company-completion-finished nil t)
  (setq-local company-backends '((company-yasnippet company-cmake))))
(add-hook 'cmake-mode-hook #'my/cmake-mode-hook)
