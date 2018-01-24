;; setting for projectile
(defun my/projectile-test-prefix (project-type) "test_")
(defun my/projectile-test-prefix (project-type)
  "Find default test files prefix based on PROJECT-TYPE."
  (cl-flet ((prefix (&optional pfx)
                    (projectile-project-type-attribute project-type 'test-prefix pfx)))
    (cond
     ((member project-type '(django python-pip python-pkg python-tox))  (prefix "test_"))
     ((member project-type '(emacs-cask)) (prefix "test-"))
     ((member project-type '(lein-midje)) (prefix "t_"))
     ((member project-type '(cmake)) (prefix "test_"))
     (t (prefix)))))

(custom-set-variables
 '(projectile-completion-system 'helm)
 '(projectile-test-prefix-function 'my/projectile-test-prefix))

(projectile-global-mode)
(helm-projectile-on)
