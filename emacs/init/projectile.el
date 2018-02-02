;; setting for projectile
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

(defun my/projectile-test-suffix (project-type)
  "Find default test files suffix based on PROJECT-TYPE."
  (cl-flet ((suffix (&optional sfx)
                    (projectile-project-type-attribute project-type 'test-suffix sfx)))
    (cond
     ((member project-type '(rebar)) (suffix "_SUITE"))
     ((member project-type '(emacs-cask)) (suffix "-test"))
     ((member project-type '(rails-rspec ruby-rspec)) (suffix "_spec"))
     ((member project-type '(rails-test ruby-test lein-test boot-clj go elixir)) (suffix "_test"))
     ((member project-type '(scons)) (suffix "test"))
     ((member project-type '(maven symfony)) (suffix "Test"))
     ((member project-type '(gradle gradlew grails)) (suffix "Spec"))
     ((member project-type '(haskell-cabal haskell-stack sbt)) (suffix "Spec"))
     ((member project-type '(cmake)) (suffix "_test"))
     (t (suffix)))))

(custom-set-variables
 '(projectile-completion-system 'helm)
 '(projectile-test-prefix-function 'my/projectile-test-prefix)
 '(projectile-test-suffix-function 'my/projectile-test-suffix))

(projectile-global-mode)
(helm-projectile-on)

;; keybindings
(global-set-key [f5] 'projectile-compile-project)
(global-set-key [f6] 'projectile-test-project)
