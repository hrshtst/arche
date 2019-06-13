;;; init.el --- My personal configuration file for Emacs -*- lexical-binding: t -*-

;; Copyright (c) 2016-2019 Hiroshi Atsuta

;; Author: Hiroshi Atsuta <atsuta.hiroshi@gmail.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This is my personal configuration file for Emacs. The word "arche",
;; which is a Greek word with senses "beginning", "origin" or "source
;; of action", is used for namespace partitioning. For the most part,
;; code written by Radon Rosborough
;; (https://github.com/raxod502/radian) is modified and reused.

;;; License:

;; Copyright 2016-2019 Hiroshi Atsuta

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;; Load utility libraries

(require 'cl-lib)
(require 'map)

;; Define customization group named "arche".

(defgroup arche nil
  "Customize Emacs configuration."
  :prefix "arche-"
  :group 'emacs)

;;; Define utility functions

(defmacro arche-defadvice (name arglist where place docstring &rest body)
  "Define an advice called NAME and add it to a function.
ARGLIST is as in `defun'. WHERE is a keyword as passed to
`advice-add', and PLACE is the function to which to add the
advice, like in `advice-add'. DOCSTRING and BODY are as in
`defun'."
  (declare (indent 2)
           (doc-string 5))
  (unless (stringp docstring)
    (error "arche-defadvice: no docstring provided"))
  `(progn
     (defun ,name ,arglist
       ,(let ((article (if (string-match-p "^:[aeiou]" (symbol-name where))
                           "an"
                         "a")))
          (format "%s\n\nThis is %s `%S' advice for `%S'."
                  docstring article where
                  (if (and (listp place)
                           (memq (car place) ''function))
                      (cadr place)
                    place)))
       ,@body)
     (advice-add ',place ',where #',name)
     ',name))

(defmacro arche-defhook (name arglist hook docstring &rest body)
  "Define a function called NAME and add it to a hook.
ARGLIST is as in `defun'. HOOK is the hook to which to add the
function. DOCSTRING and BODY are as in `defun'."
  (declare (indent 2)
           (doc-string 4))
  (unless (string-match-p "-hook$" (symbol-name hook))
    (error "Symbol `%S' is not a hook" hook))
  (unless (stringp docstring)
    (error "arche-defhook: no docstring provided"))
  `(progn
     (defun ,name ,arglist
       ,(format "%s\n\nThis function is for use in `%S'."
                docstring hook)
       ,@body)
     (add-hook ',hook ',name)))

;;; Start Emacs with appropriate setting

;; Change working directory to HOME unless non-default
;; initialization file is specified.
(unless load-file-name
  (cd (getenv "HOME")))

;; If non-default initialiation file is specified, set
;; `user-emacs-directory' to its directory.
(when load-file-name
  (setq-default user-emacs-directory
                (file-name-directory load-file-name)))

;; Set `user-init-file' to user initilization file.
(setq-default user-init-file
              (if load-file-name load-file-name
                (expand-file-name "init.el" user-emacs-directory)))

;; Make sure Emacs is running on modern version, otherwise abort.
(let ((minimum-emacs-version "26"))
  (when (version< emacs-version minimum-emacs-version)
    (error "Requires at least Emacs %s, but running Emacs %s"
           minimum-emacs-version emacs-version)))

;; Prevent package.el from installing any packages
(setq package-enable-at-startup nil)

;; Prevent this file being modified by custom
(setq custom-file (expand-file-name
                   (format "custom-%d-%d.el" (emacs-pid) (random))
                   temporary-file-directory))

;;; Package management
;;;; straight.el

;; Use the develop branch of straight.el on Radian's develop branch.
(setq straight-repository-branch "develop")

;; If watchexec and Python are installed, use file watchers to detect
;; package modifications. This saves time at startup. Otherwise, use
;; the ever-reliable find(1).
(if (and (executable-find "watchexec")
         (executable-find "python3"))
    (setq straight-check-for-modifications '(watch-files find-when-checking))
  (setq straight-check-for-modifications '(find-at-startup find-when-checking)))

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;; use-package

;; Package `use-package' allows us to make package configuration much
;; more confortable and provides lazy loading in very easy way.
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-always-defer t)

(defmacro use-feature (name &rest args)
  "Like `use-package', but with `straight-use-package-by-default' disabled."
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))

;;;; blackout

;; Package `blackout' provides a function to hide or customize the
;; display of major and minor modes in the mode line.
(use-package blackout
  :straight (:host github :repo "raxod502/blackout")
  :demand t)

;;; Keep ~/.emacs.d clean

;; Package `no-littering' changes the default paths for lots of
;; different packages. As a result, ~/.emacs.d is much more clean
;; and organized.
(use-package no-littering
  :demand t)

;;; Function tweaking

;; Package `el-patch' provides a way to customize the behavior of
;; Emacs Lisp functions that do not provide variables and hooks to
;; let us make them what we want. Also, this can be used to override
;; the definition of an internal function from another package to
;; make them work as desired.
(use-package el-patch
  :straight (:host github
                   :repo "raxod502/el-patch"
                   :branch "develop")
  :demand t)

;;; Keybindings

;; Package `bind-key' provides a useful macro which is much prettier
;; and surely takes effect.
(use-package bind-key)

(defvar arche-keymap (make-sparse-keymap)
  "Keymap for my own commands is bound under M-l.")

(bind-key* "M-l" arche-keymap)

(defmacro arche-bind-key (key-name command &optional predicate)
  "Bind a key in `arche-keymap'."
  `(bind-key ,key-name ,command arche-keymap ,predicate))

;; Package `hydra' provides a feature to tie related commands into a
;; family of short bindings with a common prefix.
(use-package hydra)

;;; Environment variables

;;;; exec-path-from-shell

;; Package `exec-path-from-shell' ensures environment variables
;; inside Emacs are copied from the user's shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))

  :demand t

  :config

  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("GOPATH")))

;;; Clipboard integration

;; If you have something on the system clipboard, and then kill
;; something in Emacs, then by default whatever you had on the system
;; clipboard is gone and there is no way to get it back. Setting the
;; following option makes it so that when you kill something in Emacs,
;; whatever was previously on the system clipboard is pushed into the
;; kill ring. This way, you can paste it with `yank-pop'.
(setq save-interprogram-paste-before-kill t)

;;; Candidate selection

;; Package `ivy' provides a user interface for choosing from a list of
;; options by typing a query to narrow the list, and then selecting
;; one of the remaining candidates. This offers a significant
;; improvement over the default Emacs interface for candidate
;; selection.
(use-package ivy
  :init

  (ivy-mode +1)

  :bind* (("C-c C-r" . ivy-resume)
          ("C-;" . ivy-switch-buffer))

  :config

  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-extra-directories nil)
  (setq enable-recursive-minibuffers t)

  :blackout t)

;; Package `ivy-hydra' provides the C-o binding for Ivy menus which
;; allows you to pick from a set of options for what to do with a
;; selected candidate.
(use-package ivy-hydra)

;; Package `counsel' provides purpose-built replacements for many
;; built-in Emacs commands that use enhanced configurations of `ivy'
;; to provide extra features.
(use-package counsel
  :init/el-patch

  (defvar counsel-mode-map
    (let ((map (make-sparse-keymap)))
      (dolist (binding
               '((execute-extended-command . counsel-M-x)
                 (describe-bindings . counsel-descbinds)
                 (el-patch-remove
                   (describe-function . counsel-describe-function)
                   (describe-variable . counsel-describe-variable))
                 (apropos-command . counsel-apropos)
                 (describe-face . counsel-describe-face)
                 (list-faces-display . counsel-faces)
                 (find-file . counsel-find-file)
                 (find-library . counsel-find-library)
                 (imenu . counsel-imenu)
                 (load-library . counsel-load-library)
                 (load-theme . counsel-load-theme)
                 (yank-pop . counsel-yank-pop)
                 (info-lookup-symbol . counsel-info-lookup-symbol)
                 (pop-to-mark-command . counsel-mark-ring)
                 (bookmark-jump . counsel-bookmark)))
        (define-key map (vector 'remap (car binding)) (cdr binding)))
      map)
    (el-patch-concat
      "Map for `counsel-mode'.
Remaps built-in functions to counsel replacements."
      (el-patch-add
        "\n\nBindings that are remapped by `helpful' have been removed.")))

  :init

  (counsel-mode +1)

  :bind (("C-c c" . counsel-find-file)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate)
         ("C-c m" . counsel-mark-ring)
         ("C-c d" . counsel-descbinds)
         ("C-x C-r" . counsel-recentf)

         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history))

  :blackout t)

;; Package `prescient' is a library for intelligent sorting and
;; filtering in various contexts.
(use-package prescient
  :config

  ;; Remember usage statistics across Emacs sessions.
  (prescient-persist-mode +1))

;; Package `ivy-prescient' provides intelligent sorting and filtering
;; for candidates in Ivy menus.
(use-package ivy-prescient
  :demand t
  :after ivy
  :config

  ;; Use `prescient' for Ivy menus.
  (ivy-prescient-mode +1))

;;; Window management

(arche-defadvice arche--advice-keyboard-quit-minibuffer-first
    (keyboard-quit)
  :around keyboard-quit
  "Cause \\[keyboard-quit] to exit the minibuffer, if it is active.
Normally, \\[keyboard-quit] will just act in the current buffer.
This advice modifies the behavior so that it will instead exit an
active minibuffer, even if the minibuffer is not selected."
  (if-let ((minibuffer (active-minibuffer-window)))
      (with-current-buffer (window-buffer minibuffer)
        (minibuffer-keyboard-quit))
    (funcall keyboard-quit)))

;; Feature `windmove' allows us to move between windows by S-left,
;; S-right, S-up, and S-down.
(use-feature windmove
  :demand t
  :config

  (windmove-default-keybindings)

  (defun arche-move-splitter-left (arg)
    "Move window splitter left."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (shrink-window-horizontally arg)
      (enlarge-window-horizontally arg)))

  (defun arche-move-splitter-right (arg)
    "Move window splitter right."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (enlarge-window-horizontally arg)
      (shrink-window-horizontally arg)))

  (defun arche-move-splitter-up (arg)
    "Move window splitter up."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (enlarge-window arg)
      (shrink-window arg)))

  (defun arche-move-splitter-down (arg)
    "Move window splitter down."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (shrink-window arg)
      (enlarge-window arg))))

;; Select another window in cyclic ordering with C-t.
(bind-key "C-t" #'other-window)

(defun other-window-backwards ()
    "Select window in backward ordering to `other-window' with
positive count."
    (interactive)
    (other-window -1))

(bind-key "C-S-t" #'other-window-backwards)

;; Package `ace-window' provides window selection with simple
;; operation. When more than two windows on the frame, calling
;; `ace-window' shows the first character of each window label upper
;; left of the window. Pressing that character will switch to that
;; window.
(use-package ace-window
  :bind ("M-o" . ace-window)
  :config

  ;; Initial characters used in window labels would like to be on the
  ;; home positions.
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l))

  ;; Make the face of leading character more visible.
  (set-face-attribute 'aw-leading-char-face nil
                      :foreground "yellow"
                      :weight 'bold
                      :height 3.0)

  ;; Always enable to issue dispatch functions for even one window.
  (setq aw-dispatch-always t))

;; Feature `winner' provides undo/redo function for window
;; configuration by C-c left and C-c right, respectively. For
;; instance, use C-x 1 to focus on a particular window, then return
;; to the previous layout with C-c left.
(use-feature winner
  :demand t
  :config

  (winner-mode +1))

;; Package `transpose-frame' provides interactive commands to
;; transpose windows arrangement: `flip-frame', `flop-frame',
;; `transpose-frame', `rotate-frame-clockwise',
;; `rotate-frame-anticlockwise', `rotate-frame'.
(use-package transpose-frame)

;; Package `buffer-move' provides interactive commands to swap
;; windows: `buf-move-up', `buf-move-down', `buf-move-left',
;; `buf-move-right'.
(use-package buffer-move
  :bind (("C-S-<up>" . buf-move-up)
         ("C-S-<down>" . buf-move-down)
         ("C-S-<left>" . buf-move-left)
         ("C-S-<right>" . buf-move-right)))

;; Feature `ibuffer' provides a more modern replacement for the
;; `list-buffers' command.
(use-feature ibuffer
  :bind (([remap list-buffers] . ibuffer)))

;; Package `eyebrowse' provides window configuration manager which
;; allows us to save several window configurations and switch to one
;; of them anytime.
(use-package eyebrowse
  :init

  (setq eyebrowse-keymap-prefix (kbd "C-z"))

  :bind (:map eyebrowse-mode-map
         ("C-z p" . eyebrowse-prev-window-config)
         ("C-z n" . eyebrowse-next-window-config)
         ("C-z SPC" . eyebrowse-last-window-config)
         ("C-z k" . eyebrowse-close-window-config)
         ("C-z ." . eyebrowse-rename-window-config)
         ("C-z ," . eyebrowse-switch-to-window-config)
         ("C-z c" . eyebrowse-create-window-config)
         ("C-z h" . winner-undo)
         ("C-z l" . winner-redo))

  :demand t
  :config

  ;; Wrap around when switching to the next/previous window config.
  (setq eyebrowse-wrap-around t)

  ;; Clean up and display the scratch buffer when creating new
  ;; workspace.
  (setq eyebrowse-new-workspace t)

  (eyebrowse-mode +1))

;; Package `pc-bufsw' provides a quick buffer switcher, which switches
;; buffers according to most recently used order with C-TAB and
;; C-S-TAB.
(use-package pc-bufsw
  :demand t
  :config

  (pc-bufsw t))

;; Define keybindings for window management with hydra.
(defhydra hydra-window (:hint nil)
  "
^Movement^^^^^  ^Split^^^      ^Delete^      ^Switch^   ^Resize^
^^^^^^^^^^^^^^^------------------------------------------------
^ ^ _k_ ^ ^     _v_,_|_: vert  _d_lt this    _a_ce      ^ ^ _K_ ^ ^
_h_ ^+^ _l_     _x_,___: horz  _D_lt other   _s_wap     _H_ ^+^ _L_
^ ^ _j_ ^ ^     _z_ ^ ^: undo  _o_nly this   ^ ^        ^ ^ _J_ ^ ^
^^^^^^          _Z_ ^ ^: redo  _O_nly other
_q_uit
"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("H" arche-move-splitter-left)
  ("J" arche-move-splitter-down)
  ("K" arche-move-splitter-up)
  ("L" arche-move-splitter-right)
  ("v" split-window-right)
  ("x" split-window-below)
  ("|" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)))
  ("_" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)))
  ("z" winner-undo)
  ("Z" winner-redo)
  ("d" delete-window)
  ("D" ace-delete-window)
  ("o" delete-other-windows :exit t)
  ("O" ace-delete-other-windows :exit t)
  ("a" ace-window :exit t)
  ("s" ace-swap-window)
  ("q" nil))

(bind-key "C-M-o" #'hydra-window/body)

;;; Finding files

;; Follow symlinks when opening files.
(setq find-file-visit-truename t)

;; Suppress warning "X and Y are the same file".
(setq find-file-suppress-same-file-warnings t)

;; Feature `recentf' builds a list of recently opend files.
(use-feature recentf
  :demand t
  :config

  ;; Increase the number of saved items.
  (setq recentf-max-saved-items 2000)

  ;; Periodically execute clean-up in 10 minites.
  (setq recentf-auto-cleanup (* 10 60))

  ;; Run update of the list periodically.
  (run-at-time t (* 10 60) 'recentf-save-list)

  (recentf-mode +1))

;; Feature `saveplace' remembers place in files so as to move point
;; to saved position automatically when visiting them later.
(use-feature saveplace
  :demand t
  :config

  (save-place-mode +1))

;; Package `projectile' provides a set of functions to operate on
(use-package projectile
  :init/el-patch

  (defcustom projectile-keymap-prefix nil
    "Projectile keymap prefix."
    :group 'projectile
    :type 'string)

  (defvar projectile-command-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "4 a") #'projectile-find-other-file-other-window)
      (define-key map (kbd "4 b") #'projectile-switch-to-buffer-other-window)
      (define-key map (kbd "4 C-o") #'projectile-display-buffer)
      (define-key map (kbd "4 d") #'projectile-find-dir-other-window)
      (define-key map (kbd "4 D") #'projectile-dired-other-window)
      (define-key map (kbd "4 f") #'projectile-find-file-other-window)
      (define-key map (kbd "4 g") #'projectile-find-file-dwim-other-window)
      (define-key map (kbd "4 t") #'projectile-find-implementation-or-test-other-window)
      (define-key map (kbd "5 a") #'projectile-find-other-file-other-frame)
      (define-key map (kbd "5 b") #'projectile-switch-to-buffer-other-frame)
      (define-key map (kbd "5 d") #'projectile-find-dir-other-frame)
      (define-key map (kbd "5 D") #'projectile-dired-other-frame)
      (define-key map (kbd "5 f") #'projectile-find-file-other-frame)
      (define-key map (kbd "5 g") #'projectile-find-file-dwim-other-frame)
      (define-key map (kbd "5 t") #'projectile-find-implementation-or-test-other-frame)
      (define-key map (kbd "!") #'projectile-run-shell-command-in-root)
      (define-key map (kbd "&") #'projectile-run-async-shell-command-in-root)
      (define-key map (kbd "a") #'projectile-find-other-file)
      (define-key map (kbd "b") #'projectile-switch-to-buffer)
      (define-key map (kbd "C") #'projectile-configure-project)
      (define-key map (kbd "c") #'projectile-compile-project)
      (define-key map (kbd "d") #'projectile-find-dir)
      (define-key map (kbd "D") #'projectile-dired)
      (define-key map (kbd "e") #'projectile-recentf)
      (define-key map (kbd "E") #'projectile-edit-dir-locals)
      (define-key map (kbd "f") #'projectile-find-file)
      (define-key map (kbd "g") #'projectile-find-file-dwim)
      (define-key map (kbd "F") #'projectile-find-file-in-known-projects)
      (define-key map (kbd "i") #'projectile-invalidate-cache)
      (define-key map (kbd "I") #'projectile-ibuffer)
      (define-key map (kbd "j") #'projectile-find-tag)
      (define-key map (kbd "k") #'projectile-kill-buffers)
      (define-key map (kbd "l") #'projectile-find-file-in-directory)
      (define-key map (kbd "m") #'projectile-commander)
      (define-key map (kbd "o") #'projectile-multi-occur)
      (define-key map (kbd "p") #'projectile-switch-project)
      (define-key map (kbd "q") #'projectile-switch-open-project)
      (define-key map (kbd "P") #'projectile-test-project)
      (define-key map (kbd "r") #'projectile-replace)
      (define-key map (kbd "R") #'projectile-regenerate-tags)
      (define-key map (kbd "s g") #'projectile-grep)
      (define-key map (kbd "s r") #'projectile-ripgrep)
      (define-key map (kbd "s s") #'projectile-ag)
      (define-key map (kbd "S") #'projectile-save-project-buffers)
      (define-key map (kbd "t") #'projectile-toggle-between-implementation-and-test)
      (define-key map (kbd "T") #'projectile-find-test-file)
      (define-key map (kbd "u") #'projectile-run-project)
      (define-key map (kbd "v") #'projectile-vc)
      (define-key map (kbd "V") #'projectile-browse-dirty-projects)
      (define-key map (kbd "x e") #'projectile-run-eshell)
      (define-key map (kbd "x i") #'projectile-run-ielm)
      (define-key map (kbd "x t") #'projectile-run-term)
      (define-key map (kbd "x s") #'projectile-run-shell)
      (define-key map (kbd "z") #'projectile-cache-current-file)
      (define-key map (kbd "<left>") #'projectile-previous-project-buffer)
      (define-key map (kbd "<right>") #'projectile-next-project-buffer)
      (define-key map (kbd "ESC") #'projectile-project-buffers-other-buffer)
      map)
    "Keymap for Projectile commands after `projectile-keymap-prefix'.")
  (fset 'projectile-command-map projectile-command-map)

  (defvar projectile-mode-map
    (let ((map (make-sparse-keymap)))
      (when projectile-keymap-prefix
        (define-key map projectile-keymap-prefix 'projectile-command-map))
      (easy-menu-define projectile-mode-menu map
        "Menu for Projectile"
        '("Projectile"
          ["Find file" projectile-find-file]
          ["Find file in known projects" projectile-find-file-in-known-projects]
          ["Find test file" projectile-find-test-file]
          ["Find directory" projectile-find-dir]
          ["Find file in directory" projectile-find-file-in-directory]
          ["Find other file" projectile-find-other-file]
          ["Switch to buffer" projectile-switch-to-buffer]
          ["Jump between implementation file and test file" projectile-toggle-between-implementation-and-test]
          ["Kill project buffers" projectile-kill-buffers]
          ["Save project buffers" projectile-save-project-buffers]
          ["Recent files" projectile-recentf]
          ["Previous buffer" projectile-previous-project-buffer]
          ["Next buffer" projectile-next-project-buffer]
          "--"
          ["Toggle project wide read-only" projectile-toggle-project-read-only]
          ["Edit .dir-locals.el" projectile-edit-dir-locals]
          "--"
          ["Switch to project" projectile-switch-project]
          ["Switch to open project" projectile-switch-open-project]
          ["Discover projects in directory" projectile-discover-projects-in-directory]
          ["Browse dirty projects" projectile-browse-dirty-projects]
          ["Open project in dired" projectile-dired]
          "--"
          ["Search in project (grep)" projectile-grep]
          ["Search in project (ag)" projectile-ag]
          ["Replace in project" projectile-replace]
          ["Multi-occur in project" projectile-multi-occur]
          "--"
          ["Run shell" projectile-run-shell]
          ["Run eshell" projectile-run-eshell]
          ["Run ielm" projectile-run-ielm]
          ["Run term" projectile-run-term]
          "--"
          ["Cache current file" projectile-cache-current-file]
          ["Invalidate cache" projectile-invalidate-cache]
          ["Regenerate [e|g]tags" projectile-regenerate-tags]
          "--"
          ["Configure project" projectile-configure-project]
          ["Compile project" projectile-compile-project]
          ["Test project" projectile-test-project]
          ["Run project" projectile-run-project]
          ["Repeat last external command" projectile-repeat-last-command]
          "--"
          ["Project info" projectile-project-info]
          ["About" projectile-version]))
      map)
    "Keymap for Projectile mode.")

  :init

  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  (projectile-mode +1)

  :defer 1
  :config

  ;; Enable the mode again now that we have all the supporting hooks
  ;; and stuff defined.
  (projectile-mode +1)

  (put 'projectile-indexing-method 'safe-local-variable
       (lambda (arg) (memq arg '(native alien))))

  (projectile-register-project-type 'cmake '("CMakeLists.txt")
                                    :configure "cmake %s"
                                    :compile "cmake --build ."
                                    :test "ctest"
                                    :test-prefix "test_"
                                    :test-suffix"_test")

  :blackout t)

;; Package `counsel-projectile' provides alternate versions of
;; Projectile commands which use Counsel.
(use-package counsel-projectile
  :init/el-patch

  (defcustom counsel-projectile-key-bindings
    '((projectile-find-file        . counsel-projectile-find-file)
      (projectile-find-file-dwim   . counsel-projectile-find-file-dwim)
      (projectile-find-dir         . counsel-projectile-find-dir)
      (projectile-switch-to-buffer . counsel-projectile-switch-to-buffer)
      (projectile-grep             . counsel-projectile-grep)
      (projectile-ag               . counsel-projectile-ag)
      (projectile-ripgrep          . counsel-projectile-rg)
      (projectile-switch-project   . counsel-projectile-switch-project)
      (" "                         . counsel-projectile)
      ("si"                        . counsel-projectile-git-grep)
      ("Oc"                        . counsel-projectile-org-capture)
      ("Oa"                        . counsel-projectile-org-agenda))
    "Alist of counsel-projectile key bindings.

Each element is of the form \(KEY . DEF\) where KEY is either a
key sequence to bind in `projectile-command-map' or a projectile
command to remap in `projectile-mode-map', and DEF is the
counsel-projectile command to which KEY is remapped or bound."
    :type '(alist :key-type (choice (function :tag "Projectile command")
                                    key-sequence)
                  :value-type (function :tag "Counsel-projectile command"))
    :group 'counsel-projectile)

  (define-minor-mode counsel-projectile-mode
    "Toggle Counsel-Projectile mode on or off.

With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil, and toggle it if ARG is `toggle'.

Counsel-Projectile mode turns on Projectile mode, thus enabling
all projectile key bindings, and adds the counsel-projectile key
bindings on top of them.

The counsel-projectile key bindings either remap existing
projectile commands to their counsel-projectile replacements or
bind keys to counsel-projectile commands that have no projectile
counterparts."
    :group 'counsel-projectile
    :require 'counsel-projectile
    :global t
    (cond
     (counsel-projectile-mode
      (projectile-mode)
      (dolist (binding counsel-projectile-key-bindings)
        (if (functionp (car binding))
            (define-key projectile-mode-map `[remap ,(car binding)] (cdr binding))
          (define-key projectile-command-map (car binding) (cdr binding)))))
     (t
      (dolist (binding counsel-projectile-key-bindings)
        (if (functionp (car binding))
            (define-key projectile-mode-map `[remap ,(car binding)] nil)
          (define-key projectile-command-map (car binding) nil)))
      (projectile-mode -1))))

  :init

  (counsel-projectile-mode +1)

  :config

  ;; Sort files using `prescient', instead of just showing them in
  ;; lexicographic order.
  (setq counsel-projectile-sort-files t))

;;; Saving files

;; Store all backup files in temporary directory.
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))

;; Store all autosave files in temporary directory.
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Delete autosave files automatically.
(setq delete-auto-save-files t)

;; Don't make lockfiles.
(setq create-lockfiles nil)

;;; Editing
;;;; Language environment

;; Use Japanese input method by default.
(set-language-environment "Japanese")

;; Use UTF-8 as default coding system.
(prefer-coding-system 'utf-8-unix)

;; Set process coding system to UTF-8 on Windows.
(setq default-process-coding-system '(undecided-dos . utf-8-unix))

;; Package `mozc' provides Emacs with a Japanese IME interface for
;; Mozc.
(use-package mozc
  :if (executable-find "mozc_emacs_helper")

  :bind* (("S-SPC" . toggle-input-method))

  :demand t
  :config

  ;; Display an indicator when mozc-mode enabled.
  (setq mozc-leim-title "も"))

;; Package `mozc-im' provides `input-method-function' for Mozc, which
;; allows to use Mozc interface even in isearch field and ansi-term.
(use-package mozc-im
  :init

  (defvar-local mozc-im-mode nil
    "Whether `mozc-im-mode' is enabled or not. Non-nil means
    `mozc-im-mode' is enabled.")

  :demand t
  :after mozc
  :config

  (setq default-input-method "japanese-mozc-im")

  ;; Store whether mozc-im mode is enabled or not. Variable
  ;; `mozc-im-mode' is used for changing cursor color.
  (add-hook 'mozc-im-activate-hook (lambda () (setq mozc-im-mode t)))
  (add-hook 'mozc-im-deactivate-hook (lambda () (setq mozc-im-mode nil))))

;; Package `mozc-popup' provides candidate selection menu with popup
;; style, which significantly reduces a lag for showing up menu
;; implemented in the original mozc package.
(use-package mozc-popup
  :demand t
  :after mozc
  :config

  (setq mozc-candidate-style 'popup))

;; Package `ccc' provides buffer local cursor color control library.
;; For the sake of clarity, cursor color is changed to lighter color
;; when mozc mode is enabled.
(use-package ccc
  :commands (ccc-set-buffer-local-cursor-color)
  :init

  (defvar arche--cursor-color-mozc-enabled "orange"
    "Cursor color when mozc mode is enabled.")

  :hook ((input-method-activate . arche-cursor-color-change)
         (input-method-inactivate . arche-cursor-color-change))

  :after (:all mozc mozc-im)
  :demand t
  :config

  ;; Register ccc-related functions to appropriate hooks.
  (ccc-setup)

  (defun arche-cursor-color-change ()
    "Change cursor color depending on whether mozc-mode is
enabled. If mozc-mode is enabled, change cursor color to
`arche--cursor-color-mozc-enabled'."
    (if (or mozc-im-mode mozc-mode)
        (ccc-set-buffer-local-cursor-color arche--cursor-color-mozc-enabled)
      (ccc-set-buffer-local-cursor-color nil))))

;;;; Text formatting

;; When region is active, capitalize it.
(bind-key "M-c" #'capitalize-dwim)
(bind-key "M-d" #'downcase-dwim)
(bind-key "M-u" #'upcase-dwim)

;; When filling paragraphs, assume that sentences end with one space
;; rather than two.
(setq sentence-end-double-space nil)

;; Trigger auto-fill after punctutation characters, not just
;; whitespace.
(mapcar
 (lambda (c)
   (set-char-table-range auto-fill-chars c t))
 "!-=+]};:'\",.?")

(defun arche--auto-fill-disable ()
  "Disable `auto-fill-mode' in the current buffer."
  (auto-fill-mode -1))

(define-minor-mode arche-fix-whitespace-mode
  "Minor mode to automatically fix whitespace on save.
If enabled, then saving the buffer deletes all trailing
whitespace and ensures that the file ends with exactly one
newline."
  nil nil nil
  (if arche-fix-whitespace-mode
      (progn
        (setq require-final-newline t)
        (add-hook 'before-save-hook #'delete-trailing-whitespace nil 'local))
    (setq require-final-newline nil)
    (remove-hook 'before-save-hook #'delete-trailing-whitespace 'local)))

(define-globalized-minor-mode arche-fix-whitespace-global-mode
  arche-fix-whitespace-mode arche-fix-whitespace-mode)

(arche-fix-whitespace-global-mode +1)

(put 'arche-fix-whitespace-mode 'safe-local-variable #'booleanp)

;; Feature `whitespace' provides a minor mode for highlighting
;; whitespace.
(use-feature whitespace
  :init

  (define-minor-mode arche-highlight-long-lines-mode
    "Minor mode for highlighting long lines."
    nil nil nil
    (if arche-highlight-long-lines-mode
        (progn
          (setq-local whitespace-style '(face lines-tail))
          (setq-local whitespace-line-column 79)
          (whitespace-mode +1))
      (whitespace-mode -1)
      (kill-local-variable 'whitespace-style)
      (kill-local-variable 'whitespace-line-column)))

  :config

  ;; Show trailing whitespaces
  (setq show-trailing-whitespace t)

  :blackout t)

;; Feature `outline' provides major and minor modes for collapsing
;; sections of a buffer into an outline-like format.
(use-feature outline
  :demand t
  :config

  (define-globalized-minor-mode global-outline-minor-mode
    outline-minor-mode outline-minor-mode)

  (global-outline-minor-mode +1)

  :blackout outline-minor-mode)

;;;; Kill and yank

;; Eliminate duplicates in the kill ring.
(setq kill-do-not-save-duplicates t)

;; Remove text properties when yanking
(setq yank-excluded-properties t)

;; If point is at the beginning of a line, kill the entire line
;; including the following newline.
(setq kill-whole-line t)

;; Feature `delsel' allows us to delete selection.
(use-feature delsel
  :demand t
  :config

  (delete-selection-mode +1))

;; Use "C-h" as backspace.
(bind-key* "C-h" #'delete-backward-char)

;;;; Region selection

;; Bind `rectangle-mark-mode' to "C-c RET"
(bind-key "C-c RET" #'rectangle-mark-mode)

;; Package `expand-region' provides increase or decrease the selected
;; region by semantic units.
(use-package expand-region
  :bind (("C-=" . er/expand-region)))

;;;; Undo/redo

;; Package `undo-tree' replaces the default Emacs undo system, which
;; is confusing and difficult to use, with intuitive undo/redo
;; system.
(use-package undo-tree
  :demand t
  :bind (;; This overrides the default binding of M-/, which is to
         ;; `dabbrev-expand'.
         :map undo-tree-map
         ("M-/" . undo-tree-redo))
  :config

  (global-undo-tree-mode +1)

  ;; Disable undo-in-region because the implementation is very buggy
  ;; and usually lose undo history by accident.
  (setq undo-tree-enable-undo-in-region nil)

  :blackout t)

;;;; Navigation

;; Feature `subword' remaps word-based editing commands to
;; subword-based commands that handle symbols with mixed uppercase
;; and lowercase letters, e.g. "GtkWidget", "EmacsFrameClass",
;; "NSGraphicsContext".
(use-feature subword
  :demand t
  :config

  (global-subword-mode +1)

  :blackout t)

;; After typing C-u C-<SPC>, we can type just C-<SPC> to cycle mark
;; ring instead of C-u C-<SPC>.
(setq set-mark-command-repeat-pop t)

;; Package `back-button' provides an alternative way for navigation
;; which enables us to move the point back and forth over all the
;; positions where pushed the mark.
(use-package back-button
  :demand t
  :config

  (back-button-mode +1)

  :blackout t)

;; `exchange-point-and-mark' can be used to push the current point
;; to the mark ring then go to the one previous point in the mark
;; ring, but it highlights the region between them. This disables
;; highlighting after jumping.
(defun arche-exchange-point-and-mark ()
  "Disable highlight after `exchange-point-and-mark'."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark))

(bind-key "C-x C-x" 'arche-exchange-point-and-mark)

;; Package `avy' provides a fast navigation within the currently
;; displaying windows. Calling `avy-goto-char' with a couple of
;; characters shows words that the first of charcters match with the
;; given ones. Pressing the highlighted charcters makes the point jump
;; to it.
(use-package avy
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-timer)
         ("M-g M-g" . avy-goto-line)
         ("C-c C-j" . avy-resume))

  :config

  ;; Make background dark during selection.
  (setq avy-background t))

;;;; Find and replace

;; Package `visual-regexp' provides an alternative version of
;; `replace-regexp' with live visual feedback directly in the
;; buffer.
(use-package visual-regexp
  :bind (("M-%" . vr/query-replace)))

;; Package `visual-regexp-steroids' allows `visual-regexp' to use
;; modern regexp engines; for example, Python or Perl regexps.
(use-package visual-regexp-steroids
  :demand t
  :after visual-regexp
  :config

  ;; Use Emacs-style regular expressions by default, instead of
  ;; Python-style.
  (setq vr/engine 'emacs))

;; Feature `isearch' provides a basic and fast mechanism for jumping
;; forward or backward to occurrences of a given search string.
(use-feature isearch
  :config

  ;; Eliminate the 0.25s idle delay for isearch highlighting.
  (setq isearch-lazy-highlight-initial-delay 0))

;; Package `swiper' provides an alternative to `isearch' which instead
;; uses `ivy' to display and select from the results.
(use-package swiper
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

;;; Electricity: automatic things
;;;; Autorevert

;; Feature `autorevert' automatically reverts the buffer when its
;; visited file changes on disk.
(use-feature autorevert
  :defer 2
  :config

  (setq auto-revert-interval 1)

  (global-auto-revert-mode +1)

  :blackout auto-revert-mode)

;;;; Automatic parens paring

;; Package `smartparens' provides functions to deal with parens
;; pairs, highlight matching paired parens and provide keybindings
;; for operating on paired parens.
(use-package smartparens
  :bind* (("C-M-p" . sp-previous-sexp))
  :demand t
  :config

  ;; Load the default pair definitions.
  (require 'smartparens-config)

  ;; Enable Smartparens in all buffers.
  (smartparens-global-mode +1)

  ;; Set up keybindings for Smartparens functions with default
  ;; keybindings.
  (sp-use-smartparens-bindings)

  ;; Highlight matching parens.
  (show-smartparens-global-mode +1)

  ;; Prevent all transient highlighting of inserted pairs.
  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil)

  ;; Work around https://github.com/Fuco1/smartparens/issues/783.
  (setq sp-escape-quotes-after-insert nil)

  :blackout t)

;;;; Snippet expansion

;; Feature `abbrev' provides functionality for expanding user-defined
;; abbreviations.
(use-feature abbrev
  :blackout t)

;; Package `yasnippet' allows us to type an abbreviation and
;; automatically expand it into function templates.
(use-package yasnippet
  :commands (yas-minor-mode)

  :bind (:map yas-minor-mode-map
              ;; Disable TAB from expanding snippets.
              ("TAB" . nil)
              ("<tab>" . nil))

  :config

  ;; Reduce verbosity. Default is 3. Suppress messages about
  ;; successful snippet loading on Emacs init. Errors should still
  ;; be shown.
  (setq yas-verbosity 2)

  ;; Specify directory which contains personal snippets.
  (add-to-list 'yas-snippet-dirs (concat user-emacs-directory "snippets"))

  ;; Loading the snippet table is required before calling
  ;; `yas-minor-mode'.
  (yas-reload-all)

  :blackout yas-minor-mode)

;; Package `yasnippet-snippets' contains the standard collection of
;; snippets.
(use-package yasnippet-snippets)

;; Package `ivy-yasnippet' allows to choose a snippet and preview it
;; with ivy interface.
(use-package ivy-yasnippet
  :bind (("C-c y" . ivy-yasnippet)))

;;; IDE-like features
;;;; Language servers

;; Package `lsp-mode' is an Emacs client for the Language Server
;; Protocol, which provides IDE-like features.
(use-package lsp-mode
  :init

  (arche-defhook arche--enable-lsp ()
     prog-mode-hook
     "Enable `lsp-mode' for most programming modes."
     ;; `lsp-mode' requires `yas-minor-mode' enabled.
     (yas-minor-mode +1)
     (unless (derived-mode-p
              ;; `lsp-mode' doesn't support Elisp. There's nothing to
              ;; do for the *scratch* buffer.
              #'emacs-lisp-mode
              ;; Disable for modes that is a specialized framework
              ;; available for
              ;;#'python-mode
              )
       (lsp)))

  :config

  ;; Use Flycheck, not Flymake.
  (setq lsp-prefer-flymake nil))

;;;; Indentation

;; Don't use tabs for indentation.
(setq-default indent-tabs-mode nil)

;;;; Autocompletion

;; Package `company' provides a text completion framework.  It uses
;; pluggable back-ends and front-ends to retrieve and display
;; completion candidates.
(use-package company
  :init
  (global-company-mode +1)

  :defer 3

  :bind (;; The followings are keybindings that take effect whenever
         ;; the completions menu is visible.
         :map company-active-map

         ;; Invoke filtering by "C-s".
         ("C-s" . company-filter-candidates)

         ;; Make TAB always complete the current selection.
         ("<tab>" . company-complete-selection)
         ("TAB" . company-complete-selection)

         ;; The followings are keybindings that only take effect if
         ;; the user has explicitly interacted with Company.
         :filter (company-explicit-action-p)

         ;; Make RET trigger a completion if and only if the user has
         ;; explicitly interacted with Company, instead of always
         ;; doing so.
         ("<return>" . company-complete-selection)
         ("RET" . company-complete-selection))

  :bind* (;; Invoke company manually.
          ("M-TAB" . company-complete))

  :config

  ;; Always display the entire suggestion list onscreen, placing it
  ;; above the cursor if necessary.
  (setq company-tooltip-minimum company-tooltip-limit)

  ;; Always display suggestions in the tooltip, even if there is only
  ;; one. Also, don't display metadata in the echo area. (This
  ;; conflicts with ElDoc.)
  (setq company-frontends '(company-pseudo-tooltip-frontend))

  ;; Show quick-reference numbers in the tooltip. (Select a completion
  ;; with M-1 through M-0.)
  (setq company-show-numbers t)

  ;; Prevent non-matching input (which will dismiss the completions
  ;; menu), but only if the user interacts explicitly with Company.
  (setq company-require-match #'company-explicit-action-p)

  ;; Company appears to override our settings in `company-active-map'
  ;; based on `company-auto-complete-chars'. Turning it off ensures we
  ;; have full control.
  (setq company-auto-complete-chars nil)

  ;; Only search the current buffer to get suggestions for
  ;; `company-dabbrev' (a backend that creates suggestions from text
  ;; found in your buffers). This prevents Company from causing lag
  ;; once you have a lot of buffers open.
  (setq company-dabbrev-other-buffers nil)

  ;; Make the `company-dabbrev' backend fully case-sensitive, to
  ;; improve the UX when working with domain-specific words that have
  ;; particular casing.
  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-downcase nil)

  ;; When candidates in the autocompletion tooltip have additional
  ;; metadata, like a type signature, align that information to the
  ;; right-hand side. This usually makes it look neater.
  (setq company-tooltip-align-annotations t)

  ;; Make selecting item before first or after last wrap around.
  (setq company-selection-wrap-around t)

  ;; Add candidates from yasnippet to backends.
  (defun arche-company-backend-append-yas (backend)
    (if (and (listp backend) (member 'company-yasnippet backend))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (defun arche-company-backend-ensure-yas ()
    (setq company-backends (mapcar #'arche-company-backend-append-yas company-backends)))
  (arche-company-backend-ensure-yas)

  :blackout t)

;; Package `company-prescient' provides intelligent sorting and
;; filtering for candidates in Company completions.
(use-package company-prescient
  :demand t
  :after company
  :config

  ;; Use `prescient' for Company menus.
  (company-prescient-mode +1))

;; Package `company-lsp' provides a Company completion backend for
;; `lsp-mode'.
(use-package company-lsp
  :config

  ;; Ensure that yasnippet candidates appear in lsp-mode.
  (arche-company-backend-ensure-yas))

;;;; Jump to definition

;; Package `imenu-list' creates a buffer which is populated with the
;; current buffer's imenu entries. This buffer is shown as a sidebar.
(use-package imenu-list
  :bind (("<f10>" . imenu-list-smart-toggle))

  :config

  ;; Set the focus to the imenu-list buffer when activating.
  (setq imenu-list-focus-after-activation t)

  ;; Always shows the buffer with the constant size.
  (setq imenu-list-auto-resize nil))

;; Package `dumb-jump' is an Emacs "jump to definition" package with
;; support for multiple programming languages that favors "just
;; working". This means minimal configuration with absolutely no
;; stored indexes (TAGS) or persistent background processes.
(use-package dumb-jump
  :init

  (dumb-jump-mode +1)

  :bind (:map dumb-jump-mode-map
              ("M-Q" . dumb-jump-quick-look)
              ("M-t" . dumb-jump-back))

  :bind* (("C-M-s" . dumb-jump-go-prompt)
          ("C-x 4 g" . dumb-jump-go-other-window)
          ("C-x 4 s" . arche-dumb-jump-go-prompt-other-window))

  :config

  (defun arche-dumb-jump-go-prompt-other-window ()
    "Like `dumb-jump-go-prompt' but use a different window."
    (interactive)
    (let ((dumb-jump-window 'other))
      (dumb-jump-go-prompt))))

;;;; Display contextual metadata

;; Feature `eldoc' provides a minor mode which allows function
;; signatures or other metadata to be displayed in the echo area.
(use-feature eldoc
  :demand t
  :config

  ;; Always truncate ElDoc messages to one line. This prevents the
  ;; echo area from resizing itself unexpectedly when point is on a
  ;; variable with a multiline docstring.
  (setq eldoc-echo-area-use-multiline-p nil)

  ;; Original code from
  ;; https://github.com/PythonNut/emacs-config/blob/1a92a1ff1d563fa6a9d7281bbcaf85059c0c40d4/modules/config-intel.el#L130-L137,
  ;; thanks!
  (arche-defadvice arche--advice-disable-eldoc-on-flycheck
      (&rest _)
    :after-while eldoc-display-message-no-interference-p
    "Disable ElDoc when point is on a Flycheck overlay.
This prevents ElDoc and Flycheck from fighting over the echo
area."
    (not (and (bound-and-true-p flycheck-mode)
              (flycheck-overlay-errors-at (point)))))

  :blackout t)

;;;; Syntax checking and code linting

;; Package `flycheck' provides a framework for in-buffer error and
;; warning highlighting, or more generally syntax checking.
(use-package flycheck
  :defer 4
  :init

  (defun arche--flycheck-disable-checkers (&rest checkers)
    "Disable the given Flycheck syntax CHECKERS, symbols.
This function affects only the current buffer, and neither causes
nor requires Flycheck to be loaded."
    (unless (boundp 'flycheck-disabled-checkers)
      (setq flycheck-disabled-checkers nil))
    (make-local-variable 'flycheck-disabled-checkers)
    (dolist (checker checkers)
      (cl-pushnew checker flycheck-disabled-checkers)))

  :bind-keymap (("C-c !" . flycheck-command-map))

  :config

  (global-flycheck-mode +1)

  ;; Run a syntax check when changing buffers, just in case you
  ;; modified some other files that impact the current one. See
  ;; https://github.com/flycheck/flycheck/pull/1308.
  (add-to-list 'flycheck-check-syntax-automatically 'idle-buffer-switch)

  ;; For the above functionality, check syntax in a buffer that you
  ;; switched to only briefly. This allows "refreshing" the syntax
  ;; check state for several buffers quickly after e.g. changing a
  ;; config file.
  (setq flycheck-buffer-switch-check-intermediate-buffers t)

  ;; Display errors in the echo area after only 0.2 seconds, not 0.9.
  (setq flycheck-display-errors-delay 0.2)

  ;; Bind keys to navigation of errors
  (arche-bind-key "p" #'flycheck-previous-error)
  (arche-bind-key "n" #'flycheck-next-error)
  (arche-bind-key "l" #'flycheck-list-errors)

  ;; Define hydra keymap for flycheck
  (use-feature hydra
    :config

    (defhydra hydra-flycheck
      (:pre flycheck-list-errors
            :post (quit-windows-on "*Flycheck errors*")
            :hint nil)
      "Errors"
      ("f" flycheck-error-list-set-filter "Filter")
      ("j" flycheck-next-error            "Next")
      ("k" flycheck-previous-error        "Previous")
      ("h" flycheck-first-error           "First")
      ("l" (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
      ("q" nil))

    (bind-key "C-M-l" #'hydra-flycheck/body))

  :blackout t)

;; Package `lsp-ui' provides Flycheck integration for `lsp-mode', as
;; well as various other UI elements that integrate with `lsp-mode'.
(use-package lsp-ui

  :bind (:map lsp-ui-mode-map
    ("C-c f" . lsp-ui-sideline-apply-code-actions)
    ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
    ([remap xref-find-references] . lsp-ui-peek-find-references))

  :config

  (arche-defadvice arche--advice-lsp-ui-apply-single-fix (orig-fun &rest args)
    :around lsp-ui-sideline-apply-code-actions
    "Apply code fix immediately if only one is possible."
    (cl-letf* ((orig-completing-read (symbol-function #'completing-read))
               ((symbol-function #'completing-read)
                (lambda (prompt collection &rest args)
                  (if (= (safe-length collection) 1)
                      (car collection)
                    (apply orig-completing-read prompt collection args)))))
      (apply orig-fun args)))

  (setq lsp-ui-sideline-ignore-duplicate t))

;;; Language support
;;;; Plain text

;; Feature `text-mode' provides a major mode for editing plain text.
(use-feature text-mode
  :config

  (add-hook 'text-mode-hook #'auto-fill-mode)

  (arche-defhook arche--flycheck-text-setup ()
    text-mode-hook
    "Disable some Flycheck checkers for plain text."
    (arche--flycheck-disable-checkers 'proselint)))

;;;; Lisp languages

;; Feature `lisp-mode' provides a base major mode for Lisp languages,
;; and supporting functions for dealing with Lisp code.
(use-feature lisp-mode
  :init

  (add-to-list 'safe-local-variable-values
               '(lisp-indent-function . common-lisp-indent-function)))

;;;; C, C++, Objective-C, Java

;; Feature `cc-mode' provides major modes for C, C++, Objective-C, and
;; Java.
(use-feature cc-mode
  :config

  (arche-defadvice arche--advice-inhibit-c-submode-indicators (&rest _)
    :override c-update-modeline
    "Unconditionally inhibit CC submode indicators in the mode lighter.")

  (arche-defhook arche--c-mode-setup ()
    c-mode-common-hook
    "Enable `auto-line' and `hungry-delete' minor modes."
    (c-toggle-auto-hungry-state +1)))

;; Package `google-c-style' provides the google C/C++ coding style.
(use-package google-c-style
  :straight (:type git :host github :repo "google/styleguide" :branch "gh-pages")

  :hook ((c-mode-common . google-set-c-style)
         (c-mode-common . google-make-newline-indent)))

;; Package `modern-cpp-font-lock' provides syntax highliting support
;; for modern C++.
(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode)

  :blackout modern-c++-font-lock-mode)

;; Package `clang-format' provides functionality to use clang-format
;; with emacs integration.
(use-package clang-format
  :if (executable-find "clang-format")
  :init

  (defun arche-clang-format-buffer-on-projectile ()
    "Reformat buffer if .clang-format exists in the projectile root."
    (when (and (featurep 'projectile)
               (file-exists-p (expand-file-name ".clang-format" (projectile-project-root))))
      (clang-format-buffer)))

  (define-minor-mode arche-clang-format-buffer-on-projectile-mode
    "Minor mode to reformat buffer on save using clang-format if
    .clang-format is found in project root."  nil nil nil
    (if arche-clang-format-buffer-on-projectile-mode
        (add-hook 'before-save-hook #'arche-clang-format-buffer-on-projectile nil 'local)
      (remove-hook 'before-save-hook #'arche-clang-format-buffer-on-projectile 'local)))

  (put 'arche-clang-format-buffer-on-projectile-mode 'safe-local-variable #'booleanp)

  (add-hook 'c-mode-common-hook #'arche-clang-format-buffer-on-projectile-mode))

;;;; Go

;; Package `go-mode' provides a major mode for Go.
(use-package go-mode)

;;;; Markdown

;; Package `markdown-mode' provides a major mode for Markdown.
(use-package markdown-mode
  :init

  (when (executable-find "pandoc")
    (setq markdown-command "pandoc -s --self-contained -t html5 -c ~/.pandoc/github-markdown.css"))

  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))

  :bind (;; C-c C-s p is a really dumb binding, we prefer C-c C-s C-p.
         ;; Same for C-c C-s q.
         :map markdown-mode-style-map
              ("C-p" . markdown-insert-pre)
              ("C-q" . markdown-insert-blockquote))

  :config

  (arche-defhook arche--flycheck-markdown-setup ()
    markdown-mode-hook
    "Disable some Flycheck checkers for Markdown."
    (arche--flycheck-disable-checkers
     'markdown-markdownlint-cli
     'markdown-mdl
     'proselint))

  (arche-defadvice arche--disable-markdown-metadata-fontification (&rest _)
    :override markdown-match-generic-metadata
    "Prevent fontification of YAML metadata blocks in `markdown-mode'.
This prevents a mis-feature wherein if the first line of a
Markdown document has a colon in it, then it's distractingly and
usually wrongly fontified as a metadata block. See
https://github.com/jrblevin/markdown-mode/issues/328."
    (prog1 nil (goto-char (point-max)))))

;;;; Python

;; Feature `python' provides a major mode for Python.
(use-feature python
  :config

   ;; Django's coding standards style.
  (setq python-fill-docstring-style 'django)

  ;; Don't warn if guessing the indention fails, just set it to the value
  ;; of `python-indent-offset'.
  (setq python-indent-guess-indent-offset-verbose nil)

  (arche-defhook arche--python-no-reindent-on-colon ()
    python-mode-hook
    "Don't reindent on typing a colon.
See https://emacs.stackexchange.com/a/3338/12534."
    (setq electric-indent-chars (delq ?: electric-indent-chars))))

;; Package `pipenv' provides interactive commands wrapping the
;; Pipenv, a minor mode with a keymap for the useful commands, and a
;; high-level pipenv-activate / pipenv-deactivate interface for
;; virtual environment integration with Emacs session.
(use-package pipenv
  :hook (python-mode . pipenv-mode)

  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended)

  :blackout t)

;; Feature `rst-mode' provides a major mode for ReST.
(use-feature rst-mode
  :config

  (arche-defhook arche--flycheck-rst-setup ()
    rst-mode-hook
    "If inside Sphinx project, disable the `rst' Flycheck checker.
This prevents it from signalling spurious errors. See also
https://github.com/flycheck/flycheck/issues/953."
    (when (locate-dominating-file default-directory "conf.py")
      (arche--flycheck-disable-checkers 'rst))))

;; Package `pip-requirements' provides a major mode for
;; requirements.txt files used by Pip.
(use-package pip-requirements

  ;; The default mode lighter is "pip-require". Ew.
  :blackout "Requirements")

(use-package sphinx-doc
  :demand t
  :after python
  :config

  (add-hook 'python-mode-hook
            (lambda ()
              (sphinx-doc-mode +1)))

  :blackout t)

;;;; TeX
;; https://www.tug.org/begin.html

;; Package `auctex' provides major modes for TeX code, including
;; compiler and viewer integration.
(straight-use-package 'auctex)

;; Feature `tex' from package `auctex' provides the base major mode
;; for TeX.
(use-feature tex
  :init

  (arche-defhook arche--yasnippet-tex-setup ()
    TeX-mode-hook
    "Enable `yasnippet-minor-mode' for `TeX-mode'."
    (yas-minor-mode +1))

  :config

  ;; The following configuration is recommended in the manual at
  ;; https://www.gnu.org/software/auctex/manual/auctex/Quick-Start.html.
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)

  (setq-default TeX-master nil) ; Query for master file.

  (arche-defhook arche--flycheck-tex-setup ()
    TeX-mode-hook
    "Disable some Flycheck checkers in TeX buffers."
    (arche--flycheck-disable-checkers 'tex-chktex 'tex-lacheck)))

;; Feature `tex-buf' from package `auctex' provides support for
;; running TeX commands and displaying their output.
(use-feature tex-buf
  :config

  ;; Save buffers automatically when compiling, instead of prompting.
  (setq TeX-save-query nil))

;; Feature `latex' from package `auctex' provides the major mode for
;; LaTeX.
(use-feature latex
  :config

  ;; Don't be afraid to break inline math between lines.
  (setq LaTeX-fill-break-at-separators nil)

  ;; When inserting a left brace, delete the current selection first,
  ;; as per `delete-selection-mode'.
  (put 'LaTeX-insert-left-brace 'delete-selection t)

  (put 'LaTeX-using-Biber 'safe-local-variable #'booleanp))

;; Feature `font-latex' from package `auctex' provides the syntax
;; highlighting for the LaTeX major mode.
(use-feature font-latex
  :init

  ;; Do the following customizations before `font-latex' is loaded,
  ;; since otherwise we would have to call
  ;; `font-latex-update-sectioning-faces'.

  ;; Prevent superscripts and subscripts from being displayed in a
  ;; different font size.
  (setq font-latex-fontify-script nil)

  ;; Prevent section headers from being displayed in different font
  ;; sizes.
  (setq font-latex-fontify-sectioning 1))

;; Feature `reftex' is a specialized feature for support of labels,
;; references, citations, and the index in LaTeX.
(use-feature reftex
  :init

  (add-hook 'LaTeX-mode-hook 'turn-on-reftex))

;; Package `company-auctex' provides a Company backend that uses
;; information from AUCTeX for autocompletion.
(use-package company-auctex
  :demand t
  :after (:all company tex)
  :config

  (company-auctex-init))

;; Package `acutex-latexmk' provides LatexMk support for AUCTeX.
(use-package auctex-latexmk
  :demand t
  :after tex
  :config

  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  (auctex-latexmk-setup))

;; Package `latex-math-preview' provides preview of particular
;; region in LaTeX file and displays it.
(use-package latex-math-preview
  :config

  (setq preview-scale-function 1.2))

;;; Configuration file formats

;; Package `apache-mode' provides a major mode for .htaccess and
;; similar files.
(use-package apache-mode)

;; Package `crontab-mode' provides a major mode for crontab files.
(use-package crontab-mode)

;; Package `dockerfile-mode' provides a major mode for Dockerfiles.
(use-package dockerfile-mode)

;; Package `gitconfig-mode' provides a major mode for .gitconfig and
;; .gitmodules files.
(use-package gitconfig-mode)

;; Package `gitignore-mode' provides a major mode for .gitignore
;; files.
(use-package gitignore-mode)

;; Package `json-mode' provides a major mode for JSON.
(use-package json-mode
  :config

  (arche-defhook arche--fix-json-indentation ()
    json-mode-hook
    "Set the tab width to 2 for JSON."
    (setq-local tab-width 2)))

;; Package `pip-requirements' provides a major mode for
;; requirements.txt files used by Pip.
(use-package pip-requirements

  ;; The default mode lighter is "pip-require". Ew.
  :blackout "Requirements")

;; Package `pkgbuild-mode' provides a major mode for PKGBUILD files
;; used by Arch Linux and derivatives.
(use-package pkgbuild-mode)

;; Package `ssh-config-mode' provides major modes for files in ~/.ssh.
(use-package ssh-config-mode)

;; Package `terraform-mode' provides major modes for Terraform
;; configuration files.
(use-package terraform-mode)

;; Package `toml-mode' provides a major mode for TOML.
(use-package toml-mode
  ;; Correct the capitalization from "Toml" to "TOML".
  :blackout "TOML")

;; Package `yaml-mode' provides a major mode for YAML.
(use-package yaml-mode
  :config

  (add-hook 'yaml-mode-hook #'arche--auto-fill-disable))

;;; Introspection
;;;; Help

;; Package `helpful' is an alternative to the built-in Emacs help
;; that provides much more contextual information.
(use-package helpful
  :init

  (use-feature counsel
    :config

    ;; Have the alternate "help" action for `counsel-M-x' use Helpful
    ;; instead of the default Emacs help.
    (setf (nth 0 (alist-get "h" (plist-get ivy--actions-list 'counsel-M-x)
                            nil nil #'equal))
          (lambda (x) (helpful-function (intern x)))))

  :bind (;; Remap standard commands.
         ([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-symbol]   . helpful-symbol)
         ([remap describe-key]      . helpful-key)

         ;; Suggested bindings from the documentation at
         ;; https://github.com/Wilfred/helpful.

         :map help-map
         ("F" . helpful-function)
         ("M-f" . helpful-macro)
         ("C" . helpful-command)

         :map global-map
         ("C-c C-d" . helpful-at-point))

  :config

  ;; Make it so you can quit out of `helpful-key' with C-g, like for
  ;; every other command. Put this in a minor mode so it can be
  ;; disabled.
  (define-minor-mode arche-universal-keyboard-quit-mode
    "Minor mode for making C-g work in `helpful-key'."
    :global t
    (if arche-universal-keyboard-quit-mode
        (arche-defadvice arche--advice-helpful-key-allow-keyboard-quit
            (func &rest args)
          :before helpful-key
          "Make C-g work in `helpful-key'."
          ;; The docstring of `add-function' says that if we make our
          ;; advice interactive and the interactive spec is *not* a
          ;; function, then it overrides the original function's
          ;; interactive spec.
          (interactive
           (list
            (let ((ret (read-key-sequence "Press key: ")))
              (when (equal ret "\^G")
                (signal 'quit nil))
              ret))))
      (advice-remove
       #'helpful-key #'arche--advice-helpful-key-allow-keyboard-quit)))

  (arche-universal-keyboard-quit-mode +1))

;; Use "M-h" instead of "C-h".
(bind-key "M-h" #'help-for-help)

;;;; Keybindings

;; Package `which-key' is a minor mode for Emacs that displays the
;; key bindings following your currently entered incomplete command
;; (a prefix) in a popup.
(use-package which-key
  :demand t
  :config

  (which-key-mode +1)

  :blackout which-key-mode)

;;;; Custom

;; Feature `cus-edit' powers Customize buffers and related
;; functionality.
(use-feature cus-edit
  :config

  ;; Don't show the search box in Custom.
  (setq custom-search-field nil))

;;;; Emacs Lisp development

;; Feature `elisp-mode' provides the major mode for Emacs Lisp. It
;; also provides the major mode for the *scratch* buffer, which is
;; very similar but slightly different.
(use-feature elisp-mode
  :config

  (arche-defhook arche--flycheck-elisp-setup ()
    emacs-lisp-mode-hook
    "Disable some Flycheck checkers for Emacs Lisp."
    ;; These checkers suck at reporting error locations, so they're
    ;; actually quite distracting to work with.
    (arche--flycheck-disable-checkers 'emacs-lisp 'emacs-lisp-checkdoc))

  ;; Note that this function is actually defined in `elisp-mode'
  ;; because screw modularity.
  (arche-defadvice arche--advice-company-elisp-use-helpful
      (func &rest args)
    :around elisp--company-doc-buffer
    "Cause `company' to use Helpful to show Elisp documentation."
    (cl-letf (((symbol-function #'describe-function) #'helpful-function)
              ((symbol-function #'describe-variable) #'helpful-variable)
              ((symbol-function #'help-buffer) #'current-buffer))
      (apply func args)))

  ;; The default mode lighter has a space instead of a hyphen.
  ;; Disgusting!
  :blackout (lisp-interaction-mode . "Lisp-Interaction"))

(defun arche-reload-init ()
  (interactive)
  (message "Reloading init-file...")
  (load user-init-file nil 'nomessage)
  (message "Reloading init-file...done"))

(arche-bind-key "r" #'arche-reload-init)

(defun arche-eval-buffer-or-region (&optional start end)
  "Evaluate the current region, or the whole buffer if no region is active.
In Lisp code, START and END denote the region to be evaluated;
they default to `point-min' and `point-max' respectively.

If evaluating a buffer visiting this file, then delegate instead
to `arche-reload-init'."
  (interactive)
  (if (and (string= buffer-file-name user-init-file)
           (not (region-active-p)))
      (arche-reload-init)
    (let ((name nil))
      (if (region-active-p)
          (progn
            (setq start (region-beginning))
            (setq end (region-end))
            (setq name "region"))
        (setq start (point-min))
        (setq end (point-max))
        (setq name (buffer-name)))
      (let ((load-file-name (buffer-file-name)))
        (message "Evaluating %s..." name)
        (eval-region start end)
        (message "Evaluating %s...done" name)))))

(bind-key "C-c C-k" #'arche-eval-buffer-or-region)

;;;;; Emacs Lisp linting

;; Feature `checkdoc' provides some tools for validating Elisp
;; docstrings against common conventions.
(use-feature checkdoc
  :init

  ;; Not sure why this isn't included by default.
  (put 'checkdoc-package-keywords-flag 'safe-local-variable #'booleanp))

;; Package `elisp-lint', not installed, provides a linting framework
;; for Elisp code.
(use-feature elisp-lint
  :init

  ;; From the package. We need this because some packages set this as
  ;; a file-local variable, but we don't install the package so Emacs
  ;; doesn't know the variable is safe.
  (put 'elisp-lint-indent-specs 'safe-local-variable #'listp))

;; Package `package-lint' provides a command that lets you check for
;; common package.el packaging problems in your packages.
(use-package package-lint)

;;; Applications
;;;; Filesystem management

;; When deleting a file interactively, move it to the trash instead.
(setq delete-by-moving-to-trash t)

;;;;; Dired

;; For some reason, the autoloads from `dired-aux' and `dired-x' are
;; not loaded automatically. Do it.
(require 'dired-loaddefs)

;; Feature `dired' provides a simplistic filesystem manager in Emacs.
(use-feature dired
  :bind (:map dired-mode-map
              ("J" . dired-up-directory))
  :config

  ;; Disable the prompt about whether I want to kill the Dired
  ;; buffer for a deleted directory.
  (setq dired-clean-confirm-killing-deleted-buffers nil)

  ;; Instantly revert Dired buffers on re-visiting them, with no
  ;; message.
  (setq dired-auto-revert-buffer t))

(use-feature dired-x
  :bind (;; Bindings for jumping to the current directory in Dired.
         ("C-x C-j" . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window))
  :config

  ;; Prevent annoying "Omitted N lines" messages when auto-reverting.
  (setq dired-omit-verbose nil))

;;;; Version control

;; Feature `vc-hooks' provides hooks for the Emacs VC package. We
;; don't use VC, because Magit is superior in pretty much every way.
(use-feature vc-hooks
  :config

  ;; Disable VC. This improves performance and disables some annoying
  ;; warning messages and prompts, especially regarding symlinks. See
  ;; https://stackoverflow.com/a/6190338/3538165.
  (setq vc-handled-backends nil))

;; Feature `smerge-mode' provides an interactive mode for visualizing
;; and resolving Git merge conflicts.
(use-feature smerge-mode
  :blackout t)

;; Package `magit' is an interface to the version control system
;; Git, implemented as an Emacs package.
(use-package magit
  :bind (;; This is the primary entry point for Magit. Binding to C-x
         ;; g is recommended in the manual:
         ;; https://magit.vc/manual/magit.html#Getting-Started
         ("C-x g" . magit-status))

  :init

  ;; Suppress the message we get about "Turning on
  ;; magit-auto-revert-mode" when loading Magit.
  (setq magit-no-message '("Turning on magit-auto-revert-mode..."))

  :config

  ;; Enable C-c M-g as a shortcut to go to a popup of Magit commands
  ;; relevant to the current file.
  (global-magit-file-mode +1)

  ;; The default location for git-credential-cache is in
  ;; ~/.config/git/credential. However, if ~/.git-credential-cache/
  ;; exists, then it is used instead. Magit seems to be hardcoded to
  ;; use the latter, so here we override it to have more correct
  ;; behavior.
  (unless (file-exists-p "~/.git-credential-cache/")
    (let* ((xdg-config-home (or (getenv "XDG_CONFIG_HOME")
                                (expand-file-name "~/.config/")))
           (socket (expand-file-name "git/credential/socket" xdg-config-home)))
      (setq magit-credential-cache-daemon-socket socket)))

  ;; Don't try to save unsaved buffers when using Magit. We know
  ;; perfectly well that we need to save our buffers if we want Magit
  ;; to see them.
  (setq magit-save-repository-buffers nil)

  (transient-append-suffix 'magit-merge "-s"
    '("-u" "Allow unrelated" "--allow-unrelated-histories"))

  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash")))

;; Feature `git-commit' from package `magit' provides the commit
;; message editing capabilities of Magit.
(use-feature git-commit
  :config

  ;; Max length for commit message summary is 50 characters as per
  ;; https://chris.beams.io/posts/git-commit/.
  (setq git-commit-summary-max-length 50))

;; Package `forge' provides a GitHub/GitLab/etc. interface directly
;; within Magit.
(use-package forge
  :demand t
  :after magit)

;;;; External commands

;; Feature `compile' provides a way to run a shell command from Emacs
;; and view the output in real time, with errors and warnings
;; highlighted and hyperlinked.
(use-feature compile
  :init

  (arche-bind-key "m" #'compile)

  :config

  ;; Automatically scroll the Compilation buffer as output appears,
  ;; but stop at the first error.
  (setq compilation-scroll-output 'first-error)

  ;; Don't ask about saving buffers when invoking `compile'. Try to
  ;; save them all immediately using `save-some-buffers'.
  (setq compilation-ask-about-save nil)

  ;; Actually, don't bother saving buffers at all. That's dumb. We
  ;; know to save our buffers if we want them to be updated on disk.
  (setq compilation-save-buffers-predicate
        (lambda ()))

  (arche-defadvice arche--advice-compile-pop-to-buffer (buf)
    :filter-return compilation-start
    "Pop to compilation buffer on \\[compile]."
    (prog1 buf
      (select-window (get-buffer-window buf))))

  ;; Define hydra keymap for compilation buffer.
  (use-feature hydra
    :config

    (defhydra hydra-next-error (global-map "C-x" :hint nil)
      "
Compilation errors:
_j_: next error      _h_: first error    _q_uit
_k_: previous error  _l_: last error
"
      ("`" next-error)
      ("j" next-error :bind nil)
      ("k" previous-error :bind nil)
      ("h" first-error :bind nil)
      ("l" (progn (goto-char (point-max)) (previous-error)) :bind nil)
      ("q" nil)))

  (use-feature ansi-color
    :config

    (arche-defhook arche--colorize-compilation-buffer ()
      compilation-filter-hook
      "Colorize compilation buffer with ansi color."
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (point-min) (point-max))))))

;;;; Internet applications

;; Feature `browse-url' provides commands for opening URLs in
;; browsers.
(use-feature browse-url
  :init

  (defun arche-browse-url-predicate ()
    "Return non-nil if \\[browse-url-at-point] should be rebound."
    ;; All of these major modes provide more featureful bindings for
    ;; C-c C-o than `browse-url-at-point'.
    (not (derived-mode-p 'markdown-mode 'org-mode 'org-agenda-mode)))

  :bind* (:filter (arche-browse-url-predicate)
                  ("C-c C-o" . browse-url-at-point)))

;; Feature `bug-reference' provides a mechanism for hyperlinking issue
;; tracker references (like #20), so that you can open them in a web
;; browser easily.
(use-feature bug-reference
  :config

  (bind-key "C-c C-o" #'bug-reference-push-button bug-reference-map))

;; Package `git-link' provides a simple function M-x git-link which
;; copies to the kill ring a link to the current line of code or
;; selection on GitHub, GitLab, etc.
(use-package git-link
  :config

  ;; Link to a particular revision of a file rather than using the
  ;; branch name in the URL.
  (setq git-link-use-commit t))

;; Package `atomic-chrome' provides a way for you to edit textareas
;; in Chrome or Firefox using Emacs. See
;; https://chrome.google.com/webstore/detail/atomic-chrome/lhaoghhllmiaaagaffababmkdllgfcmc
;; for the Chrome extension.
(use-package atomic-chrome
  :defer 5
  :config

  (defvar-local arche-atomic-chrome-url nil
    "The URL of the text area being edited.")

  (defcustom arche-atomic-chrome-setup-hook nil
    "Hook run while setting up an `atomic-chrome' buffer."
    :type 'hook)

  (arche-defadvice arche--advice-atomic-chrome-setup (url)
    :after atomic-chrome-set-major-mode
    "Save the URL in `arche-atomic-chrome-url'.
Also run `arche-atomic-chrome-setup-hook'."
    (setq arche-atomic-chrome-url url)
    (run-hooks 'arche-atomic-chrome-setup-hook))

  ;; Edit in Markdown by default, because many sites support it and
  ;; it's not a big deal if the text area doesn't actually support
  ;; Markdown.
  (setq atomic-chrome-default-major-mode 'markdown-mode)

  ;; Edit in a specific mode for a specific website.
  (setq atomic-chrome-url-major-mode-alist
        '(("github\\.com" . gfm-mode)
          ("redmine" . textile-mode)))

  ;; Listen for requests from the Chrome/Firefox extension.
  (atomic-chrome-start-server))

;;;; Emacs profiling

;; Package `esup' allows you to run a child Emacs process with special
;; profiling functionality, and to collect timing results for each
;; form in your init-file.
(use-package esup
  :config

  ;; Work around a bug where esup tries to step into the byte-compiled
  ;; version of `cl-lib', and fails horribly.
  (setq esup-depth 0))

;;; Startup

;; Disable the *About GNU Emacs* buffer at startup, and go straight
;; for the scratch buffer.
(setq inhibit-startup-screen t)

;; Remove the initial *scratch* message. Start with a blank screen, we
;; know what we're doing.
(setq initial-scratch-message nil)

;;; Miscellaneous

;; Enable all disabled commands.
(setq disabled-command-function nil)

;; Disable warnings from obsolete advice system. They don't provide
;; useful diagnostic information and often they can't be fixed except
;; by changing packages upstream.
(setq ad-redefinition-action 'accept)

;; Answer yes or no questions with y or n.
(defalias 'yes-or-no-p 'y-or-n-p)

;;; Appearance

;; Allow you to resize frames however you want, not just in whole
;; columns. "The 80s called, they want their user interface back"
(setq frame-resize-pixelwise t)

;; Turn off the alarm bell.
(setq ring-bell-function #'ignore)

;; Display keystrokes in the echo area immediately, not after one
;; second. We can't set the delay to zero because somebody thought it
;; would be a good idea to have that value suppress keystroke display
;; entirely.
(setq echo-keystrokes 1e-6)

;; Don't blink the cursor on the opening paren when you insert a
;; closing paren, as we already have superior handling of that from
;; Smartparens.
(setq blink-matching-paren nil)

;; Disable the contextual menu that pops up when you right-click.
(unbind-key "<C-down-mouse-1>")

(defcustom arche-font nil
  "Default font, as a string. Nil means use the default.
This is passed to `set-frame-font'."
  :type '(choice string (const :tag "Default" nil)))

(defcustom arche-font-size nil
  "Default font size, in pixels. Nil means use the default."
  :type '(choice integer (const :tag "Default" nil)))

(when (display-graphic-p)

  ;; Disable the scroll bars.
  (scroll-bar-mode -1)

  ;; Disable the tool bar and menu bar. See
  ;; <https://github.com/raxod502/radian/issues/180> for why we do it
  ;; this way instead of via `tool-bar-mode' and `menu-bar-mode'.
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(menu-bar-lines . 0) default-frame-alist)

  ;; Prevent the cursor from blinking.
  (blink-cursor-mode -1)

  ;; Use Ricty font.
  (when (eq system-type 'gnu/linux)
    (custom-set-variables '(arche-font "Ricty Discord")))

  ;; When using large monitor, increase font size.
  (when (>= (x-display-pixel-width) 3000)
    (custom-set-variables '(arche-font-size 140)))

  ;; Set the default font size.
  (when arche-font-size
    (set-face-attribute 'default nil :height arche-font-size))

  ;; Set the default font.
  (when arche-font
    (set-frame-font arche-font 'keep-size t))

  ;; Use the same font for fixed-pitch text as the rest of Emacs.
  (set-face-attribute 'fixed-pitch nil :family 'unspecified))

;; For terminal Emacs only, disable the menu bar the proper way (using
;; `menu-bar-mode'). Unlike in windowed Emacs, this doesn't have a big
;; performance impact. Furthermore, if we don't do it this way in the
;; terminal, then you can see the menu bar during startup
;; unfortunately.
(unless (display-graphic-p)
  (menu-bar-mode -1))

;;;; Mode line

;; Display the end-of-line format more **mnemonic**.
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-unix "(LF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-undecided "(?)")

(defun arche--mode-line-buffer-coding-system-base ()
  "Return the name of the current buffer coding system base in
more mnemonic way than default one. This is not the complete list
of all the available coding systems, but rather most used ones."
  (let ((coding-system-name
         (symbol-name (coding-system-base buffer-file-coding-system))))
    (cond ((string-match "utf-8" coding-system-name) "U8")
          ((string-match "utf-16" coding-system-name) "U16")
          ((string-match "japanese-shift-jis" coding-system-name) "SJIS")
          ((string-match "cp\\([0-9]+\\)" coding-system-name)
           (match-string 1 coding-system-name))
          ((string-match "japanese-iso-8bit" coding-system-name) "EUC")
          ((string-match "undecided" coding-system-name) "-")
          (t "?"))))

(defun arche-mode-line-buffer-coding-system ()
  "Return the current buffer coding system with its end-of-line format."
  (format "%s%s"
          (arche--mode-line-buffer-coding-system-base)
          (mode-line-eol-desc)))

(defun arche-mode-line-input-method ()
  "Return the current input method."
  (if current-input-method-title
      (concat current-input-method-title ":") ""))

(defun arche-mode-line-buffer-modified-status ()
  "Return a mode line construct indicating buffer modification status.
This is [*] if the buffer has been modified and whitespace
otherwise. (Non-file-visiting buffers are never considered to be
modified.) It is shown in the same color as the buffer name, i.e.
`mode-line-buffer-id'."
  (propertize
   (if (and (buffer-modified-p)
            (buffer-file-name))
       "[*]"
     "   ")
   'face 'mode-line-buffer-id))

;; Normally the buffer name is right-padded with whitespace until it
;; is at least 12 characters. This is a waste of space, so we
;; eliminate the padding here. Check the docstrings for more
;; information.
(setq-default mode-line-buffer-identification
              (propertized-buffer-identification "%b"))

(defvar-local arche-mode-line-project-and-branch nil
  "Mode line construct showing Projectile project and Git status.
The format is [project:branch*], where the * is shown if the
working directory is dirty. Either component can be missing; this
might happen if Projectile is not available or if the project is
not version-controlled with Git. If nothing should be displayed,
this variable is set to nil.

This variable is actually only a cached value; it is set by
`arche-mode-line-compute-project-and-branch' for performance
reasons.

See also `arche-show-git-mode'.")

;; Don't clear the cache when switching major modes (or using M-x
;; normal-mode).
(put 'arche-mode-line-project-and-branch 'permanent-local t)

(defun arche--mode-line-recompute-project-and-branch ()
  "Recalculate and set `arche-mode-line-project-and-branch'.
Force a redisplay of the mode line if necessary. This is
buffer-local."
  (unless (file-remote-p default-directory)
    (condition-case-unless-debug err
        (let ((old arche-mode-line-project-and-branch)
              (new
               (let* (;; Don't insist on having Projectile loaded.
                      (project-name (when (featurep 'projectile)
                                      (projectile-project-name)))
                      ;; Projectile returns "-" to mean "no project".
                      ;; I'm still wondering what happens if someone
                      ;; makes a project named "-".
                      (project-name (unless (equal project-name "-")
                                      project-name))
                      ;; Check if we are actually in a Git repo, and Git
                      ;; is available, and we want to show the Git
                      ;; status.
                      (git (and
                            arche-show-git-mode
                            (executable-find "git")
                            (locate-dominating-file default-directory ".git")))
                      (branch-name
                       (when git
                         ;; Determine a reasonable string to show for
                         ;; the current branch. This is actually more or
                         ;; less the same logic as we use for the Radian
                         ;; Zsh prompt.
                         (with-temp-buffer
                           ;; First attempt uses symbolic-ref, which
                           ;; returns the branch name if it exists.
                           (ignore-errors
                             (call-process "git" nil '(t nil) nil
                                           "symbolic-ref" "HEAD"))
                           (if (> (buffer-size) 0)
                               ;; It actually returns something like
                               ;; refs/heads/master, though, so let's
                               ;; try to trim it if possible.
                               (let ((regex "^\\(refs/heads/\\)?\\(.+\\)$")
                                     (str (string-trim (buffer-string))))
                                 (if (string-match regex str)
                                     (match-string 2 str)
                                   ;; If it's something weird then just
                                   ;; show it literally.
                                   str))
                             ;; If symbolic-ref didn't return anything
                             ;; on stdout (we discarded stderr), we
                             ;; probably have a detached head and we
                             ;; should show the abbreviated commit hash
                             ;; (e.g. b007692).
                             (erase-buffer)
                             (ignore-errors
                               (call-process "git" nil '(t nil) nil
                                             "rev-parse" "--short" "HEAD"))
                             (if (> (buffer-size) 0)
                                 (string-trim (buffer-string))
                               ;; We shouldn't get here. Unfortunately,
                               ;; it turns out that we do every once in
                               ;; a while. (I have no idea why.)
                               "???")))))
                      (dirty (when git
                               (with-temp-buffer
                                 (ignore-errors
                                   (call-process "git" nil t nil
                                                 "status" "--porcelain"))
                                 (if (> (buffer-size) 0)
                                     "*" "")))))
                 (cond
                  ((and project-name git)
                   (format "  [%s:%s%s]" project-name branch-name dirty))
                  (project-name
                   (format "  [%s]" project-name))
                  ;; This should never happen unless you do something
                  ;; perverse like create a version-controlled
                  ;; Projectile project whose name is a hyphen, but we
                  ;; want to handle it anyway.
                  (git
                   (format "  [%s%s]" branch-name dirty))))))
          (unless (equal old new)
            (setq arche-mode-line-project-and-branch new)
            (force-mode-line-update)))
      (error
       ;; We should not usually get an error here. In the case that we
       ;; do, however, let's try to avoid displaying garbage data, and
       ;; instead delete the construct entirely from the mode line.
       (unless (null arche-mode-line-project-and-branch)
         (setq arche-mode-line-project-and-branch nil)
         (force-mode-line-update))))))

;; We will make sure this information is updated after some time of
;; inactivity, for the current buffer.

(defcustom arche-mode-line-update-delay 1
  "Seconds of inactivity before updating the mode line.
Specifically, this entails updating the Projectile project, Git
branch, and dirty status, which are the most computationally
taxing elements."
  :type 'number)

;; We only need one global timer pair for all the buffers, since we
;; will only be updating the cached mode line value for the current
;; buffer.
;;
;; The way this is set up, the main idle timer runs each time that
;; Emacs is idle for exactly one second. That triggers a recomputation
;; of the mode line, and also schedules the repeat timer, which
;; reschedules itself repeatedly. Why do we need two timers? If we
;; tried to use just the idle timer, then the recomputation would only
;; get scheduled once per idle session, one second in, instead of
;; going once per second after one second of initial idleness. If we
;; tried to use just the repeat timer, then we would get
;; ever-increasing delays before it would fire, in each new idle
;; session. Why? Because the pattern for scheduling an idle timer
;; repeatedly is to increase the idle delay, since the idle time is
;; not re-set just because a timer fired. And if the idle session ends
;; between timer fires, then the repeat timer will be stuck with a
;; really long idle delay, and won't fire again.

(defun arche--mode-line-recompute-and-reschedule ()
  "Compute mode line data and re-set timers.
The delay is `arche-mode-line-update-delay'. The timers are
`arche--mode-line-idle-timer' and
`arche--mode-line-repeat-timer'."

  ;; Cancel any existing timer (we wouldn't want to introduce
  ;; duplicate timers!), and do it early in a half-hearted attempt to
  ;; avoid race conditions.
  (when arche--mode-line-repeat-timer
    (cancel-timer arche--mode-line-repeat-timer))

  ;; Do the computation.
  (arche--mode-line-recompute-project-and-branch)

  ;; If Emacs is already idle (meaning that the main idle timer has
  ;; already been triggered, and won't go again), then we need to
  ;; schedule the repeat timer. Otherwise, the main idle timer will be
  ;; triggered when Emacs does become idle, and we don't need to
  ;; schedule anything. There's no need to clear an old repeat timer,
  ;; since the idle timer will always get called before the repeat
  ;; timer and that will cause the repeat timer to be re-set as below.
  (when (current-idle-time)
    (setq arche--mode-line-repeat-timer
          (run-with-idle-timer
           (time-add (current-idle-time) arche-mode-line-update-delay)
           nil #'arche--mode-line-recompute-and-reschedule))))

(defvar arche--mode-line-idle-timer
  (run-with-idle-timer
   arche-mode-line-update-delay 'repeat
   #'arche--mode-line-recompute-and-reschedule)
  "Timer that recomputes information for the mode line, or nil.
This runs once each time Emacs is idle.
Future recomputations are scheduled under
`arche--mode-line-repeat-timer'. See also
`arche--mode-line-recompute-and-reschedule' and
`arche--mode-line-recompute-project-and-branch'.")

(defvar arche--mode-line-repeat-timer nil
  "Timer that recomputes information for the mode line, or nil.
This is scheduled repeatedly at intervals after
`arche--mode-line-idle-timer' runs once. See also
`arche--mode-line-recompute-and-reschedule' and
`arche--mode-line-recompute-project-and-branch'.")

;; Make `mode-line-position' show the column, not just the row.
(column-number-mode +1)

(define-minor-mode arche-show-git-mode
  "Minor mode for showing Git status in mode line.

If enabled, then both the current Projectile project and the
current Git branch are shown in the mode line. Otherwise, only
the former is shown.")

(define-globalized-minor-mode arche-show-git-global-mode
  arche-show-git-mode arche-show-git-mode)

(arche-show-git-global-mode +1)

;; https://emacs.stackexchange.com/a/7542/12534
(defun arche-mode-line-align (left right)
  "Render a left/right aligned string for the mode line.
LEFT and RIGHT are strings, and the return value is a string that
displays them left- and right-aligned respectively, separated by
spaces."
  (let ((width (- (window-total-width) (length left))))
    (format (format "%%s%%%ds" width) left right)))

(defcustom arche-mode-line-left
  '(;; Show current input method.
    (:eval (arche-mode-line-input-method))
    ;; Show buffer coding system.
    (:eval (arche-mode-line-buffer-coding-system))
    " "
    ;; Show the name of the current buffer.
    mode-line-buffer-identification
    ;; Show [*] if the buffer is modified.
    (:eval (arche-mode-line-buffer-modified-status))
    "  "
    ;; Show the row and column of point.
    mode-line-position
    ;; Show the current Projectile project and Git branch.
    arche-mode-line-project-and-branch
    ;; Show the active major and minor modes.
    "  "
    mode-line-modes)
  "Composite mode line construct to be shown left-aligned."
  :type 'sexp)

(defcustom arche-mode-line-right
  '(;; Show eyebrowse window conf list and current one.
    (:eval ((lambda ()
              (when (featurep 'eyebrowse)
                (eyebrowse-mode-line-indicator))))))
  "Composite mode line construct to be shown right-aligned."
  :type 'sexp)

;; Actually reset the mode line format to show all the things we just
;; defined.
(setq-default mode-line-format
              '(:eval (replace-regexp-in-string
                       "%" "%%"
                       (arche-mode-line-align
                        (format-mode-line arche-mode-line-left)
                        (format-mode-line arche-mode-line-right))
                       'fixedcase 'literal)))

;;;; Highlighting

;; Package `col-highlight' highlights the current column.
(use-package col-highlight
  :config

  (arche-bind-key "c" #'column-highlight-mode))

;; Package `dimmer' makes the buffer window on focus more visible by
;; dimming the faces in the other buffers.
(use-package dimmer
  :straight (;; Use gonewest818's fork, which improves feature to
             ;; exclude buffers from dimming.
             :host github :repo "gonewest818/dimmer.el"
                   :fork (:repo "cmccloud/dimmer.el" :branch "feature/improvements"))

  :demand t
  :config/el-patch

  (defun dimmer-config-change-hook ()
    "Process all buffers if window configuration has changed."
    (dimmer--dbg "dimmer-config-change-hook")
    (el-patch-add (dimmer-restore-all))
    (unless (bound-and-true-p dimmer-timer)
      (setq dimmer-timer (run-at-time nil nil #'dimmer-process-all))
      ;; Queue up a sanity-check in case something forces a window change on us
      ;; This is useful mainly trying to keep up with other asychronous processes
      ;; - like those used in magit, for example, which often call `select-window'
      ;; sometime after changing the window configuration.
      (run-at-time 0.2 nil #'dimmer-command-hook)))

  :config

  ;; Make the other buffers a little bit dimmer than default. Default
  ;; value is 0.2.
  (setq dimmer-fraction 0.3)

  ;; Some buffers should be never dimmed.
  (setq dimmer-exclusion-predicates '(window-minibuffer-p))
  (setq dimmer-exclusion-regexp-list
        '("^\\*Minibuf-[0-9]+\\*" "^.\\*which-key\\*$"
          "^*Messages*" "*LV*"))

  (dimmer-mode +1)

  :blackout t)

;;;; Color theme

;; Package `zerodark-theme' provides a good-looking color theme that
;; works in both windowed and tty Emacs.
(straight-register-package
 '(zerodark-theme :host github :repo "NicolasPetton/zerodark-theme"))
(use-package zerodark-theme)

;; Prune the build cache for straight.el; this will prevent it from
;; growing too large. Do this after the final hook to prevent packages
;; installed there from being pruned.
(straight-prune-build-cache)

;; Occasionally prune the build directory as well. For similar reasons
;; as above, we need to do this after local configuration.
(when (= 0 (random 100))
  (straight-prune-build-directory))

;; Enable color theme as late as is humanly possible. This reduces
;; frame flashing and other artifacts during startup.
(use-feature zerodark-theme
  :demand t
  :config

  (load-theme 'zerodark 'no-confirm))

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; End:
