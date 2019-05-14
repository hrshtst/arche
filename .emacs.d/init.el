;;; init.el --- My personal configuration file for Emacs -*- lexical-binding: t -*-

;; Copyright (c) 2016-2019 Hiroshi Atsuta

;; Author: Hiroshi Atsuta <atsuta.hiroshi@gmail.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This is just a configuration file for Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;;; Load utility libraries

(require 'cl-lib)
(require 'map)

;;; Define utility functions
(defmacro my/defadvice (name arglist where place docstring &rest body)
  "Define an advice called NAME and add it to a function.
ARGLIST is as in `defun'. WHERE is a keyword as passed to
`advice-add', and PLACE is the function to which to add the
advice, like in `advice-add'. DOCSTRING and BODY are as in
`defun'."
  (declare (indent 2)
           (doc-string 5))
  (unless (stringp docstring)
    (error "my/defadvice: no docstring provided"))
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

(defmacro my/defhook (name arglist hook docstring &rest body)
  "Define a function called NAME and add it to a hook.
ARGLIST is as in `defun'. HOOK is the hook to which to add the
function. DOCSTRING and BODY are as in `defun'."
  (declare (indent 2)
           (doc-string 4))
  (unless (string-match-p "-hook$" (symbol-name hook))
    (error "Symbol `%S' is not a hook" hook))
  (unless (stringp docstring)
    (error "my/defhook: no docstring provided"))
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

;;;; el-patch

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

;;;; bind-key

;; Package `bind-key' provides a useful macro which is much prettier
;; and surely takes effect.
(use-package bind-key)

(defvar my/keymap (make-sparse-keymap)
  "Keymap for my own commands is bound under M-g.")

(bind-key* "M-g" my/keymap)

(defmacro my/bind-key (key-name command &optional predicate)
  "Bind a key in `my/keymap'."
  `(bind-key ,key-name ,command my/keymap ,predicate))

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

;;; Candidate selection

;;;; ivy
(use-package ivy
  :init
  (ivy-mode +1)
  :bind* (("C-c C-r" . ivy-resume))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-extra-directories nil)
  (setq enable-recursive-minibuffers t)
  :blackout t)

;;;; ivy-hydra
(use-package ivy-hydra)

;;;; counsel
(use-package counsel
  :init
  (counsel-mode +1)
  :bind (("C-c c" . counsel-find-file)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-git-ag)
         ("C-c m" . counsel-mark-ring))
  :blackout t)

;;; Window management

;; Feature `windmove' allows us to move between windows by S-left,
;; S-right, S-up, and S-down.
(use-feature windmove
  :demand t
  :config
  (windmove-default-keybindings))

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

;;; Finding files

;; Follow symlinks when opening files.
(setq find-file-visit-truename t)
;; Suppress warning "X and Y are the same file".
(setq find-file-suppress-same-file-warnings t)

;; Feature `saveplace' remembers place in files so as to move point
;; to saved position automatically when visiting them later.
(use-feature saveplace
  :demand t
  :config
  (save-place-mode +1))

;; Package `projectile' provides a set of functions to operate on
;; project level such as Git repository, CMake project, etc.
;; (use-package projectile
;;   :init
;;   (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;;   (projectile-mode +1)
;;   :defer 1
;;   :config
;;   :blackout t)

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

  (setq counsel-projectile-sort-files t))


;;; Editing

;;;; Text formatting
;; When region is active, capitalize it.
(bind-key "M-c" #'capitalize-dwim)
(bind-key "M-l" #'downcase-dwim)
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

(defun my/auto-fill-disable ()
  "Disable `auto-fill-mode' in the current buffer."
  (auto-fill-mode -1))

(define-minor-mode my/fix-whitespace-mode
  "Minor mode to automatically fix whitespace on save.
If enabled, then saving the buffer deletes all trailing
whitespace and ensures that the file ends with exactly one
newline."
  nil nil nil
  (if my/fix-whitespace-mode
      (progn
        (setq require-final-newline t)
        (add-hook 'before-save-hook #'delete-trailing-whitespace nil 'local))
    (setq require-final-newline nil)
    (remove-hook 'before-save-hook #'delete-trailing-whitespace 'local)))

(define-globalized-minor-mode my/fix-whitespace-global-mode
  my/fix-whitespace-mode my/fix-whitespace-mode)

(my/fix-whitespace-global-mode +1)

(put 'my/fix-whitespace-mode 'safe-local-variable #'booleanp)

;; Feature `whitespace' provides a minor mode for highlighting
;; whitespace.
(use-feature whitespace
  :init
  (define-minor-mode my/highlight-long-lines-mode
    "Minor mode for highlighting long lines."
    nil nil nil
    (if my/highlight-long-lines-mode
        (progn
          (setq-local whitespace-style '(face lines-tail))
          (setq-local whitespace-line-column 79)
          (whitespace-mode +1))
      (whitespace-mode -1)
      (kill-local-variable 'whitespace-style)
      (kill-local-variable 'whitespace-line-column)))
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

;; Feature `delsel' allows us to delete selection.
(use-feature delsel
  :demand t
  :config
  (delete-selection-mode +1))

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
(defun my/exchange-point-and-mark ()
  "Disable highlight after `exchange-point-and-mark'."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark))

(bind-key "C-x C-x" 'my/exchange-point-and-mark)

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
  :demand t

  :config
  ;; Load the default pair definitions.
  (require 'smartparens-config)

  ;; Enable Smartparens in all buffers.
  (smartparens-global-mode +1)

  ;; Set up keybindings for Smartparens functions with keybindings
  ;; of corresponding paredit functions.
  (sp-use-paredit-bindings)

  ;; Highlight matching parens.
  (show-smartparens-global-mode +1)

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
  :hook
  (prog-mode . yas-minor-mode)

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
  (my/defhook my/enable-lsp ()
     prog-mode-hook
     "Enable `lsp-mode' for most programming modes."
     (unless (derived-mode-p
              ;; `lsp-mode' doesn't support Elisp. There's nothing to
              ;; do for the *scratch* buffer.
              #'emacs-lisp-mode
              ;; Disable for modes that is a specialized framework
              ;; available for
              #'python-mode)
       (lsp)))

  :config
  ;; Use Flycheck, not Flymake.
  (setq lsp-prefer-flymake nil))

;;;; Indentation

;; Don't use tabs for indentation.
(setq-default indent-tabs-mode nil)

;;;; Autocompletion

;; Package `company' provides a text completion framework.  It uses
;;  pluggable back-ends and front-ends to retrieve and display
;;  completion candidates.
(use-package company
  :defer 3
  :init
  (defvar my/company-backends-global
    '(company-capf
      company-files
      (company-dabbrev-code company-keywords)
      company-dabbrev)
    "Values for `company-backends' used everywhere. If
`company-backends' is overridden, then these backends will still
be included.")

  :bind (;; Remap the standard Emacs keybindings for invoking
         ;; completion to instead use Company. You might think this
         ;; could be put in the `:bind*' declaration below, but it
         ;; seems that `bind-key*' does not work with remappings.
         ([remap completion-at-point] . company-manual-begin)
         ([remap complete-symbol] . company-manual-begin)

         ;; The following are keybindings that take effect whenever
         ;; the completions menu is visible, even if the user has not
         ;; explicitly interacted with Company.

         :map company-active-map

         ;; Make TAB always complete the current selection, instead of
         ;; only completing a common prefix.
         ("<tab>" . company-complete-selection)
         ("TAB" . company-complete-selection)

         ;; The following are keybindings that only take effect if the
         ;; user has explicitly interacted with Company. Note that
         ;; `:map' from above is "sticky", and applies also below: see
         ;; https://github.com/jwiegley/use-package/issues/334#issuecomment-349473819.

         :filter (company-explicit-action-p)

         ;; Make RET trigger a completion if and only if the user has
         ;; explicitly interacted with Company, instead of always
         ;; doing so.
         ("<return>" . company-complete-selection)
         ("RET" . company-complete-selection)

         ;; We then make <up> and <down> abort the completions menu
         ;; unless the user has interacted explicitly. Note that we
         ;; use `company-select-previous' instead of
         ;; `company-select-previous-or-abort'. I think the former
         ;; makes more sense since the general idea of this `company'
         ;; configuration is to decide whether or not to steal
         ;; keypresses based on whether the user has explicitly
         ;; interacted with `company', not based on the number of
         ;; candidates.
         ;;
         ;; Note that M-p and M-n work regardless of whether explicit
         ;; interaction has happened yet, and note also that M-TAB
         ;; when the completions menu is open counts as an
         ;; interaction.
         ("<up>" . company-select-previous)
         ("<down>" . company-select-next))

  :bind* (;; The default keybinding for `completion-at-point' and
          ;; `complete-symbol' is M-TAB or equivalently C-M-i. We
          ;; already remapped those bindings to `company-manual-begin'
          ;; above. Here we make sure that they definitely invoke
          ;; `company-manual-begin' even if a minor mode binds M-TAB
          ;; directly.
          ("M-TAB" . company-manual-begin))

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

  (global-company-mode +1)

  :blackout t)

;; Package `company-lsp' provides a Company completion backend for
;; `lsp-mode'.
(use-package company-lsp)

;;;; Jump to definition

;; Package `dumb-jump' is an Emacs "jump to definition" package with
;; support for multiple programming languages that favors "just
;; working". This means minimal configuration with absolutely no
;; stored indexes (TAGS) or persistent background processes.
(use-package dumb-jump
  :init
  (dumb-jump-mode +1)

  :bind (:map dumb-jump-mode-map
              ("M-Q" . dumb-jump-quick-look))
  :bind* (("C-M-d" . dumb-jump-go-prompt)
          ("C-x 4 g" . dumb-jump-go-other-window)
          ("C-x 4 d" . my/dumb-jump-go-prompt-other-window))

  :config
  (defun my/dumb-jump-go-prompt-other-window ()
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
  (my/defadvice my/advice-disable-eldoc-on-flycheck
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
  (defun my/flycheck-disable-checkers (&rest checkers)
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

  (dolist (name '("python" "python2" "python3"))
    (add-to-list 'safe-local-variable-values
                 `(flycheck-python-pycompile-executable . ,name)))

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

  :config
  (my/bind-key "p" #'flycheck-previous-error)
  (my/bind-key "n" #'flycheck-next-error)

  :blackout t)

;; Package `lsp-ui' provides Flycheck integration for `lsp-mode', as
;; well as various other UI elements that integrate with `lsp-mode'.
(use-package lsp-ui
  :bind (("C-c f" . lsp-ui-sideline-apply-code-actions))

  :config
  ;; Don't show symbol definitions in the sideline. They are pretty
  ;; noisy, and there appears to currently be a bug where they prevent
  ;; Flycheck errors from being shown (the errors flash briefly and
  ;; then disappear).
  (setq lsp-ui-sideline-show-hover nil))

;;; Language support
;;;; Plain text

;; Feature `text-mode' provides a major mode for editing plain text.
(use-feature text-mode
  :config
  (add-hook 'text-mode-hook #'auto-fill-mode)

  (my/defhook my/flycheck-text-setup ()
    text-mode-hook
    "Disable some Flycheck checkers for plain text."
    (my/flycheck-disable-checkers 'proselint)))

;;;; Lisp languages

;; Feature `lisp-mode' provides a base major mode for Lisp languages,
;; and supporting functions for dealing with Lisp code.
(use-feature lisp-mode
  :init
  (add-to-list 'safe-local-variable-values
               '(lisp-indent-function . common-lisp-indent-function)))

;;; init.el ends here
