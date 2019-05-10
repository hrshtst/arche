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
  ;; Reduce verbosity. Suppress messages about successful snippet
  ;; loading on Emacs init. Errors should still be shown.
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

;;; init.el ends here
