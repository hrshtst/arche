(require 'cl-lib)

;; encoding
(set-language-environment 'Japanese)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(setq default-process-coding-system '(undecided-dos . utf-8-unix))
(set-clipboard-coding-system 'utf-8-unix)

;; environment-dependent characters
(set-charset-priority 'ascii 'japanese-jisx0208 'latin-jisx0201
                      'katakana-jisx0201 'iso-8859-1 'cp1252 'unicode)
(set-coding-system-priority 'utf-8 'euc-jp 'iso-2022-jp 'cp932)

;; basic customize variables
(custom-set-variables
 '(package-enable-at-startup nil)
 '(inhibit-startup-screen t)
 '(read-file-name-completion-ignore-case t)
 '(line-move-visual t)
 '(set-mark-command-repeat-pop t)
 '(find-file-visit-truename t)
 '(comment-style 'multi-line)
 '(imenu-auto-rescan t)
 '(delete-auto-save-files t)
 '(create-lockfiles nil)
 '(backup-directory-alist `((".*" . ,temporary-file-directory)))
 '(auto-save-file-name-transforms `((".*" ,temporary-file-directory t))))

;; coloring
(global-font-lock-mode +1)
(require 'generic-x)

;; cursor
(blink-cursor-mode t)

;; disable default scroll bar and tool bar
(when window-system
  (set-scroll-bar-mode 'nil)
  (tool-bar-mode 0))

;; not create backup file and not create auto save file
(setq-default backup-inhibited t)

;; wrap lines
(setq-default truncate-lines nil)
(setq-default truncate-partial-width-windows nil)

;; disable menu bar
(menu-bar-mode -1)

;; not beep
(setq-default ring-bell-function 'ignore)

;; display line information
(line-number-mode t)
(column-number-mode t)

;; yes-or-no-p
(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default use-dialog-box nil)

;; run server
(require 'server)
(unless (server-running-p)
  (server-start))

;; show in which function you are (only available in C)
(which-function-mode +1)
(setq-default which-func-unknown "")

;; show paren mode
(show-paren-mode t)
(setq show-paren-style 'mixed)
(setq show-paren-delay 0) ; default: 0.15

;; delete section mode
(delete-selection-mode 1)

;; global auto revert mode
(global-auto-revert-mode 1)

;; remove text properties when yanking
(setq yank-excluded-properties t)

;; smart repetition
(require 'smartrep)

;; for debug
;; (setq debug-on-error t)
(add-to-list 'minor-mode-alist '(debug-on-error " (^^)"))
;; (setq eval-expression-print-length nil)
;; (setq eval-expression-print-level nil)
