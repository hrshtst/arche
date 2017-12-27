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

;; save mini-buffer history
(savehist-mode 1)

;; save cursor place of visited file
(save-place-mode +1)

;; show current function in mini-buffer
(which-function-mode +1)
(setq-default which-func-unknown "")

;; set fill column
(setq-default fill-column 68)

;; clean display of mode line
(defvar mode-line-cleaner-alist
  '( ;; For minor-mode, first char is 'space'
    (yas-minor-mode . " Ys")
    (paredit-mode . " Pe")
    (eldoc-mode . "")
    (abbrev-mode . "")
    (undo-tree-mode . " Ut")
    (elisp-slime-nav-mode . " EN")
    (helm-gtags-mode . " HG")
    (flymake-mode . " Fm")
    (flycheck-mode . " Fc")
    (helm-mode . "")
    (company-mode . " C")
    ;; Major modes
    (lisp-interaction-mode . "Li")
    (python-mode . "Py")
    (ruby-mode   . "Rb")
    (emacs-lisp-mode . "El")
    (markdown-mode . "Md")))

(defun clean-mode-line ()
  (interactive)
  (loop for (mode . mode-str) in mode-line-cleaner-alist
        do
        (let ((old-mode-str (cdr (assq mode minor-mode-alist))))
          (when old-mode-str
            (setcar old-mode-str mode-str))
          ;; major mode
          (when (eq mode major-mode)
            (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;; show paren mode
(show-paren-mode t)
(custom-set-variables
 '(show-paren-style 'mixed)
 '(show-paren-delay 0) ; default: 0.15
)

;; delete selection mode
(delete-selection-mode 1)

;; smartrep
(require 'smartrep)

;; remove text properties when yanking
(setq-default yank-excluded-properties t)

;; show buffer or file name on title bar
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))
        (:eval (if (buffer-modified-p)
                   " •"))
        " - Emacs@" system-name))

;; for debug
;; (setq debug-on-error t)
(add-to-list 'minor-mode-alist '(debug-on-error " (^^)"))
;; (setq eval-expression-print-length nil)
;; (setq eval-expression-print-level nil)
