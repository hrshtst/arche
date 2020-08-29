;; -*- lexical-binding: t -*-

;; Copyright (c) 2020 Hiroshi Atsuta

;; Author: Hiroshi Atsuta <atsuta@ieee.org>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This is my personal configuration for Emacs. The word "arche",
;; which is an ancient Greek word with senses "beginning", "origin" or
;; "source of action", is used for namespace partitioning. I borrowed
;; some snippets of code, just substituting the prefix "radian-" with
;; "arche-", from the implementation originally written by Radon
;; Rosborough [https://github.com/raxod502/radian/emacs/radian.el]. I
;; learned a lot from his elegant implementation and meticulous
;; comments about the pratical way of Emacs-Lisp, and I would like to
;; express my thanks to him.

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;; Detect stale bytecode

;; If Emacs version changed, the bytecode is no longer valid and we
;; must recompile. Also, if the location of the configuration files
;; chagend, our dotfile-finding functions are defined incorrectly and
;; we must recompile.
(eval
 `(unless (equal
           (list
            (emacs-version)
            arche-lib-file)
           ',(eval-when-compile
               (list
                (emacs-version)
                arche-lib-file)))
    (throw 'stale-bytecode nil)))

;;; Load built-in utility libraries

(require 'cl-lib)
(require 'map)
(require 'subr-x)

;;; Define customization groups

(defgroup arche-hooks nil
  "Startup hooks for our Emacs configuration."
  :group 'arche
  :link '(url-link :tag "GitHub" "https://github.com/atsut97/dotfiles"))

(defgroup arche nil
  "Customize our Emacs configuration."
  :prefix "arche-"
  :group 'emacs
  :link '(url-link :tag "GitHub" "https://github.com/atsut97/dotfiles"))

;;; Define utility functions and variables

(defvar arche-directory (file-name-directory
                         (directory-file-name
                          (file-name-directory
                           arche-lib-file)))
  "Path to the Git repository containing this configuration.")

(defmacro arche-protect-macros (&rest body)
  "Eval BODY, protecting macros from incorrect expansion.
This macro should be used in the following situation:

Some form is being evaluated, and this form contains as a
sub-form some code that will not be evaluated immediately, but
will be evaluated later. The code uses a macro that is not
defined at the time the top-level form is evaluated, but will be
defined by time the sub-form's code is evaluated. This macro
handles its arguments in some way other than evaluating them
directly. And finally, one of the arguments of this macro could
be interpreted itself as a macro invocation, and expanding the
invocation would break the evaluation of the outer macro.

You might think this situation is such an edge case that it would
never happen, but you'd be wrong, unfortunately. In such a
situation, you must wrap at least the outer macro in this form,
but can wrap at any higher level up to the top-level form."
  (declare (indent 0))
  `(eval '(progn ,@body)))

(defmacro arche-flet (bindings &rest body)
  "Temporarily override function definitions using `cl-letf*'.
BINDINGS are composed of `defun'-ish forms. NAME is the function
to override. It has access to the original function as a
lexically bound variable by the same name, for use with
`funcall'. ARGLIST and BODY are as in `defun'.

\(fn ((defun NAME ARGLIST &rest BODY) ...) BODY...)"
  (declare (indent defun))
  `(cl-letf* (,@(cl-mapcan
                 (lambda (binding)
                   (when (memq (car binding) '(defun lambda))
                     (setq binding (cdr binding)))
                   (cl-destructuring-bind (name arglist &rest body) binding
                     (list
                      `(,name (symbol-function #',name))
                      `((symbol-function #',name)
                        (lambda ,arglist
                          ,@body)))))
                 bindings))
     ,@body))

(defmacro arche-defadvice (name arglist where place docstring &rest body)
  "Define an advice called NAME and add it to a function.
ARGLIST is as in `defun'. WHERE is a keyword as passed to
`advice-add', and PLACE is the function to which to add the
advice, like in `advice-add'. PLACE should be sharp-quoted.
DOCSTRING and BODY are as in `defun'."
  (declare (indent 2)
           (doc-string 5))
  (unless (stringp docstring)
    (error "arche: advice `%S' not documented'" name))
  (unless (and (listp place)
               (= 2 (length place))
               (eq (nth 0 place) 'function)
               (symbolp (nth 1 place)))
    (error "arche: advice `%S' does not sharp-quote place `%S'" name place))
  `(progn
     ;; You'd think I would put an `eval-and-compile' around this. It
     ;; turns out that doing so breaks the ability of
     ;; `elisp-completion-at-point' to complete on function arguments
     ;; to the advice. I know, right? Apparently this is because the
     ;; code that gets the list of lexically bound symbols at point
     ;; tries to `macroexpand-all', and apparently macroexpanding
     ;; `eval-and-compile' goes ahead and evals the thing and returns
     ;; only the function symbol. No good. But the compiler does still
     ;; want to know the function is defined (this is a Gilardi
     ;; scenario), so we pacify it by `eval-when-compile'ing something
     ;; similar (see below).
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
     (eval-when-compile
       (declare-function ,name nil))
     (advice-add ,place ',where #',name)
     ',name))

(defmacro arche-defhook (name arglist hooks docstring &rest body)
  "Define a function called NAME and add it to a hook.
ARGLIST is as in `defun'. HOOKS is a list of hooks to which to
add the function, or just a single hook. DOCSTRING and BODY are
as in `defun'."
  (declare (indent 2)
           (doc-string 4))
  (unless (listp hooks)
    (setq hooks (list hooks)))
  (dolist (hook hooks)
    (unless (string-match-p "-\\(hook\\|functions\\)$" (symbol-name hook))
      (error "Symbol `%S' is not a hook" hook)))
  (unless (stringp docstring)
    (error "arche: no docstring provided for `arche-defhook'"))
  (let ((hooks-str (format "`%S'" (car hooks))))
    (dolist (hook (cdr hooks))
      (setq hooks-str (format "%s\nand `%S'" hooks-str hook)))
    `(progn
       (defun ,name ,arglist
         ,(format "%s\n\nThis function is for use in %s."
                  docstring hooks-str)
         ,@body)
       (dolist (hook ',hooks)
         (add-hook hook ',name)))))

(defmacro arche-operating-system-p (os)
  "Return non-nil if OS corresponds to the current operating system.
Allowable values for OS (not quoted) are `macOS', `osx',
`windows', `linux', `unix'."
  (pcase os
    (`unix `(not (memq system-type '(ms-dos windows-nt cygwin))))
    ((or `macOS `osx) `(eq system-type 'darwin))
    (`linux `(not (memq system-type
                        '(darwin ms-dos windows-nt cygwin))))
    (`windows `(memq system-type '(ms-dos windows-nt cygwin)))))

(defmacro arche-with-operating-system (os &rest body)
  "If OS corresponds to the current operating system, eval and return BODY.
If not, return nil.

Allowable values for OS (not quoted) are `macOS', `osx',
`windows', `linux', `unix'."
  (declare (indent 1))
  `(when (arche-operating-system-p ,os)
     ,@body))

(defmacro arche-if-compiletime (cond then else)
  "Like `if', but COND is evaluated at compile time.
The macro expands directly to either THEN or ELSE, and the other
branch is not compiled. This can be helpful to deal with code
that uses functions only defined in a specific Emacs version."
  (declare (indent 2))
  (if (eval cond)
      then
    else))

(defmacro arche-when-compiletime (cond &rest body)
  "Like `when', but COND is evaluated at compile time.
BODY is only compiled if COND evaluates to non-nil. This can be
helpful to deal with code that uses functions only defined in a
specific Emacs version."
  (declare (indent 1))
  (when (eval cond)
    `(progn ,@body)))

(defun arche-managed-p (filename)
  "Return non-nil if FILENAME is managed by arche.
This means that FILENAME is a symlink whose target is inside
`arche-directory'."
  (let ((truename (file-truename filename)))
    (string-prefix-p arche-directory truename
                     (when (if (fboundp 'file-name-case-insensitive-p)
                               (file-name-case-insensitive-p truename)
                             (arche-with-operating-system macOS
                               t))
                       'ignore-case))))

(defmacro arche--with-silent-load (&rest body)
  "Execute BODY, with the function `load' made silent."
  (declare (indent 0))
  `(arche-flet ((defun load (file &optional noerror _nomessage &rest args)
                   (apply load file noerror 'nomessage args)))
     ,@body))

(defmacro arche--with-silent-write (&rest body)
  "Execute BODY, with the function `write-region' made silent."
  (declare (indent 0))
  `(arche-flet ((defun write-region
                     (start end filename &optional append visit lockname
                            mustbenew)
                   (funcall write-region start end filename append 0
                            lockname mustbenew)
                   (when (or (stringp visit) (eq visit t))
                     (setq buffer-file-name
                           (if (stringp visit)
                               visit
                             filename))
                     (set-visited-file-modtime)
                     (set-buffer-modified-p nil))))
     (cl-letf (((symbol-function #'message) #'ignore))
       ,@body)))

(defmacro arche--with-silent-message (regexps &rest body)
  "Silencing any messages that match REGEXPS, execute BODY.
REGEXPS is a list of strings; if `message' would display a
message string (not including the trailing newline) matching any
element of REGEXPS, nothing happens. The REGEXPS need not match
the entire message; include ^ and $ if necessary. REGEXPS may
also be a single string."
  (declare (indent 1))
  (let ((regexps-sym (cl-gensym "regexps")))
    `(let ((,regexps-sym ,regexps))
       (when (stringp ,regexps-sym)
         (setq ,regexps-sym (list ,regexps-sym)))
       (arche-flet ((defun message (format &rest args)
                       (let ((str (apply #'format format args)))
                         ;; Can't use an unnamed block because during
                         ;; byte-compilation, some idiot loads `cl', which
                         ;; sticks an advice onto `dolist' that makes it
                         ;; behave like `cl-dolist' (i.e., wrap it in
                         ;; another unnamed block) and therefore breaks
                         ;; this code.
                         (cl-block done
                           (dolist (regexp ,regexps-sym)
                             (when (or (null regexp)
                                       (string-match-p regexp str))
                               (cl-return-from done)))
                           (funcall message "%s" str)))))
         ,@body))))

(defun arche--advice-silence-messages (func &rest args)
  "Invoke FUNC with ARGS, silencing all messages.
This is an `:around' advice for many different functions."
  (cl-letf (((symbol-function #'message) #'ignore))
    (apply func args)))

(defun arche--random-string ()
  "Return a random string designed to be globally unique."
  (md5 (format "%s%s%s%s"
               (system-name) (emacs-pid) (current-time) (random))))

(defun arche--list-of-strings-p (obj)
  "Return non-nil if OBJ is a list of strings."
  (and (listp obj)
       (cl-every #'stringp obj)))

(defun arche--path-join (path &rest segments)
  "Join PATH with SEGMENTS using `expand-file-name'.
First `expand-file-name' is called on the first member of
SEGMENTS, with PATH as DEFAULT-DIRECTORY. Then `expand-file-name'
is called on the second member, with the result of the first call
as DEFAULT-DIRECTORY, and so on. If no SEGMENTS are passed, the
return value is just PATH."
  (while segments
    (setq path (expand-file-name (pop segments) path)))
  path)

;;; Define hooks and load local configuration

;; Reset the value of this variable so that stale functions don't
;; stick around.
(setq arche--finalize-init-hook nil)

(defcustom arche-before-straight-hook nil
  "Hook run just before arche bootstraps straight.el.
For use with `arche-local-on-hook' in init.local.el."
  :group 'arche-hooks
  :type 'hook)

(defcustom arche-after-init-hook nil
  "Hook run after at the very end of init.
For use with `arche-local-on-hook' in init.local.el."
  :group 'arche-hooks
  :type 'hook)

(defvar arche--hook-contents nil
  "Alist mapping local init hooks to lists of forms.
This is used to embed local init hook code directly into the
init-file at the appropriate places during byte-compilation,
without breaking macro-expansion.")

;; Idempotency.
(setq arche--hook-contents nil)

;; Allow binding this variable dynamically before straight.el has been
;; loaded.
(defvar straight-current-profile)

(defmacro arche--load-local-init-file ()
  "Load local init-file, with crazy hacks for byte-compilation.
In particular, if we are byte-compiling, actually macroexpand to
the entire contents of the local init-file, except that the
bodies of invocations to `arche-local-on-hook' are recorded in
`arche--hook-contents'. Otherwise just load the file like
usual."
  (if byte-compile-current-file
      (let ((forms nil))
        (with-temp-buffer
          (ignore-errors
            ;; Can't do this literally because it breaks Unicode
            ;; characters.
            (insert-file-contents arche-local-init-file))
          (condition-case _
              (while t
                (let ((form (read (current-buffer))))
                  (if (and (listp form)
                           (eq (nth 0 form) #'arche-local-on-hook)
                           (nth 1 form)
                           (symbolp (nth 1 form))
                           (nthcdr 2 form))
                      (let* ((name (nth 1 form))
                             (body (nthcdr 2 form))
                             (hook (intern (format "arche-%S-hook" name)))
                             (link (assq hook arche--hook-contents)))
                        (unless link
                          (setq link (cons hook nil))
                          (push link arche--hook-contents))
                        (dolist (subform body)
                          (push subform (cdr link))))
                    (push form forms))))
            (end-of-file)))
        (setq forms (nreverse forms))
        (dolist (link arche--hook-contents)
          (setf (cdr link)
                (nreverse (cdr link))))
        `(progn ,@forms))
    `(load arche-local-init-file 'noerror 'nomessage)))

(defmacro arche-local-on-hook (name &rest body)
  "Register some code to be run on one of arche's hooks.
The hook to be used is `arche-NAME-hook', with NAME an unquoted
symbol, and the code which is added is BODY wrapped in a `progn'.
See \\[customize-group] RET arche-hooks RET for a list of hooks
which you can use with this macro in your local init-file.

Using this macro instead of defining functions and adding them to
arche's hooks manually means that a lot of magic happens which
allows arche to embed your entire local init-file into arche
during byte-compilation without breaking macroexpansion in
unexpected ways."
  (declare (indent 1))
  (let ((func-name (intern (format "arche-local--%S" name)))
        (hook (intern (format "arche-%S-hook" name))))
    `(progn
       (arche-defhook ,func-name ()
         ,hook
         "Automatically-generated local hook function."
         (arche-protect-macros
           ,@body)))))

(defmacro arche--run-hook (name)
  "Run the given local init HOOK.
The hook to be used is `arche-NAME-hook', with NAME an unquoted
symbol. This binds `straight-current-profile', and also has some
gnarly hacks to allow arche to embed the entire contents of the
hook directly into the init-file during byte-compilation."
  (declare (indent 0))
  (let ((hook (intern (format "arche-%S-hook" name))))
    `(let ((straight-current-profile 'arche-local))
       (run-hooks ',hook)
       ,@(when byte-compile-current-file
           (alist-get hook arche--hook-contents)))))

;; Allow to disable local customizations with a
;; command-line argument.
(if (member "--no-local" command-line-args)

    ;; Make sure to delete --no-local from the list, because
    ;; otherwise Emacs will issue a warning about the unknown
    ;; argument.
    (setq command-line-args
          (delete "--no-local" command-line-args))

  ;; Load local customizations.
  (arche--load-local-init-file))

;;; Startup optimizations

;; Disable frequency of GC. This helps performance both during init
;; and after init. Value is in bytes so this is 100MB, as suggested in
;; <https://github.com/emacs-lsp/lsp-mode#performance>.
(setq gc-cons-threshold (* 100 1024 1024))

;; After we enabled `load-prefer-newer' in init.el, disable it again
;; for the duration of init. Presumably, it slows things down, and we
;; shouldn't need it for anything but loading arche.el itself.
(setq load-prefer-newer nil)

;;; Networking

;; Use `with-eval-after-load' instead of `use-feature' because we have
;; not yet set up package management.

;; Feature `gnutls' provides support for SSL/TLS connections, using
;; the GnuTLS library.
(with-eval-after-load 'gnutls

  ;; `use-package' does this for us normally.
  (eval-when-compile
    (require 'gnutls))

  ;; Do not allow insecure TLS connections.
  (setq gnutls-verify-error t)

  ;; Bump the required security level for TLS to an acceptably modern
  ;; value.
  (setq gnutls-min-prime-bits 3072))

;; Feature `url-http' is a library for making HTTP requests.
(with-eval-after-load 'url-http

  (eval-when-compile
    (require 'url-http))

  (arche-defadvice arche--no-query-on-http-kill
      (buffer)
    :filter-return #'url-http
    "Disable query-on-exit for all network connections.
This prevents Emacs shutdown from being interrupted just because
there is a pending network request."
    (prog1 buffer
      (set-process-query-on-exit-flag
       (get-buffer-process buffer) nil))))

;;; Set up package management
;;;; straight.el

;; Tell straight.el about the profiles we are going to be using.
(setq straight-profiles
      '(;; Packages registered in this file.
        (arche . "arche.el")
        ;; Packages registered in the local init-file during hooks.
        (arche-local . "arche-local.el")
        ;; Packages registered interactively.
        (nil . "default.el")))

;; Pretend to dynamically bind `straight-current-profile' to `arche'
;; over the init-file. We do this to avoid having straight.el
;; configuration mentioned in the top-level init-file.

(arche-defhook arche--reset-straight-current-profile ()
  arche--finalize-init-hook
  "Reset `straight-current-profile' to nil.
This function is used on `arche--finalize-init-hook' to emulate
binding the variable dynamically over the entire init-file."
  (setq straight-current-profile nil))

(setq straight-current-profile 'arche)

;; Use the develop branch of straight.el on develop branch.
(setq straight-repository-branch "develop")

;; If watchexec and Python are installed, use file watchers to detect
;; package modifications. This saves time at startup. Otherwise, use
;; the ever-reliable find(1).
(if (and (executable-find "watchexec")
         (executable-find "python3"))
    (setq straight-check-for-modifications '(watch-files find-when-checking))
  (setq straight-check-for-modifications
        '(find-at-startup find-when-checking)))

(setq straight-vc-git-default-clone-depth 1)

;; Clear out recipe overrides (in case of re-init).
(setq straight-recipe-overrides nil)

(arche--run-hook before-straight)

;; Bootstrap the package manager, straight.el.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
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

;; Package `use-package' provides a handy macro by the same name which
;; is essentially a wrapper around `with-eval-after-load' with a lot
;; of handy syntactic sugar and useful features.
(straight-use-package 'use-package)

;; When configuring a feature with `use-package', also tell
;; straight.el to install a package of the same name, unless otherwise
;; specified using the `:straight' keyword.
(setq straight-use-package-by-default t)

;; Tell `use-package' to always load features lazily unless told
;; otherwise. It's nicer to have this kind of thing be deterministic:
;; if `:demand' is present, the loading is eager; otherwise, the
;; loading is lazy. See
;; https://github.com/jwiegley/use-package#notes-about-lazy-loading.
(setq use-package-always-defer t)

(defmacro use-feature (name &rest args)
  "Like `use-package', but with `straight-use-package-by-default' disabled.
NAME and ARGS are as in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))

(defun arche--remove-sharp-quotes (form)
  "Remove sharp quotes in all sub-forms of FORM."
  (pcase form
    (`(function ,x) (arche--remove-sharp-quotes x))
    (`(,x . ,y) (cons (arche--remove-sharp-quotes x)
                      (arche--remove-sharp-quotes y)))
    ((pred vectorp)
     (apply #'vector (mapcar #'arche--remove-sharp-quotes form)))
    (x x)))

(arche-defadvice arche--advice-use-package-bind-handle-sharp-quotes
    (args)
  :filter-args #'use-package-normalize-binder
  "Make `use-package' handle sharp-quoted functions correctly in `:bind'.
It is unclear to me why this is needed, as JW said explicitly to
the contrary in
<https://github.com/jwiegley/use-package/issues/461#issuecomment-348045772>.
Nevertheless we hack around the issue by simply doing a recursive
find-and-replace on sharp quotes in the arguments, because that's
the simple solution and the performance overhead is unimportant
since it happens during compilation anyway. (No, I'm not willing
to give up my sharp quotes; having autocompletion is really
nice.)"
  (arche--remove-sharp-quotes args))

;; Package `blackout' provides a convenient function for customizing
;; mode lighters. It supports both major and minor modes with the same
;; interface, and includes `use-package' integration. The features are
;; a strict superset of those provided by similar packages `diminish',
;; `delight', and `dim'.
(use-package blackout
  :straight (:host github :repo "raxod502/blackout")
  :demand t)

;;;; straight.el configuration

;; Feature `straight-x' from package `straight' provides
;; experimental/unstable extensions to straight.el which are not yet
;; ready for official inclusion.
(use-feature straight-x
  ;; Add an autoload for this extremely useful command.
  :commands (straight-x-fetch-all))

;;; Configure ~/.emacs.d paths

;; Package `no-littering' changes the default paths for lots of
;; different packages, with the net result that the ~/.emacs.d folder
;; is much more clean and organized.
(use-package no-littering
  :demand t)

;;; Prevent Emacs-provided Org from being loaded

;; Our real configuration for Org comes much later. Doing this now
;; means that if any packages that are installed in the meantime
;; depend on Org, they will not accidentally cause the Emacs-provided
;; (outdated and duplicated) version of Org to be loaded before the
;; real one is registered.
;;
;; Use Radon's mirror of Org because the upstream has *shockingly*
;; atrocious uptime (namely, the entire service will just go down for
;; more than a day at a time on a regular basis). Unacceptable because
;; it keeps breaking CI.
(straight-use-package
 '(org :host github :repo "emacs-straight/org-mode" :local-repo "org"))

;;; el-patch

;; Package `el-patch' provides a way to override the definition of an
;; internal function from another package by providing an s-expression
;; based diff which can later be validated to ensure that the upstream
;; definition has not changed.
(use-package el-patch)

;; Only needed at compile time, thanks to Jon
;; <https://github.com/raxod502/el-patch/pull/11>.
(eval-when-compile
  (require 'el-patch))

;;; Keybindings

;; Package `bind-key' provides a macro by the same name (along with
;; `bind-key*' and `unbind-key') which provides a much prettier API
;; for manipulating keymaps than `define-key' and `global-set-key' do.
;; It's also the same API that `:bind' and similar keywords in
;; `use-package' use.
(use-package bind-key
  :demand t)

(defvar arche-keymap (make-sparse-keymap)
  "Keymap for our commands that should be put under a prefix.
This keymap is bound under \\[arche-keymap].")

(bind-key* "M-P" arche-keymap)

(defmacro arche-bind-key (key-name command &optional predicate)
  "Bind a key in `arche-keymap'.
KEY-NAME, COMMAND, and PREDICATE are as in `bind-key'."
  `(bind-key ,key-name ,command arche-keymap ,predicate))

(defun arche-join-keys (&rest keys)
  "Join key sequences KEYS. Empty strings and nils are discarded.
\(arche--join-keys \"\\[arche-keymap] e\" \"e i\")
  => \"\\[arche-keymap] e e i\"
\(arche--join-keys \"\\[arche-keymap]\" \"\" \"e i\")
  => \"\\[arche-keymap] e i\""
  (string-join (remove "" (mapcar #'string-trim (remove nil keys))) " "))

(arche-defadvice arche--quoted-insert-allow-quit (quoted-insert &rest args)
  :around #'quoted-insert
  "Allow quitting out of \\[quoted-insert] with \\[keyboard-quit]."
  (arche-flet ((defun insert-and-inherit (&rest args)
                  (dolist (arg args)
                    (when (equal arg ?\C-g)
                      (signal 'quit nil)))
                  (apply insert-and-inherit args)))
    (apply quoted-insert args)))

;; Package `which-key' displays the key bindings and associated
;; commands following the currently-entered key prefix in a popup.
(use-package which-key
  :demand t
  :config

  ;; We configure it so that `which-key' is triggered by typing C-h
  ;; during a key sequence (the usual way to show bindings). See
  ;; <https://github.com/justbur/emacs-which-key#manual-activation>.
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay most-positive-fixnum)
  (setq which-key-idle-secondary-delay 1e-100)

  (which-key-mode +1)

  :blackout t)

;;; Environment
;;;; Environment variables

(defvar arche--env-setup-p nil
  "Non-nil if `arche-env-setup' has completed at least once.")

(defun arche-env-setup (&optional again)
  "Load ~/.profile and set environment variables exported therein.
Only do this once, unless AGAIN is non-nil."
  (interactive (list 'again))
  ;; No need to worry about race conditions because Elisp isn't
  ;; concurrent (yet).
  (unless (and arche--env-setup-p (not again))
    (let (;; Current directory may not exist in certain horrifying
          ;; circumstances (yes, this has happened in practice).
          (default-directory "/")
          (profile-file "~/.profile")
          (buf-name " *arche-env-output*"))
      (when (and profile-file
                 (file-exists-p profile-file)
                 (executable-find "python"))
        (ignore-errors (kill-buffer buf-name))
        (with-current-buffer (get-buffer-create buf-name)
          (let* ((python-script
                  (expand-file-name "scripts/print_env.py" arche-directory))
                 (delimiter (arche--random-string))
                 (sh-script (format ". %s && %s %s"
                                    (shell-quote-argument
                                     (expand-file-name profile-file))
                                    (shell-quote-argument python-script)
                                    (shell-quote-argument delimiter)))
                 (return (call-process "sh" nil t nil "-c" sh-script))
                 (found-delimiter
                  (progn
                    (goto-char (point-min))
                    (search-forward delimiter nil 'noerror))))
            (if (and (= 0 return) found-delimiter)
                (let* ((results (split-string
                                 (buffer-string) (regexp-quote delimiter)))
                       (results (cl-subseq results 1 (1- (length results)))))
                  (if (cl-evenp (length results))
                      (progn
                        (cl-loop for (var value) on results by #'cddr do
                                 (setenv var value)
                                 (when (string= var "PATH")
                                   (setq exec-path (append
                                                    (parse-colon-path value)
                                                    (list exec-directory)))))
                        (setq arche--env-setup-p t))
                    (message
                     "Loading %s produced malformed result; see buffer %S"
                     profile-file
                     buf-name)))
              (message "Failed to load %s; see buffer %S"
                       profile-file
                       buf-name))))))))

(defvar arche--env-setup-timer
  (run-at-time 1 nil #'arche-env-setup)
  "Timer used to run `arche-env-setup'.
We (mostly) don't need environment variables to be set correctly
during init, so deferring their processing saves some time at
startup.")

;;;; Clipboard integration

;; On macOS, clipboard integration works out of the box in windowed
;; mode but not terminal mode. The following code to fix it was
;; originally based on [1], and then modified based on [2].
;;
;; [1]: https://gist.github.com/the-kenny/267162
;; [2]: https://emacs.stackexchange.com/q/26471/12534
(arche-with-operating-system macOS
  (unless (display-graphic-p)

    (defvar arche--clipboard-last-copy nil
      "The last text that was copied to the system clipboard.
This is used to prevent duplicate entries in the kill ring.")

    (eval-and-compile
      (defun arche--clipboard-paste ()
        "Return the contents of the macOS clipboard, as a string."
        (let* (;; Setting `default-directory' to a directory that is
               ;; sure to exist means that this code won't error out
               ;; when the directory for the current buffer does not
               ;; exist.
               (default-directory "/")
               ;; Command pbpaste returns the clipboard contents as a
               ;; string.
               (text (shell-command-to-string "pbpaste")))
          ;; If this function returns nil then the system clipboard is
          ;; ignored and the first element in the kill ring (which, if
          ;; the system clipboard has not been modified since the last
          ;; kill, will be the same) is used instead. Including this
          ;; `unless' clause prevents you from getting the same text
          ;; yanked the first time you run `yank-pop'.
          (unless (string= text arche--clipboard-last-copy)
            text)))

      (defun arche--clipboard-copy (text)
        "Set the contents of the macOS clipboard to given TEXT string."
        (let* (;; Setting `default-directory' to a directory that is
               ;; sure to exist means that this code won't error out
               ;; when the directory for the current buffer does not
               ;; exist.
               (default-directory "/")
               ;; Setting `process-connection-type' makes Emacs use a pipe to
               ;; communicate with pbcopy, rather than a pty (which is
               ;; overkill).
               (process-connection-type nil)
               ;; The nil argument tells Emacs to discard stdout and
               ;; stderr. Note, we aren't using `call-process' here
               ;; because we want this command to be asynchronous.
               ;;
               ;; Command pbcopy writes stdin to the clipboard until it
               ;; receives EOF.
               (proc (start-process "pbcopy" nil "pbcopy")))
          (process-send-string proc text)
          (process-send-eof proc))
        (setq arche--clipboard-last-copy text)))

    (setq interprogram-paste-function #'arche--clipboard-paste)
    (setq interprogram-cut-function #'arche--clipboard-copy)))

;; If you have something on the system clipboard, and then kill
;; something in Emacs, then by default whatever you had on the system
;; clipboard is gone and there is no way to get it back. Setting the
;; following option makes it so that when you kill something in Emacs,
;; whatever was previously on the system clipboard is pushed into the
;; kill ring. This way, you can paste it with `yank-pop'.
(setq save-interprogram-paste-before-kill t)

(arche-defadvice arche--advice-gui-get-selection-quietly (func &rest args)
  :around #'gui-selection-value
  "Disable an annoying message emitted when Emacs can't yank something.
In particular, if you have an image on your system clipboard and
you either yank or kill (as `save-interprogram-paste-before-kill'
means Emacs will try to put the system clipboard contents into
the kill ring when you kill something new), you'll get the
message 'gui-get-selection: (error \"Selection owner couldn't
convert\" UTF8_STRING)'. Disable that."
  (arche--with-silent-message "Selection owner couldn't convert"
    (apply func args)))

;;;; Mouse integration

;; Scrolling is way too fast on macOS with Emacs 27 and on Linux in
;; general. Decreasing the number of lines we scroll per mouse event
;; improves the situation. Normally, holding shift allows this slower
;; scrolling; instead, we make it so that holding shift accelerates
;; the scrolling.
(setq mouse-wheel-scroll-amount
      '(1 ((shift) . 5) ((control))))

;; Mouse integration works out of the box in windowed mode but not
;; terminal mode. The following code to fix it was based on
;; <https://stackoverflow.com/a/8859057/3538165>.
(unless (display-graphic-p)

  ;; Enable basic mouse support (click and drag).
  (xterm-mouse-mode t)

  ;; Note that the reason for the next two functions is that
  ;; `scroll-down' and `scroll-up' scroll by a "near full screen"
  ;; by default, whereas we want a single line.

  (eval-and-compile
    (defun arche-scroll-down ()
      "Scroll down one line."
      (interactive)
      (scroll-down 1))

    (defun arche-scroll-up ()
      "Scroll up one line."
      (interactive)
      (scroll-up 1)))

  ;; Enable scrolling with the mouse wheel.
  (bind-key "<mouse-4>" #'arche-scroll-down)
  (bind-key "<mouse-5>" #'arche-scroll-up))

;;; Candidate selection

;; Allow doing a command that requires candidate-selection when you
;; are already in the middle of candidate-selection. Sometimes it's
;; handy!
(setq enable-recursive-minibuffers t)

(arche-defadvice arche--advice-eval-expression-save-garbage
    (func prompt &optional initial-contents keymap read &rest args)
  :around #'read-from-minibuffer
  "Save user input in history even if it's not a valid sexp.
We do this by forcing `read-from-minibuffer' to always be called
with a nil value for READ, and then handling the effects of READ
ourselves."
  (let ((input (apply func prompt initial-contents keymap nil args)))
    (when read
      ;; This is based on string_to_object in minibuf.c.
      (let ((result (read-from-string input)))
        (unless (string-match-p
                 "\\`[ \t\n]*\\'" (substring input (cdr result)))
          (signal
           'invalid-read-syntax
           '("Trailing garbage following expression")))
        (setq input (car result))))
    input))

;; Package `selectrum' is an incremental completion and narrowing
;; framework. Like Ivy and Helm, which it improves on, Selectrum
;; provides a user interface for choosing from a list of options by
;; typing a query to narrow the list, and then selecting one of the
;; remaining candidates. This offers a significant improvement over
;; the default Emacs interface for candidate selection.
(use-package selectrum
  :straight (:host github :repo "raxod502/selectrum")
  :defer t
  :init

  ;; This doesn't actually load Selectrum.
  (selectrum-mode +1))

;; Package `prescient' is a library for intelligent sorting and
;; filtering in various contexts.
(use-package prescient
  :config

  ;; Remember usage statistics across Emacs sessions.
  (prescient-persist-mode +1)

  ;; The default settings seem a little forgetful to me. Let's try
  ;; this out.
  (setq prescient-history-length 1000))

;; Package `selectrum-prescient' provides intelligent sorting and
;; filtering for candidates in Selectrum menus.
(use-package selectrum-prescient
  :straight (:host github :repo "raxod502/prescient.el"
                   :files ("selectrum-prescient.el"))
  :demand t
  :after selectrum
  :config

  (selectrum-prescient-mode +1))











;; Local Variables:
;; checkdoc-symbol-words: ("top-level")
;; indent-tabs-mode: nil
;; outline-regexp: ";;;+ "
;; sentence-end-double-space: nil
;; End:
