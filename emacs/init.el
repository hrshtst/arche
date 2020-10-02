;; -*- lexical-binding: t -*-

;; Copyright (c) 2016-2020 Hiroshi Atsuta

;; Author: Hiroshi Atsuta <atsuta@ieee.org>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This file wraps the primary configuration (which lives in arche.el)
;; so that we don't have to wrap the entire file in various `let'
;; forms, etc. We put as much as possible in arche.el.

;; I learned an idea from Radon Rosborough's Emacs configuration
;; <https://github.com/raxod502/radian> to isolate the main
;; configuration from the init-file, and borrowed some snippets of
;; code from his implementation.

;; This allows us to instead load a different Emacs configuration by
;; exporting USER_EMACS_DIRECTORY to another .emacs.d directory.
(let ((alternate-user-emacs-directory (getenv "USER_EMACS_DIRECTORY")))

  (defvar arche--init-file-loaded-p nil
    "Non-nil if the init-file has already been loaded.
This is important for Emacs 27 and above, since our early
init-file just loads the regular init-file, which would lead to
loading the init-file twice if it were not for this variable.")

  (cond
   ;; If already loaded, do nothing. But still allow re-loading, just
   ;; do it only once during init.
   ((and (not after-init-time) arche--init-file-loaded-p))

   ;; Delegate to another Emacs configuration. (We still don't want to
   ;; load it twice.)
   (alternate-user-emacs-directory
    (setq alternate-user-emacs-directory
          (file-name-as-directory alternate-user-emacs-directory))
    (setq user-emacs-directory alternate-user-emacs-directory)
    (setq user-init-file (expand-file-name "init.el" user-emacs-directory))
    (load user-init-file 'noerror 'nomessage))
   (t
    (setq arche--init-file-loaded-p t)

    (defvar arche-minimum-emacs-version "27.1"
      "Minimum version of Emacs to support.
Our Emacs configuration does not support any versions below this.")

    (defvar arche-local-init-file
      (expand-file-name "init.local.el" user-emacs-directory)
      "File for local customizations of Emacs.")

    ;; Prevent package.el from modifying this file.
    (setq package-enable-at-startup nil)

    ;; Prevent Custom from modifying this file.
    (setq custom-file (expand-file-name
                       (format "custom-%d-%d.el" (emacs-pid) (random))
                       temporary-file-directory))

    ;; Make sure we are running a modern enough Emacs, otherwise abort
    ;; init.
    (if (version< emacs-version arche-minimum-emacs-version)
        (error (concat "Our Emacs configuration requires at least "
                       "Emacs %s, but you are running Emacs %s")
               arche-minimum-emacs-version emacs-version)

      (let* ((this-file (or
			 ;; We may be loading init.el in batch mode,
			 ;; in which case `user-init-file' is nil. In
			 ;; that case, we should have some backup
			 ;; options to try.
			 user-init-file
			 load-file-name
			 buffer-file-name))
	     (link-target
	      ;; This function returns the target of the link. If the
	      ;; init-file is not a symlink, then we abort.
	      (file-symlink-p this-file)))

        (unless link-target
          (error "Init-file %S is not a symlink" this-file))

        (defvar arche-lib-file (expand-file-name
                                "arche.el"
                                (file-name-directory link-target))
          "File containing the main configuration.
This file is loaded by init.el")

        (unless (file-exists-p arche-lib-file)
          (error "Library file %S does not exist" arche-lib-file))

        (defvar arche--finalize-init-hook nil
          "Hook run unconditionally after init, even if it fails.
Unlike `after-init-hook', this hook is run every time the
init-file is loaded, not just once.")

        (unwind-protect
            ;; Load the main configuration code. Disable
            ;; `file-name-handler-alist' to improve load time.
            ;;
            ;; Make sure not to load an out-of-date .elc file. Since
            ;; we byte-compile asynchronously in the background after
            ;; init succeeds, this case will happen often.
            (let ((file-name-handler-alist nil)
                  (load-prefer-newer t)
                  (stale-bytecode t))
              (catch 'stale-bytecode
                ;; We actually embed the contents of the local
                ;; init-file directly into the compiled arche.elc, so
                ;; that it can get compiled as well (and its
                ;; macroexpansion can use packages that are only
                ;; loaded at compile-time). So that means we have to go
                ;; the slow path if the local init-file has been
                ;; updated more recently than the compiled arche.elc.
                (when (file-newer-than-file-p
                       arche-local-init-file
                       (concat arche-lib-file "c"))
                  (throw 'stale-bytecode nil))
                (load
                 (file-name-sans-extension arche-lib-file)
                 nil 'nomessage)
                (setq stale-bytecode nil))
              (when stale-bytecode
                ;; Don't bother trying to recompile, unlike in
                ;; straight.el, since we are going to handle that
                ;; later, asynchronously.
                (ignore-errors
                  (delete-file (concat arche-lib-file "c")))
                (load arche-lib-file nil 'nomessage 'nosuffix)))
          (run-hooks 'arche--finalize-init-hook)))))))
