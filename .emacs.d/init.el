;;; init.el --- My personal configuration file for Emacs -*- lexical-binding: t -*-

;; Copyright (c) 2016-2019 Hiroshi Atsuta

;; Author: Hiroshi Atsuta <atsuta.hiroshi@gmail.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This is just a configuration file for Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
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

;; Change working directory to HOME unless non-default initialization
;; file is specified.
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

;;; Setup package manager

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


;;;; blackout

;; Package `blackout' provides a function to hide or customize the
;; display of major and minor modes in the mode line.
(use-package blackout
  :straight (:host github :repo "raxod502/blackout")
  :demand t)


;;; init.el ends here
