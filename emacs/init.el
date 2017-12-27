;;; init.el --- Initialization file for Emacs
;;
;; Copyright (c) 2016-2017 Hiroshi Atsuta
;;
;; Author: Hiroshi Atsuta <atsuta.hiroshi@gmail.com>
;; URL:
;; Keywords:

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

(package-initialize)

(unless load-file-name
  (cd (getenv "HOME")))

(require 'cl-lib)

(when load-file-name
  (setq-default user-emacs-directory (file-name-directory load-file-name)))

(load (concat user-emacs-directory "init-el-get.el"))

;; load environment variables
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PYTHONPATH" "VIRTUAL_ENV" "GTAGSLIBPATH")))

;; custom-file
(setq custom-file (concat user-emacs-directory "emacs-custom.el"))

;; init-loader
(custom-set-variables
 '(init-loader-show-log-after-init 'error-only))
(init-loader-load (concat user-emacs-directory "init-loader"))

;;; init.el ends here
