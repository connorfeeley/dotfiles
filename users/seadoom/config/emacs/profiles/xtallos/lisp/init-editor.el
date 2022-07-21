;;; init-editor.el --- Editor configuration -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2022, Chris Montgomery <chris@cdom.io>
;;
;; Author: Chris Montgomery <https://github.com/montchr>
;; Maintainer: Chris Montgomery <chris@cdom.io>
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))
;;
;; Created: 06 Feb 2022
;;
;; URL: https://github.com/montchr/dotfield/tree/main/config/emacs
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Editor customizations for Dotfield.
;;
;;; Sources:
;;
;; https://github.com/d12frosted/environment/blob/master/emacs/lisp/init-editor.el
;;
;;; Code:

;; Backup files sound useful but get in the way, experts say.
(setq make-backup-files nil)

;; Search improvements.
(setq-default search-default-mode #'char-fold-to-regexp)
(setq-default replace-char-fold t)

;; Electrify everything by default.
(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(defun editor-disable-electric-pair ()
  "Disable the command `electric-pair-mode' locally."
  (electric-pair-local-mode -1))
(add-hook 'after-init-hook 'electric-indent-mode)
(defun editor-disable-electric-indent ()
  "Disable the command `electric-indent-mode' locally."
  (electric-indent-local-mode -1))

;; Replace active region upon input.
(delete-selection-mode +1)
;; Enable column numbers.
(column-number-mode +1)

;; Auto-save files, because I forget.
(setq-default
 auto-save-list-file-prefix (expand-file-name
                             "auto-save-list/.saves-"
                             path-cache-dir))

;; Refresh buffers every so often in case a file changes outside of Emacs.
(use-package autorevert
  :config
  (global-auto-revert-mode +1)
  (setq auto-revert-interval 2
        auto-revert-check-vc-info t
        global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

(use-package paren
  :init (setq show-paren-delay 0)
  :config (show-paren-mode +1))

(use-package elec-pair
  :hook (prog-mode . electric-pair-mode))

;; Whitespace
(setq-default indent-tabs-mode nil)
(setq-default tab-width xtallos/indent-width)
(setq-default require-final-newline t)
(setq-default tab-always-indent t)
(setq-default delete-trailing-lines t)
(setq-default sentence-end-double-space t)
(setq-default word-wrap t)

(defun editor-show-trailing-whitespace ()
  "Enable display of trailing whitespace in this buffer."
  (setq-local show-trailing-whitespace t))
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook 'editor-show-trailing-whitespace))

(use-package adaptive-wrap
  :defer t)

(setq-default fill-column 80)
(use-package visual-fill-column
  :hook ((visual-line-mode . visual-fill-column-mode)))

(use-package unfill
  :commands (unfill-toggle)
  :general
  ("M-q" #'unfill-toggle))

(use-package move-text
  :commands (move-text-up
             move-text-down)
  :general
  ("M-S-<down>" #'move-text-down)
  ("M-S-<up>" #'move-text-up))

(use-package avy
  :defer t
  :general
  (xtallos/leader-def
    "jj" '(avy-goto-char :which-key "char")
    "jl" '(avy-goto-line :which-key "line")
    "jw" '(avy-goto-word-0 :which-key "word")
    "jJ" '(avy-goto-char-timer :which-key "charS")))

(use-package ace-link
  :defer t
  :general
  (xtallos/leader-def
    "jb" '(ace-link :which-key "btn")))

(provide 'init-editor)
;;; init-editor.el ends here
