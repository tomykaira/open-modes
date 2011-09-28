;;; ruby-open-mode.el --- provide often used actions for rack projects

;;; Version 0.1 - 2011-9-14
;;; Copyright (C) 2011 tomykaira (tomykaira@gmail.com)
;;;
;;; Author: tomykaira -- tomykaira@gmail.com

;;; This file is NOT part of GNU Emacs.
;;; You may however redistribute it and/or modify it under the terms of the GNU
;;; General Public License as published by the Free Software Foundation; either
;;; version 2, or (at your option) any later version.
;;;
;;; The file is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

(defvar ruby-open-ignored '(".bundle"))

(require 'open-mode)

(easy-mmode-define-minor-mode ruby-open-mode
"Rack open mode easily opens files in the temporary Rack project directory."
  nil
  " ro"
  '(("\C-c\C-r" . ruby-open-anything)
    ("\C-c\C-b" . ruby-open-grep-project)))

(defun ruby-open-rootp (current-path)
  (file-exists-p (expand-file-name "Gemfile" current-path)))

(defun ruby-open-root ()
  (om-project-root 'ruby-open-rootp))

(defun ruby-open-grep-project (query)
  (interactive "s")
  (om-grep-project 'ruby-open-root query ruby-open-ignored))

(defun ruby-open-launch ()
  (interactive)
  (let* ((root (ruby-open-root)))
  (if root (ruby-open-mode t)
    (if (and (fboundp ruby-open-mode) ruby-open-mode)
      (ruby-open-mode)))))

(defun ruby-open-anything()
  (interactive)
  (anything-other-buffer (om-make-anything-sources 'ruby-open-root ruby-open-ignored) nil))

(defun ruby-open-activate ()
  (interactive)
  (add-hook 'find-file-hook 'ruby-open-launch)
  (add-hook 'dired-after-readin-hook 'ruby-open-launch))

(defun ruby-open-deactivate ()
  (interactive)
  (remove-hook 'find-file-hook 'ruby-open-launch)
  (remove-hook 'dired-after-readin-hook 'ruby-open-launch))

(provide 'ruby-open-mode)
