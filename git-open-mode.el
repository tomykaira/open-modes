;;; git-open-mode.el --- provide often used actions for a git project

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

(defvar git-open-ignored '())

(require 'open-mode)

(defun git-open-root-p (current-path)
  (file-exists-p (or (expand-file-name ".git" current-path))))

(define-open-mode "git")

(provide 'git-open-mode)
