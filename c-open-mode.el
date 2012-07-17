;;; c-open-mode.el --- provide often used actions for rack projects

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

(defvar c-open-ignored '("*\\.o"))

(require 'open-mode)

(defun c-open-root-p (current-path)
  (file-exists-p (or (expand-file-name "Makefile" current-path)
                     (expand-file-name "OMakefile" current-path))))

(define-open-mode "c")

(provide 'c-open-mode)
