;;; php-open-mode.el --- provide often used actions for php projects

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

(defvar php-open-directory-server-pairs '(("/home/foo/bar" . "foo@deployment.example.com:"))
  "Specify the pairs of a local directory of your project and a corresponding 
staging server directory")

(defvar php-open-rsync-command "rsync -aurv -e "
  "Command to upload and deploy")

(defvar php-open-ignored '())

(require 'open-mode)

(defun php-open-root-p (current-path)
  (not (not (member-if
   '(lambda (pair) (string= (concat (car pair) "/") current-path))
   php-open-directory-server-pairs))))

(define-open-mode "php")

(defun php-open-upload ()
  (interactive)
  (let* ((current-path (or (buffer-file-name) (concat list-buffers-directory)))
         (path-pair (php-open-directory-server-pair current-path)))
    (if path-pair
        (shell-command (concat php-open-rsync-command (car path-pair) "/* " (cdr path-pair)))
      (message "You are not in a PHP directory"))))

(define-key php-open-mode-map "\C-c\C-d" 'php-open-upload)

(provide 'php-open-mode)
