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

(easy-mmode-define-minor-mode php-open-mode
"Rack open mode easily opens files in the temporary Rack project directory."
  nil
  " po"
  '(("\C-c\C-r" . php-open-anything)
    ("\C-c\C-b" . php-open-grep-project)
    ("\C-c\C-d" . php-open-upload)))

(defun php-open-directory-server-pair (current-path)
  (car (member-if
        '(lambda (pair) (string-match-p (car pair) current-path))
        php-open-directory-server-pairs)))

(defun php-open-root ()
  (let* ((file (or (buffer-file-name)
                  (concat list-buffers-directory)))
         (pair (php-open-directory-server-pair file)))
    (cond (pair (concat (car pair) "/"))
          (t nil))))

(defun php-open-grep-project (query)
  (interactive "s")
  (om-grep-project 'php-open-root query php-open-ignored))

(defun php-open-launch ()
  (interactive)
  (let* ((root (php-open-root)))
  (if root (php-open-mode t)
    (if (and (fboundp php-open-mode) php-open-mode)
      (php-open-mode)))))

(defun php-open-anything()
  (interactive)
  (anything-other-buffer (om-make-anything-sources 'php-open-root php-open-ignored) nil))

(defun php-open-activate ()
  (interactive)
  (add-hook 'find-file-hook 'php-open-launch)
  (add-hook 'dired-after-readin-hook 'php-open-launch))

(defun php-open-deactivate ()
  (interactive)
  (remove-hook 'find-file-hook 'php-open-launch)
  (remove-hook 'dired-after-readin-hook 'php-open-launch))

(defun php-open-upload ()
  (interactive)
  (let* ((current-path (or (buffer-file-name) (concat list-buffers-directory)))
         (path-pair (php-open-directory-server-pair current-path)))
    (if path-pair
        (shell-command (concat php-open-rsync-command (car path-pair) "/* " (cdr path-pair)))
      (message "You are not in a PHP directory"))))

(provide 'php-open-mode)
