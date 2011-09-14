;;; rack-open-mode.el --- provide often used actions for rack projects

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

(defvar rack-open-process-name "rackup"
  "The name of `so-rackup' process in emacs.")

(defvar rack-open-process-path "bundle"
  "The path to the process which is executed with `so-rackup'")

(defvar rack-open-ignored '(".bundle"))

(require 'open-mode)

(easy-mmode-define-minor-mode rack-open-mode
"Rack open mode easily opens files in the temporary Rack project directory."
  nil
  " ro"
  '(("\C-c\C-r" . rack-open-anything)
    ("\C-c\C-b" . rack-open-grep-project)
    ("\C-c\C-d" . rack-open-rackup)))

(defun rack-open-rootp (current-path)
  (file-exists-p (expand-file-name "config.ru" current-path)))

(defun rack-open-root ()
  (om-project-root 'rack-open-rootp))

(defun rack-open-grep-project (query)
  (interactive "s")
  (om-grep-project 'rack-open-root query rack-open-ignored))

(defun rack-open-launch ()
  (interactive)
  (let* ((root (rack-open-root)))
  (if root (rack-open-mode t)
    (if (and (fboundp rack-open-mode) rack-open-mode)
      (rack-open-mode)))))

(defun rack-open-anything()
  (interactive)
  (anything-other-buffer (om-make-anything-sources 'rack-open-root rack-open-ignored) nil))

(defun rack-open-activate ()
  (interactive)
  (add-hook 'find-file-hook 'rack-open-launch)
  (add-hook 'dired-after-readin-hook 'rack-open-launch))

(defun rack-open-deactivate ()
  (interactive)
  (remove-hook 'find-file-hook 'rack-open-launch)
  (remove-hook 'dired-after-readin-hook 'rack-open-launch))

(defun rack-open-rackup ()
  (interactive)
  (if (get-process rack-open-process-name)
      (rack-open-rack-stop))
  (start-process rack-open-process-name "*rackup*" rack-open-process-path "exec" "rackup" (concat (rack-open-root) "config.ru")))

(defun rack-open-rack-stop ()
  (interactive)
  (delete-process rack-open-process-name))

(provide 'rack-open-mode)
