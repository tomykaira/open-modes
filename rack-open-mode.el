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

(defun rack-open-root-p (current-path)
  (file-exists-p (expand-file-name "config.ru" current-path)))

(define-open-mode "rack")

(defun rack-open-rackup ()
  (interactive)
  (let ((buffer-name "*rackup*"))
    (if (get-process rack-open-process-name)
        (rack-open-rack-stop))
    (save-current-buffer
      (set-buffer buffer-name)
      (erase-buffer)
      (message "Starting Rack...")
      (start-process rack-open-process-name buffer-name rack-open-process-path "exec" "rackup" (concat (rack-open-root) "config.ru")))))

(defun rack-open-rack-stop ()
  (interactive)
  (delete-process rack-open-process-name))

(define-key rack-open-mode-map "\C-c\C-d" 'rack-open-rackup)

(provide 'rack-open-mode)
