;;; open-mode.el --- provide functions for *-open-mode

;;; Version 0.2 - 2012-1-8
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

;; This is not expected to use by itself.

(defun om-project-root (rootp &optional dir)
  (setq dir (or dir
                (buffer-file-name)
                (concat list-buffers-directory))))
  (if (funcall rootp dir)
      dir
    (let ((new-dir (expand-file-name (file-name-as-directory "..") dir)))
      ;; regexp to match windows roots, tramp roots, or regular posix roots
      (unless (string-match "\\(^[[:alpha:]]:/$\\|^/[^\/]+:/?$\\|^/$\\)" dir)
        (om-project-root rootp new-dir)))))

(defun om-grep-project (getroot query ignored-dirs)
  (let ((grep-find-ignored-directories
         (append grep-find-ignored-directories ignored-dirs)))
    (rgrep query "*.*" (funcall getroot))))

(defun om--directory-files-recursive (dir)
  (if (file-directory-p dir)
      (let (files)
        (mapcar
         (lambda (child)
           (unless (string-match "\\.\\.?$" child)
             (if (file-directory-p child)
                 (setq files (append files (om--directory-files-recursive child)))
               (setq files (cons child files)))))
         (directory-files dir t))
        files)
    (list dir)))

(defun om-make-anything-sources (getroot ignored-dir-list)
  "Make anything sources for open mode"
  (let ((root (funcall getroot))
        (ignored-dir-list
         (append (list "\\.\\.?$" "\\.git$")
                 (mapcar (lambda (str) (regexp-quote str)) ignored-dir-list)))
    sources root-files path)
    (mapcar (lambda (file)
              (catch 'ignored
                (dolist (regex ignored-dir-list)
                  (if (string-match regex file)
                      (throw 'ignored t)))
                (setq path (concat root file))
                (if (file-directory-p path)
                    (push
                     `((name . ,file)
                       (candidates . ,(om--directory-files-recursive path))
                       (action . om-anything-c-open-candidate))
                     sources)
                  (push path root-files))))
            (directory-files root nil))
  (push
   `((name . "root")
     (candidates . ,root-files)
     (action . om-anything-c-open-candidate))
   sources)
  (reverse sources)))

;; elscreen extension
(defun elscreen-find-screen-by-file-path(PATH)
  (catch 'screen
    (elscreen-find-screen
     `(lambda (screen)
        (elscreen-goto-internal screen)
        (walk-windows
         '(lambda (x)
            (if (string= PATH (buffer-file-name (window-buffer x)))
                (throw 'screen screen))) nil)))))

(defun om-anything-c-open-candidate(candidate)
  (let* ((existing-screen (elscreen-find-screen-by-file-path candidate)))
    (cond
     (existing-screen
      (elscreen-goto existing-screen))
     ((string= (buffer-name) "*scratch*")
      (find-file candidate))
     ((or (other-window 1) (string= (buffer-name) "*scratch*"))
      (find-file candidate))
     ((fboundp 'elscreen-create)
      (elscreen-create)
      (split-window-horizontally)
      (find-file candidate)))))

(defmacro define-open-mode (mode)
  (defun --sym (suffix)
    (intern (concat mode "-open-" suffix)))
  `(progn
     (defun ,(--sym "root") ()
       (om-project-root ',(--sym "root-p")))
     (defun ,(--sym "anything")()
       (interactive)
       (anything-other-buffer (om-make-anything-sources ',(--sym "root") ,(--sym "ignored")) nil))
     (defun ,(--sym "grep-project") (query)
       (interactive "s")
       (om-grep-project ',(--sym "root") query ,(--sym "ignored")))
     (easy-mmode-define-minor-mode
      ,(--sym "mode") (concat mode "-open minor mode")nil " po"
      (("\C-c\C-r" . ,(--sym "anything"))
       ("\C-c\C-b" . ,(--sym "grep-project"))))
     (defun ,(--sym "launch") ()
       (interactive)
       (if (,(--sym "root"))
           (,(--sym "mode") 1)
         (if (fboundp ,(--sym "mode"))
             (,(--sym "mode") -1))))
     (defun ,(--sym "activate") ()
       (interactive)
       (add-hook 'find-file-hook ',(--sym "launch"))
       (add-hook 'dired-after-readin-hook ',(--sym "launch")))
     (defun ,(--sym "deactivate") ()
       (interactive)
       (remove-hook 'find-file-hook ',(--sym "launch"))
       (remove-hook 'dired-after-readin-hook ',(--sym "launch")))))

(provide 'open-mode)
