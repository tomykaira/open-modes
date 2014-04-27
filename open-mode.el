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

(eval-when-compile (require 'cl))

(defvar om--temporary-project-root nil
  "[internal use] temporary variable: base directory")

(defun om-project-root (rootp &optional dir)
  (setq dir (or dir
                (buffer-file-name)
                (concat list-buffers-directory)))
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

(defun om--directory-files-recursive (ignored-dir-list dir)
  (if (file-directory-p dir)
      (let ((ignored-dir-list (om--local-ignored-dir-list ignored-dir-list)) files)
        (mapcar
         (lambda (child)
           (unless (om--is-ignored ignored-dir-list dir)
             (if (file-directory-p child)
                 (setq files (append files (om--directory-files-recursive ignored-dir-list child)))
               (setq files (cons child files)))))
         (directory-files dir t))
        files)
    (list dir)))

(defun om--subdirectory-files (root ignored-dir-list top)
  (mapcar (lambda (x) (replace-regexp-in-string (concat "^" root) "" x))
          (om--directory-files-recursive ignored-dir-list (concat root top))))

(defun om--is-ignored (ignored-name-list file)
  (find-if (lambda (regex) (string-match regex file)) ignored-name-list))

(defun om--local-ignored-dir-list (ignored-dir-list)
  (append (list "^\\." "\\.\\.?$" "\\.git$" "\\.svn$")
          (mapcar (lambda (str) (regexp-quote str)) ignored-dir-list)))

(defun om-make-helm-sources (getroot ignored-dir-list open-method)
  "Make helm sources for open mode"
  (let ((root (funcall getroot))
        (ignored-dir-list (om--local-ignored-dir-list ignored-dir-list))
        (action (case open-method
                  ('other-window 'om-helm-c-open-candidate-in-other-window)
                  ('new-screen 'om-helm-c-open-candidate-in-new-screen)))
    sources root-files path)
    (setq om--temporary-project-root root)
    (mapcar (lambda (file)
              (unless (om--is-ignored ignored-dir-list file)
                (setq path (concat root file))
                (if (file-directory-p path)
                    (push
                     `((name . ,file)
                       (candidates . ,(om--subdirectory-files root ignored-dir-list file))
                       (action . ,action))
                     sources)
                  (push file root-files))))
            (directory-files root nil))
  (push
   `((name . "root")
     (candidates . ,root-files)
     (action . ,action))
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

(defun om-helm-c-open-candidate-in-other-window(cand-file)
  (let ((candidate (concat om--temporary-project-root cand-file)))
    (if (one-window-p)
        (split-window-horizontally)
      (other-window 1))
    (find-file candidate)))

(defun om-helm-c-open-candidate-in-new-screen(cand-file)
  (let* ((candidate (concat om--temporary-project-root cand-file))
         (existing-screen (elscreen-find-screen-by-file-path candidate)))
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
     (defun ,(--sym "helm") (args)
       (interactive "P")
       (helm-other-buffer
        (om-make-helm-sources ',(--sym "root") ,(--sym "ignored") (if args 'other-window 'new-screen))
        nil))
     (defun ,(--sym "grep-project") (query)
       (interactive "s")
       (om-grep-project ',(--sym "root") query ,(--sym "ignored")))
     (easy-mmode-define-minor-mode
      ,(--sym "mode") ,(concat mode "-open minor mode")nil ""
      '(("\C-c\C-r" . ,(--sym "helm"))
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
