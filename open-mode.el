;;; open-mode.el --- provide functions for *-open-mode

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

;; This is not expected to use by itself.

(defun om-project-root (rootp &optional dir)
  (or dir (setq dir (or (buffer-file-name)
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

(defun om-make-anything-sources (getroot ignored-dir-list)
  "Make anything sources for open mode"
  (let ((root (funcall getroot))
        (ignored-dir-list
         (append (list "\\.\\.?$" "\\.git$")
                 (mapcar (lambda (str) (regexp-quote str)) ignored-dir-list)))
    sources root-files path ignoredp)
    (mapcar (lambda (file)
              (setq ignoredp nil)
              (dolist (regex ignored-dir-list ignoredp)
                (setq ignoredp (or ignoredp (string-match regex file))))
							(setq path (concat root file))
							(if (and (not ignoredp) (file-directory-p path))
									(push
									 `((name . ,file)
										 (candidates . ,(ro-directory-files-recursive path))
										 (action . om-anything-c-open-candidate))
									 sources)
								(push path root-files)))
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
  (let* ((existing-screen
      (first (elscreen-find-screen-by-file-path candidate))))
  (if existing-screen
      (elscreen-goto existing-screen)
    (if (fboundp 'elscreen-create)
        (elscreen-create)
      (delete-other-windows))
    (split-window-horizontally)
    (find-file candidate))))

(provide 'open-mode)
