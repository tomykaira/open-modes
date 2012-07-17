;;; scala-open-mode.el --- provide often used actions for rack projects

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

(defvar scala-open-ignored '("target" "project"))

(require 'open-mode)

(defun scala-open-root-p (current-path)
  (or (file-exists-p (expand-file-name "build.sbt" current-path))
      (file-exists-p (expand-file-name "Build.sbt" current-path))))

(define-open-mode "scala")

(defun scala-open-cand-spec (cand-file)
  (let ((cand-file-name (car (last (split-string cand-file "/")))))
    (message cand-file-name)
    (if (string-match "\.scala$" cand-file-name)
        (let ((fn (concat "/" (car (split-string cand-file-name "\\."))
                         "Spec.scala"))
              (files (om--subdirectory-files om--temporary-project-root "src/test")))
          (message fn)
          (car (delq nil (mapcar (lambda (s) (and (numberp (string-match (regexp-quote fn) s)) s)) files)))))))

(defun scala-open-anything-c-open-candidate-in-new-screen (cand-file)
  (let* ((candidate (concat om--temporary-project-root cand-file)) 
         (existing-screen (elscreen-find-screen-by-file-path candidate)))
    (cond
     (existing-screen
      (elscreen-goto existing-screen))
     ((fboundp 'elscreen-create)
      (elscreen-create)
      (split-window-horizontally)
      (find-file candidate)
      (let ((cand-spec (scala-open-cand-spec cand-file)))
        (when cand-spec
          (other-window 1)
          (find-file (concat om--temporary-project-root cand-spec))))))))

(defun scala-open-anything (args)
  (interactive "P")
  (letf (((symbol-function 'om-anything-c-open-candidate-in-new-screen)
          (symbol-function 'scala-open-anything-c-open-candidate-in-new-screen)))
    (anything-other-buffer
     (om-make-anything-sources 'scala-open-root scala-open-ignored (if args 'other-window 'new-screen))
     nil)))

(provide 'scala-open-mode)
