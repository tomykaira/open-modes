;;; ruby-open-mode.el --- provide often used actions for rack projects

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

(defvar ruby-open-ignored '(".bundle"))

(require 'open-mode)

(defun ruby-open-root-p (current-path)
  (file-exists-p (expand-file-name "Gemfile" current-path)))

(define-open-mode "ruby")

(defun ruby-open-cand-spec (cand-file)
  (let* ((cand-frags (nreverse (split-string cand-file "/")))
         (cand-file-name (car cand-frags)))
    (if (string-match "\.rb$" cand-file-name)
        (let* ((spec-name (concat (car (split-string cand-file-name "\\.")) "_spec.rb"))
               (files (om--subdirectory-files om--temporary-project-root ruby-open-ignored "spec"))
               (match-list
                (mapcar (lambda (path)
                          (let* ((spec-frags (nreverse (split-string path "/")))
                                 (depth 0))
                            (when (string= spec-name (car spec-frags))
                              (setq depth 1)
                              (while (string= (nth depth cand-frags) (nth depth spec-frags))
                                (incf depth)))
                            (cons path depth)))
                        files))
               (result (car (sort match-list (lambda (p q) (> (cdr p) (cdr q)))))))
          (unless (= (cdr result) 0)
              (car result))))))

(defun ruby-open-helm-c-open-candidate-in-new-screen (cand-file)
  (let* ((candidate (concat om--temporary-project-root cand-file))
         (existing-screen (elscreen-find-screen-by-file-path candidate)))
    (cond
     (existing-screen
      (elscreen-goto existing-screen))
     ((fboundp 'elscreen-create)
      (elscreen-create)
      (split-window-horizontally)
      (find-file candidate)
      (let ((cand-spec (ruby-open-cand-spec cand-file)))
        (when cand-spec
          (other-window 1)
          (find-file (concat om--temporary-project-root cand-spec))))))))

(defun ruby-open-helm (args)
  (interactive "P")
  (letf (((symbol-function 'om-helm-c-open-candidate-in-new-screen)
          (symbol-function 'ruby-open-helm-c-open-candidate-in-new-screen)))
    (helm-other-buffer
     (om-make-helm-sources 'ruby-open-root ruby-open-ignored (if args 'other-window 'new-screen))
     nil)))

(provide 'ruby-open-mode)
