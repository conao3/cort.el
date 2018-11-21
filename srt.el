;;; srt.el ---                                       -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita
;; Keywords: test

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar srt-test-cases nil)

(defmacro srt-deftest (name keys)
  (declare (indent 1))
  `(add-to-list 'srt-test-cases '(,name ,keys) t))

(defun srt-run-tests ()
  (dolist (test srt-test-cases)
    (let ((name (car test))
	  (keys (cadr test)))
      (srt-test keys))))

(defun srt-test (keys)
  (let ((fn (lambda (keys)
	      (when keys
		(princ (format "%s, " (car keys)))
		(funcall fn (cdr keys))))))
    (funcall fn keys)
    (princ "\n")))

(provide 'srt)
;;; srt.el ends here
