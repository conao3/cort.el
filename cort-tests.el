;;; cort-tests.el --- cort-test test file       -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Naoya Yamashita <conao3@gmail.com>

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; Keywords: test lisp
;; Version: 6.0.0
;; URL: https://github.com/conao3/cort.el
;; Package-Requires: ((emacs "24.0"))

;;   Abobe declared this package requires Emacs-24, but it's for warning
;;   suppression, and will actually work from Emacs-22.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the Affero GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the Affero
;; GNU General Public License for more details.

;; You should have received a copy of the Affero GNU General Public
;; License along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; cort-test test file

;;; Code:

(require 'cort-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  temp functions, macros
;;

(defun quote-a ()
  'a)

(defmacro sym (x)
  `',x)

(defmacro match-expansion (form expect)
  `(:equal (macroexpand ',form) ,expect))

(defun match-expansion-fn (form expect)
  `(:equal ',(macroexpand form) ',expect))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; define tests
;;

(defvar d 'a)
(cort-deftest simple:equal1
  `((:equal ',d 'a)))

(cort-deftest simple:equal2
  (let ((e 'b))
    `((:equal ',e 'b))))

(cort-deftest simple:=
  '((:= 100 100)))

(cort-deftest quote-a
  '((:eq 'a 'a)
    (:eq (quote-a) 'a)
    (:eq 'a (quote-a))
    (:eq (quote-a) (quote-a))))

(cort-deftest arith
  '((:= 9   (+ 4 5))
    (:= -1  (- 4 5))
    (:= 20  (* 4 5))
    (:= 0   (/ 4 5))
    (:= 0.8 (/ 4.0 5))
    (:= 4   (mod 4 5))))

(cort-deftest string-concat
  '((:string= "aaabbb" (concat "aaa" "bbb"))
    (:string= "aaa,bbb,ccc"
              (mapconcat #'identity '("aaa" "bbb" "ccc") ","))))

(cort-deftest string-split
  '((:equal '("aaa" "bbb" "ccc") (split-string "aaa,bbb,ccc" ","))))

(cort-deftest string-length
  '((:= 5  (length "asdfg"))
    (:= 5  (length "あいうえお"))
    (:= 10 (string-width "あいうえお"))))

(cort-deftest string-pickup
  '((:string= "ab"    (substring "abcdef" 0 2))
    (:string= "abcd"  (substring "abcdef" 0 -2))
    (:string= "abcde" (substring "abcdef" 0 -1))
    (:string= "cdef"  (substring "abcdef" 2))))

(cort-deftest string-serch
  '((:= 1 (string-match "bc" "abcd"))))

(cort-deftest list-reference
  (let ((lst '(100 200 300)))
    `((:=     100        (car ',lst))
      (:equal '(200 300) (cdr ',lst))

      (:= 100 (nth 0 ',lst))
      (:= 100 (elt ',lst 0)))))

(cort-deftest err
  '((:cort-error 'void-function (a 'a))
    (:cort-error 'error (a 'a))
    (:cort-error 'arith-error (/ 1 0))
    (:cort-error 'void-variable (+ 1 a))))

(provide 'leaf-tests)
;;; leaf-tests.el ends here
