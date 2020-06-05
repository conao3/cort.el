;;; cort-tests.el --- cort test file       -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Naoya Yamashita <conao3@gmail.com>

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; URL: https://github.com/conao3/cort.el

;; This program is free software: you can redistribute it and/or modify
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

;; cort-test test file

;;; Code:

(require 'cort)

(defun quote-a ()
  'a)

(defvar var 'a)
(cort-deftest simple
  '((:equal var
            'a)
    (:= 100
        100)))

(cort-deftest quote-a
  '((:eq 'a 'a)
    (:eq (quote-a) 'a)
    (:eq 'a (quote-a))
    (:eq (quote-a) (quote-a))))

(cort-deftest arith
  '((:= (+ 4 5)   9)
    (:= (- 4 5)   -1)
    (:= (* 4 5)   20)
    (:= (/ 4 5)   0)
    (:= (/ 4.0 5) 0.8)
    (:= (mod 4 5) 4)))

(cort-deftest string-concat
  '((:string= (concat "aaa" "bbb") "aaabbb")
    (:string= (mapconcat #'identity '("aaa" "bbb" "ccc") ",")
              "aaa,bbb,ccc")))

(cort-deftest string-split
  '((:equal (split-string "aaa,bbb,ccc" ",") '("aaa" "bbb" "ccc"))))

(cort-deftest string-length
  '((:= (length "asdfg")  5)
    (:= (length "あいうえお")  5)
    (:= (string-width "あいうえお") 10)))

(cort-deftest string-pickup
  '((:string= (substring "abcdef" 0 2)  "ab")
    (:string= (substring "abcdef" 0 -2) "abcd")
    (:string= (substring "abcdef" 0 -1) "abcde")
    (:string= (substring "abcdef" 2)    "cdef")))

(cort-deftest string-serch
  '((:= (string-match "bc" "abcd") 1)))

(cort-deftest err
  '((:cort-error 'void-function (a 'a))
    (:cort-error 'error (a 'a))
    (:cort-error 'arith-error (/ 1 0))
    (:cort-error 'void-variable (+ 1 a))))

;; (provide 'cort-tests)
;;; cort-tests.el ends here
