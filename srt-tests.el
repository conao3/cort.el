;;; srt-tests.el ---                                 -*- lexical-binding: t; -*-

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
(require 'srt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  temp functions, macros
;;

(defun quote-a ()
  'a)

(defmacro sym (x)
  `',x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; define tests
;;

(srt-deftest simple:equal
  (:equal '(a b c) '(a b c)))

(srt-deftest simple:=
  (:= 100 100))

(srt-deftest quote-a:0
  (:eq 'a 'a))

(srt-deftest quote-a:1
  (:eq (quote-a) 'a))

(srt-deftest quote-a:2
  (:eq 'a (quote-a)))

(srt-deftest quote-a:3
  (:eq (quote-a) (quote-a)))

(srt-deftest sym:0
  (:eq 'a 'a))

(srt-deftest sym:1
  (:eq (sym a) 'a))

(srt-deftest sym:2
  (:eq 'a (sym a)))

(srt-deftest sym:3
  (:eq (sym a) (sym a)))

(srt-deftest sym:4
  (:equal (sym (a b c)) '(a b c)))

(srt-deftest sym:5
  (:equal '(a b c) (sym (a b c))))

(srt-deftest sym:6
  (:equal (sym (a b c)) (sym (a b c))))


(provide 'leaf-tests)
;;; leaf-tests.el ends here
