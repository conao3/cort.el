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

;; (srt-deftest debug:fail
;;   (:= (+ 1 3) 5))
;; 
;; (srt-deftest debug:error
;;   (:equa 'a 'a))
;; 
;; (srt-deftest debug:unexpected-error
;;   (:srt-error 'arith-error
;; 	      (a 'a)))
;; 
;; (srt-deftest debug:srt-if:1
;;   (:eq 'a
;;        ('b
;; 	:srt-if (t 'c))))
;; 
;; (srt-deftest debug:srt-if:2
;;   (:eq 'a
;;        ('b
;; 	:srt-if (nil 'c)
;; 	:srt-if (t 'd))))
;; 
;; (srt-deftest debug:srt-if:3
;;   (:eq 'a
;;        ('e
;; 	:srt-if (nil 'c)
;; 	:srt-if (nil 'd))))
;; 
;; (srt-deftest debug:srt-if:4
;;   (:eq 'a
;;        ('b
;; 	:srt-if (t 'c)
;; 	:srt-if (t 'd))))

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

(srt-deftest err:1
  (:srt-error 'void-function
	      (a 'a)))

(srt-deftest err:2
  (:srt-error 'error
	      (a 'a)))

(srt-deftest err:3
  (:srt-error 'arith-error
	      (/ 1 0)))

(srt-deftest err:4
  (:srt-error 'void-variable
	      (+ 1 a)))

(srt-deftest srt-if:1
  (:eq 'a
       ('b
	:srt-if (t 'a))))

(srt-deftest srt-if:2
  (:eq 'a
       ('b
	:srt-if (nil 'c)
	:srt-if (t 'a))))

(srt-deftest srt-if:3
  (:eq 'a
       ('a
	:srt-if (nil 'c)
	:srt-if (nil 'd))))

(srt-deftest srt-if:4
  (:eq 'a
       ('b
	:srt-if (t 'a)
	:srt-if (t 'b))))

(srt-deftest srt-if:1-
  (:eq ('b
	:srt-if (t 'a))
       'a))

(srt-deftest srt-if:2-
  (:eq ('b
	:srt-if (nil 'c)
	:srt-if (t 'a))
       'a))

(srt-deftest srt-if:3-
  (:eq ('a
	:srt-if (nil 'c)
	:srt-if (nil 'd))
       'a))

(srt-deftest srt-if:4-
  (:eq ('b
	:srt-if (t 'a)
	:srt-if (t 'b))
       'a))

(srt-deftest srt-if:1--
  (:eq ('b
	:srt-if (t 'a))
       ('b
	:srt-if (t 'a))))

(srt-deftest srt-if:2--
  (:eq ('b
	:srt-if (nil 'c)
	:srt-if (t 'a))
       ('b
	:srt-if (nil 'c)
	:srt-if (t 'a))))

(srt-deftest srt-if:3--
  (:eq ('a
	:srt-if (nil 'c)
	:srt-if (nil 'd))
       ('a
	:srt-if (nil 'c)
	:srt-if (nil 'd))))

(srt-deftest srt-if:4--
  (:eq ('b
	:srt-if (t 'a)
	:srt-if (t 'b))
       ('b
	:srt-if (t 'a)
	:srt-if (t 'b))))

(provide 'leaf-tests)
;;; leaf-tests.el ends here
