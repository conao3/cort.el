;;; cort-tests.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; Keywords: test lisp
;; Version: 3.0.0
;; URL: https://github.com/conao3/cort.el
;; Package-Requires: ((emacs "22.0"))

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
(require 'cort)

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

;; (cort-deftest debug:error
;;   (:equa 'a 'a))
;;
;; (cort-deftest debug:fail
;;   (:= (+ 1 3) 5))
;;
;; (cort-deftest debug:unexpected-error
;;   (:cort-error 'arith-error
;;            (a 'a)))
;;
;; (cort-deftest debug:cort-if:1
;;   (:eq 'a
;;        ('b
;;      :cort-if (t 'c))))
;;
;; (cort-deftest debug:cort-if:2
;;   (:eq 'a
;;        ('b
;;      :cort-if (nil 'c)
;;      :cort-if (t 'd))))
;;
;; (cort-deftest debug:cort-if:3
;;   (:eq 'a
;;        ('e
;;      :cort-if (nil 'c)
;;      :cort-if (nil 'd))))
;;
;; (cort-deftest debug:cort-if:4
;;   (:eq 'a
;;        ('b
;;      :cort-if (t 'c)
;;      :cort-if (t 'd))))

(cort-deftest simple:equal
              (:equal '(a b c) '(a b c)))

(cort-deftest simple:=
              (:= 100 100))

(cort-deftest quote-a:0
              (:eq 'a 'a))

(cort-deftest quote-a:1
              (:eq (quote-a) 'a))

(cort-deftest quote-a:2
              (:eq 'a (quote-a)))

(cort-deftest quote-a:3
              (:eq (quote-a) (quote-a)))

(cort-deftest sym:0
              (:eq 'a 'a))

(cort-deftest sym:1
              (:eq (sym a) 'a))

(cort-deftest sym:2
              (:eq 'a (sym a)))

(cort-deftest sym:3
              (:eq (sym a) (sym a)))

(cort-deftest sym:4
              (:equal (sym (a b c)) '(a b c)))

(cort-deftest sym:5
              (:equal '(a b c) (sym (a b c))))

(cort-deftest sym:6
              (:equal (sym (a b c)) (sym (a b c))))

(cort-deftest err:1
              (:cort-error 'void-function
                           (a 'a)))

(cort-deftest err:2
              (:cort-error 'error
                           (a 'a)))

(cort-deftest err:3
              (:cort-error 'arith-error
                           (/ 1 0)))

(cort-deftest err:4
              (:cort-error 'void-variable
                           (+ 1 a)))

(cort-deftest cort-if:1
              (:eq 'a
                   ('b
                    :cort-if (t 'a))))

(cort-deftest cort-if:2
              (:eq 'a
                   ('b
                    :cort-if (nil 'c)
                    :cort-if (t 'a))))

(cort-deftest cort-if:3
              (:eq 'a
                   ('a
                    :cort-if (nil 'c)
                    :cort-if (nil 'd))))

(cort-deftest cort-if:4
              (:eq 'a
                   ('b
                    :cort-if (t 'a)
                    :cort-if (t 'b))))

(cort-deftest cort-if:1-
              (:eq ('b
                    :cort-if (t 'a))
                   'a))

(cort-deftest cort-if:2-
              (:eq ('b
                    :cort-if (nil 'c)
                    :cort-if (t 'a))
                   'a))

(cort-deftest cort-if:3-
              (:eq ('a
                    :cort-if (nil 'c)
                    :cort-if (nil 'd))
                   'a))

(cort-deftest cort-if:4-
              (:eq ('b
                    :cort-if (t 'a)
                    :cort-if (t 'b))
                   'a))

(cort-deftest cort-if:1--
              (:eq ('b
                    :cort-if (t 'a))
                   ('b
                    :cort-if (t 'a))))

(cort-deftest cort-if:2--
              (:eq ('b
                    :cort-if (nil 'c)
                    :cort-if (t 'a))
                   ('b
                    :cort-if (nil 'c)
                    :cort-if (t 'a))))

(cort-deftest cort-if:3--
              (:eq ('a
                    :cort-if (nil 'c)
                    :cort-if (nil 'd))
                   ('a
                    :cort-if (nil 'c)
                    :cort-if (nil 'd))))

(cort-deftest cort-if:4--
              (:eq ('b
                    :cort-if (t 'a)
                    :cort-if (t 'b))
                   ('b
                    :cort-if (t 'a)
                    :cort-if (t 'b))))

(cort-deftest cort-emacs=:0
              (:= 10
                  (0
                   :cort-emacs> (0 10))))

(cort-deftest cort-macro:0
              (match-expansion
               (when x (princ x))
               '(if x (progn (princ x)))))

(cort-deftest cort-macro:0-
              (match-expansion-fn
               '(when x (princ x))
               '(if x (progn (princ x)))))

(provide 'leaf-tests)
;;; leaf-tests.el ends here
