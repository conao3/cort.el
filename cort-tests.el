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

;; (cort-deftest debug:error
;;   (:equa 'a 'a))

;; (cort-deftest debug:fail
;;   (:= (+ 1 3) 5))

;; (cort-deftest debug:unexpected-error
;;   (:cort-error 'arith-error
;;            (a 'a)))

;; (cort-deftest debug:cort-if:1
;;   (:eq 'a
;;        ('b
;;      :cort-if (t 'c))))

;; (cort-deftest debug:cort-if:2
;;   (:eq 'a
;;        ('b
;;      :cort-if (nil 'c)
;;      :cort-if (t 'd))))

;; (cort-deftest debug:cort-if:3
;;   (:eq 'a
;;        ('e
;;      :cort-if (nil 'c)
;;      :cort-if (nil 'd))))

;; (cort-deftest debug:cort-if:4
;;   (:eq 'a
;;        ('b
;;      :cort-if (t 'c)
;;      :cort-if (t 'd))))

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

(cort-deftest sym
  '((:eq 'a 'a)
    (:eq (sym a) 'a)
    (:eq 'a (sym a))
    (:eq (sym a) (sym a))
    (:equal (sym (a b c)) '(a b c))
    (:equal '(a b c) (sym (a b c)))
    (:equal (sym (a b c)) (sym (a b c)))))

(cort-deftest err
  '((:cort-error 'void-function (a 'a))
    (:cort-error 'error (a 'a))
    (:cort-error 'arith-error (/ 1 0))
    (:cort-error 'void-variable (+ 1 a))))

(cort-deftest cort-if
  '((:eq 'a ('b
             :cort-if (t 'a)))
    (:eq 'a ('b
             :cort-if (nil 'c)
             :cort-if (t 'a)))
    (:eq 'a ('a
             :cort-if (nil 'c)
             :cort-if (nil 'd)))
    (:eq 'a ('b
             :cort-if (t 'a)
             :cort-if (t 'b)))))

(cort-deftest cort-if-
  '((:eq ('b
          :cort-if (t 'a))
         'a)
    (:eq ('b
          :cort-if (nil 'c)
          :cort-if (t 'a))
         'a)
    (:eq ('a
          :cort-if (nil 'c)
          :cort-if (nil 'd))
         'a)
    (:eq ('b
          :cort-if (t 'a)
          :cort-if (t 'b))
         'a)))

(cort-deftest cort-if--
  '((:eq ('b
          :cort-if (t 'a))
         ('b
          :cort-if (t 'a)))
    (:eq ('b
          :cort-if (nil 'c)
          :cort-if (t 'a))
         ('b
          :cort-if (nil 'c)
          :cort-if (t 'a)))
    (:eq ('a
          :cort-if (nil 'c)
          :cort-if (nil 'd))
         ('a
          :cort-if (nil 'c)
          :cort-if (nil 'd)))
    (:eq ('b
          :cort-if (t 'a)
          :cort-if (t 'b))
         ('b
          :cort-if (t 'a)
          :cort-if (t 'b)))))

(cort-deftest cort-emacs=:0
  '((:= 10
        (0
         :cort-emacs> (0 10)))))

;; (cort-deftest cort-macro:0
;;   ((match-expansion
;;     (when x (princ x))
;;     '(if x (progn (princ x))))))

;; (cort-deftest cort-macro:0-
;;   ((match-expansion-fn
;;     '(when x (princ x))
;;     '(if x (progn (princ x))))))

(provide 'leaf-tests)
;;; leaf-tests.el ends here
