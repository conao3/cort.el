;;; cort.el --- Simplify extended unit test framework  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Naoya Yamashita <conao3@gmail.com>

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; Keywords: test lisp
;; Version: 7.0.7
;; URL: https://github.com/conao3/cort.el
;; Package-Requires: ((emacs "24.4") (ansi "0.4"))

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

;; Simplify extended unit test framework.

;;; Code:

(require 'cl-lib)
(require 'ansi)

(defgroup cort nil
  "Simplify elisp test framework."
  :group 'lisp)

(defvar cort-test-cases nil
  "Test list such as ((TEST-NAME VALUE) (TEST-NAME VALUE) ...).")


;;; functions

(defsubst cort-pp (sexp)
  "Return pretty printed SEXP string."
  (if (stringp sexp)
      sexp
    (replace-regexp-in-string "\n+$" "" (pp-to-string sexp))))


;;; deftest

(defmacro cort-deftest (name testlst)
  "Define a test case with the NAME.
TESTLST is list of forms as below.

basic         : (:COMPFUN EXPECT GIVEN)
error testcase: (:cort-error EXPECTED-ERROR:ROR-TYPE FORM)"
  (declare (indent 1))
  (let ((count 0)
        (suffixp (< 1 (length (cadr testlst)))))
    `(progn
       ,@(mapcar (lambda (test)
                   (setq count (1+ count))
                   `(add-to-list 'cort-test-cases
                                 '(,(if suffixp
                                        (make-symbol
                                         (format "%s-%s" (symbol-name name) count))
                                      name)
                                   ,@test)))
                 (eval testlst)))))


;;; main

(defun cort-test-prune ()
  "Prune all test."
  (interactive)
  (setq cort-test-cases nil)
  (message "prune tests completed."))

(defun cort-test-run-1 ()
  "Actually execute test of `cort-test-cases'.
TEST expect (METHOD EXPECT GIVEN).
Evaluate GIVEN to check it match EXPECT.
Return list of (testc failc errorc)"
  (let ((testc  (length cort-test-cases))
        (failc  0)
        (errorc 0))
    (dolist (test (reverse cort-test-cases)
                  (list testc failc errorc))
      (let* ((name   (nth 0 test))
             (method (nth 1 test))
             (given  (nth 2 test))
             (expect (nth 3 test))
             (method-errorp (eq method :cort-error))
             err res ret)
        (setq res
              (condition-case e
                  (cond
                   (method-errorp
                    (eval
                     `(condition-case err
                          ,(nth 3 test)
                        (,(nth 2 test) t))))
                   (t
                    (setq ret (eval given))
                    (funcall
                     (intern (substring (symbol-name method) 1))
                     (eval expect)
                     ret)))
                (error
                 (setq err e) nil)))

        (cond
         (err (cl-incf errorc))
         ((not res) (cl-incf failc)))

        (if res
            (with-ansi-princ
             (format "%s %s\n" (cyan "[PASSED]") name))
          (with-ansi-princ
           (if err
               (format "%s %s\n" (magenta "<<ERROR>>") name)
             (format "%s %s\n" (red "[FAILED]") name)))

          (if method-errorp
              (with-ansi-princ
               (format "Given:\n%s\n" (cort-pp expect))
               (format "Expected-error:   %s\n" (cort-pp given))
               (format "Unexpected-error: %s\n" (cort-pp err)))
            (with-ansi-princ
             (format "< Tested with %s >\n" (yellow (prin1-to-string method)))
             (format "Given:\n%s\n" (cort-pp given))
             (if err
                 (format "Unexpected-error: %s\n" (cort-pp err))
               (format "Returned:\n%s\n" (cort-pp ret)))
             (format "Expected:\n%s\n" (cort-pp expect))))

          (princ "\n"))))))

(defun cort-test-run ()
  "Run all test."
  (with-ansi-princ
   "\n"
   (yellow (format "Running %d tests..." (length cort-test-cases))) "\n"
   (format "%s\n" (emacs-version)))

  (let* ((res    (cort-test-run-1))
         (testc  (nth 0 res))
         (failc  (nth 1 res))
         (errorc (nth 2 res)))

    (princ "\n\n")
    (if (or (< 0 failc) (< 0 errorc))
        (error
         (with-ansi
          (red (format "===== Run %2d Tests, %2d Expected, %2d Failed, %2d Errored on Emacs-%s ====="
                       testc (- testc failc errorc) failc errorc emacs-version))
          "\n\n"))
      (with-ansi-princ
       (blue (format "===== Run %2d Tests, %2d Expected, %2d Failed, %2d Errored on Emacs-%s ====="
                     testc (- testc failc errorc) failc errorc emacs-version))
       "\n\n"))))

(provide 'cort)
;;; cort.el ends here
