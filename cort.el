;;; cort.el --- Simplify extended unit test framework  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Naoya Yamashita <conao3@gmail.com>

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; Keywords: test lisp
;; Version: 7.0.5
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

(require 'ansi)

(defgroup cort-test nil
  "Simplify elisp test framework."
  :group 'lisp)

(defvar cort-test-test-cases nil
  "Test list such as ((TEST-NAME VALUE) (TEST-NAME VALUE) ...).")

(defcustom cort-test-show-backtrace nil
  "If non nil, show backtrace when fail test case."
  :type 'boolean
  :group 'cort-test)


;;; functions

(defsubst cort-test-pp (sexp)
  "Return pretty printed SEXP string."
  (replace-regexp-in-string "\n+$" "" (pp-to-string sexp)))

(defun cort-test-test (test)
  "Actually execute TEST.  TEST expect (METHOD EXPECT GIVEN).
Evaluate GIVEN to check it match EXPECT.
If match, return t, otherwise return nil."
  (let ((_name  (nth 0 test))
        (method (nth 1 test))
        (given  (nth 2 test))
        (expect (nth 3 test)))
    (if (eq method :cort-error)
        (eval
         `(condition-case err
              (eval ,(nth 3 test))
            (,(nth 2 test) t)))
      (funcall (intern
                (substring (symbol-name method) 1))
               (eval given) (eval expect)))))

(defun cort-test-testpass (test)
  "Output messages for passed TEST."
  (let* ((name    (nth 0 test))
         (_method (nth 1 test))
         (_given  (nth 2 test))
         (_expect (nth 3 test)))
    (princ (with-ansi (cyan "[PASSED]") " " (format "%s" name) "\n"))))

(defun cort-test-testfail (test &optional err)
  "Output messages for failed TEST.
ERR is error message."
  (let ((name    (nth 0 test))
        (method  (nth 1 test))
        (given   (nth 2 test))
        (expect  (nth 3 test)))
    (let* ((failp           (not err))
           (errorp          (not failp))
           (method-errorp   (eq method :cort-error))
           (method-defaultp (not method-errorp)))
      (let ((mesheader) (mesmethod) (mesgiven) (mesreturned) (mesexpect)
            (meserror) (mesbacktrace))
        (setq mesgiven  (format "Given:\n%s\n" (cort-test-pp given)))
        (setq mesbacktrace (format "Backtrace:\n%s\n"
                                   (with-output-to-string
                                     (backtrace))))
        (progn
          (when errorp
            (setq mesheader (with-ansi (magenta "<<ERROR>>") " " (format "%s" name) "\n"))
            (setq meserror  (format "Unexpected-error: %s\n" (cort-test-pp err))))
          (when failp
            (setq mesheader (with-ansi (red "[FAILED]") " " (format "%s" name) "\n"))))

        (progn
          (when method-defaultp
            (setq mesmethod (format "< Tested with %s >\n" method))
            (setq mesexpect (format "Expected:\n%s\n" (cort-test-pp expect)))
            (when failp
              (setq mesreturned (format "Returned:\n%s\n" (cort-test-pp (eval given))))))
          (when method-errorp
            (setq meserror  (format "Unexpected-error: %s\n" (cort-test-pp err)))
            (setq mesexpect (format "Expected-error:   %s\n" (cort-test-pp expect)))))

        (princ (concat mesheader
                       (when mesmethod   mesmethod)
                       (when mesgiven    mesgiven)
                       (when mesreturned mesreturned)
                       (when mesexpect   mesexpect)
                       (when meserror    meserror)
                       (when cort-test-show-backtrace
                         (when mesbacktrace mesbacktrace))
                       "\n"))))))


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
                   `(add-to-list 'cort-test-test-cases
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
  (setq cort-test-test-cases nil)
  (message "prune tests completed."))

(defun cort-test-run ()
  "Run all test."
  (let ((testc  (length cort-test-test-cases))
        (failc  0)
        (errorc 0))
    (with-ansi-princ
     "\n" (yellow (format "Running %d tests..." testc)) "\n")
    (princ (format "%s\n" (emacs-version)))

    (dolist (test (reverse cort-test-test-cases))
      (condition-case err
          (if (cort-test-test test)
              (cort-test-testpass test)
            (cort-test-testfail test)
            (setq failc (1+ failc)))
        (error
         (cort-test-testfail test err)
         (setq errorc (1+ errorc)))))

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
