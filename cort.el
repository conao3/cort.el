;;; cort.el --- Simplify extended unit test framework  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Naoya Yamashita <conao3@gmail.com>

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; Keywords: test lisp
;; Version: 7.1.1
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

(defcustom cort-silence nil
  "Do test with silence.
Output just dot when success test."
  :group 'cort
  :type 'boolean)

(defvar cort-test-cases nil
  "Test list such as ((TEST-NAME VALUE)...).")


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

basic         : (:COMPFUN GIVEN EXPECT BEFOREFN AFTERFN)
error testcase: (:cort-error EXPECTED-ERROR FORM)"
  (declare (indent 1))
  (let* ((count 0)
         (testlst* (eval testlst))
         (suffixp (< 1 (length testlst*))))
    `(progn
       ,@(mapcar (lambda (test)
                   (setq count (1+ count))
                   `(add-to-list 'cort-test-cases
                                 '(,(if suffixp
                                        (make-symbol
                                         (format "%s-%s" (symbol-name name) count))
                                      name)
                                   ,@test)))
                 testlst*))))


;;; generate

(defmacro cort-generate (op form)
  "Return `cort-deftest' compare by OP for FORM."
  (declare (indent 1))
  `',(mapcar (lambda (elm)
               `(,(intern (format ":%s" op)) ,(car elm) ,(cadr elm)))
             (cadr form)))

(defmacro cort-generate--macroexpand (form)
  "Return `cort-deftest' compare by `equal' for NAME, FORM.

Example:
  (cort-deftest-with-equal leaf/disabled
    '((asdf asdf)
      (uiop uiop)))

   => (cort-deftest leaf/disabled
        '((:equal '(macroexpand-1 'asdf)
                  asdf)
          (:equal '(macroexpand-1 'uiop)
                  uiop)))"
  `',(mapcar (lambda (elm)
               `(:equal
                 (macroexpand-1 ',(car elm))
                 ',(cadr elm)))
             (cadr form)))

(defmacro cort-generate--macroexpand-let (letform form)
  "Return `cort-deftest' compare by `equal' for NAME, LETFORM FORM.

Example:
  (cort-deftest-with-macroexpand-let leaf/leaf
      ((leaf-expand-leaf-protect t))
    '(((prog1 'leaf
         (leaf-handler-leaf-protect leaf
           (leaf-init)))
       (leaf leaf
         :config (leaf-init)))))

   => (cort-deftest leaf/leaf
        '((:equal
           '(let ((leaf-expand-leaf-protect t))
             (macroexpand-1
              '(leaf leaf
                 :config (leaf-init))))
           (prog1 'leaf
              (leaf-handler-leaf-protect leaf
                (leaf-init))))))"
  `',(mapcar (lambda (elm)
               `(:equal
                 (let ,letform (macroexpand-1 ',(car elm)))
                 ',(cadr elm)))
             (cadr form)))

(defmacro cort-generate--shell-command (form)
  "Return `cort-deftest' compare with `string=' for NAME, FORM.

Example:
  (cort-deftest-with-shell-command keg/subcommand-help
    '((\"keg version\"
       \"Keg 0.0.1 running on Emacs 26.3\")
      (\"keg files\"
       \"keg-ansi.el\\nkeg-mode.el\\nkeg.el\")))

  => (cort-deftest keg/subcommand-help
       '((:string= (string-trim-right
                    (shell-command-to-string \"keg version\"))
                   \"Keg 0.0.1 running on Emacs 26.3\")
         (:string= (string-trim-right
                    (shell-command-to-string \"keg files\"))
                   \"keg-ansi.el\\nkeg-mode.el\\nkeg.el\")))"
  `',(mapcar (lambda (elm)
               `(:string=
                 (string-trim-right (shell-command-to-string ,(car elm)))
                 ,(cadr elm)))
             (cadr form)))


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
        (errorc 0)
        (dots 0))
    (dolist (test (reverse cort-test-cases)
                  (list testc failc errorc))
      (let* ((name   (nth 0 test))
             (method (nth 1 test))
             (given  (nth 2 test))
             (expect (nth 3 test))
             (beforefn (nth 4 test))
             (afterfn  (nth 5 test))
             (method-errorp (eq method :cort-error))
             err-before err-after
             err res ret exp)
        (when beforefn
          (condition-case e (funcall beforefn) (error (setq err-before e))))

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
                    (setq exp (eval expect))
                    (funcall (intern (substring (symbol-name method) 1)) exp ret)))
                (error
                 (setq err e) nil)))

        (when afterfn
          (condition-case e (funcall afterfn res err) (error (setq err-after e))))

        (cond
         (err (cl-incf errorc))
         ((not res) (cl-incf failc)))

        (if res
            (if (not cort-silence)
                (with-ansi-princ
                 (format "%s %s\n" (cyan "[PASSED]") name))
              (princ ".")
              (cl-incf dots)
              (when (<= 60 dots) (princ "\n") (setq dots 0)))
          (unless (= -1 dots) (princ "\n") (setq dots -1))

          (with-ansi-princ
           (if err
               (format "%s %s\n" (magenta "<<ERROR>>") name)
             (format "%s %s\n" (red "[FAILED]") name)))

          (if method-errorp
              (with-ansi-princ
               (format "%s\n%s\n" (yellow "Given:") (cort-pp expect))
               (format "%s   %s\n" (yellow "Expected-error:") (cort-pp given))
               (format "%s %s\n" (yellow "Unexpected-error:") (cort-pp err)))
            (with-ansi-princ
             (format "< Tested with %s >\n" (yellow (prin1-to-string method)))
             (format "%s\n%s\n" (yellow "Given:") (cort-pp given))
             (if err
                 (format "%s %s\n" (yellow "Unexpected-error:") (cort-pp err))
               (format "%s\n%s\n" (yellow "Returned:") (cort-pp ret)))
             (format "%s\n%s\n" (yellow "Expected:") (cort-pp expect))
             (when (and (not err) (executable-find "diff"))
               (let ((retfile (make-temp-file
                               "cort-returned-" nil nil
                               (format "%s\n" (cort-pp ret))))
                     (expfile (make-temp-file
                               "cort-expected-" nil nil
                               (format "%s\n" (cort-pp exp)))))
                 (unwind-protect
                     (format "%s\n%s"
                             (yellow "Diff:")
                             (with-output-to-string
                               (call-process "diff" nil standard-output nil
                                             "-u" retfile expfile)))
                   (delete-file retfile)
                   (delete-file expfile))))
             (when err-before
               (format "%s %s\n" (yellow "Error-before-hook:") (cort-pp err-before)))
             (when err-after
               (format "%s %s\n" (yellow "Error-after-hook:") (cort-pp err-after)))))

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

(defun cort-test-run-silence ()
  "Run all test with silence."
  (let ((cort-silence t))
    (cort-test-run)))

(provide 'cort)
;;; cort.el ends here
