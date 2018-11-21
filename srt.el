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

(eval-when-compile
  (require 'cl))

(defgroup srt nil
  "Simplify elisp test framework."
  :group 'lisp)

(defconst srt-version 2.2
  "srt.el version")

(defconst srt-env-symbols '(:srt-emacs^
			    :srt-emacs=
			    :srt-emacs_
			    :srt-if)
  "Test case environment symbols.")

(defvar srt-test-cases nil
  "Test list such as ((TEST-NAME VALUE) (TEST-NAME VALUE))")

(defcustom srt-debug nil
  "If non nil, turn on debug mode.

- don't throw annoying error when test fail, just output message."
  :type 'boolean
  :group 'srt)

(defcustom srt-show-backtrace nil
  "If non nil, show backtrace when fail test case."
  :type 'boolean
  :group 'srt)

(defcustom srt-enable-color (null window-system)
  "If non nil, enable color message to output with meta character.
Default, enable color if run test on CUI.
`window-system' returns nil on CUI"
  :type 'boolean
  :group 'srt)

(defcustom srt-header-message
  (if srt-enable-color
      "\n\e[33mRunning %d tests...\e[m\n"
    "\nRunning %d tests...\n")
  "Header message"
  :type 'string
  :group 'srt)

(defcustom srt-passed-label
  (if srt-enable-color
      "\e[36m[PASSED] \e[m"
    "[PASSED] ")
  "Passed label."
  :type 'string
  :group 'srt)

(defcustom srt-fail-label
  (if srt-enable-color
      "\e[31m[FAILED] \e[m"
    "[FAILED] ")
  "Fail label."
  :type 'string
  :group 'srt)

(defcustom srt-error-label
  (if srt-enable-color
      "\e[31m<ERROR>  \e[m"
    "<<ERROR>>")
  "Fail label."
  :type 'string
  :group 'srt)

(defcustom srt-error-message
  (if srt-enable-color
      "\e[31m===== Run %d Tests, %d Expected, %d Failed, %d Errored =====\n\e[m"
    "===== Run %d Tests, %d Expected, %d Failed, %d Errored =====\n")
  "Error message"
  :type 'string
  :group 'srt)

(defcustom srt-passed-message
  (if srt-enable-color
      "\e[34m===== Run %d Tests, %d Expected, %d Failed, %d Errored =====\n\n\e[m"
    "===== Run %d Tests, %d Expected, %d Failed, %d Errored =====\n\n")
  "Error message"
  :type 'string
  :group 'srt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  for old Emacs
;;

(defmacro srt-inc (var &optional step)
  "increment VAR. If given STEP, increment VAR by STEP.
Emacs-22 doesn't support `incf'."
  (declare (indent 1) (debug t))
  `(setq ,var (+ ,var ,(if step step 1))))

;; defalias cl-symbols for old Emacs.
(when (version< emacs-version "24.0")
  (mapc (lambda (x)
	  (defalias (intern (format "cl-%s" x)) x))
	'(multiple-value-bind)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  small functions
;;

(defmacro srt-aif (test-form* then-form &rest else-form)
  "Anaphoric if macro.
This macro expansion is implemented carefully so that sexp is not 
evaluated multiple times.

\(fn (ASYM TEST-FORM) THEN-FORM [ELSE-FORM...])"
  (declare (indent 2) (debug t))
  `(let ((,(car test-form*) ,(cadr test-form*)))
     (if ,(car test-form*) ,then-form ,@else-form)))

(defmacro srt-asetq (sym* &optional body)
  "Anaphoric setq macro.

\(fn (ASYM SYM) &optional BODY)"
  (declare (indent 1))
  `(let ((,(car sym*) ,(cadr sym*)))
     (setq ,(cadr sym*) ,body)))

(defmacro srt-alet (varlist* &rest body)
  "Anaphoric let macro. Return first arg value.
CAUTION:
`it' has first var value, it is NOT updated if var value changed.

(macroexpand
 '(srt-alet (it ((result t)))
  (princ it)))
=> (let* ((result t)
          (it result))
     (progn (princ it))
     result)

\(fn (ASYM (VARLIST...)) &rest BODY)"
  (declare (debug t) (indent 1))
  `(let* (,@(cadr varlist*)
	  (,(car varlist*) ,(caar (cadr varlist*))))
     (progn ,@body)
     ,(caar (cadr varlist*))))

(defmacro srt-with-gensyms (syms &rest body)
  "Create `let' block with `gensym'ed variables.

\(fn (SYM...) &rest body)"
  (declare (indent 1))
  `(let ,(mapcar (lambda (s)
                   `(,s (gensym)))
                 syms)
     ,@body))

(defsubst srt-truep (var)
  "Return t if var is non-nil."
  (not (not var)))

(defsubst srt-pp (sexp)
  "Return pretty printed SEXP string."
  (replace-regexp-in-string "\n+$" "" (pp-to-string sexp)))

(defsubst srt-list-digest (fn list)
  "Make digest from LIST using FN (using 2 args).
Example:
(list-digest (lambda (a b) (or a b))
  '(nil nil t))
=> nil

(list-digest (lambda (a b) (or a b))
  '(nil nil t))
=> nil"
  (declare (indent 1))
  (let ((result))
    (mapc (lambda (x) (setq result (funcall fn x result))) list)
    result))

(defsubst srt-list-memq (symlist list)
  "Return t if LIST contained element of SYMLIST."
  (srt-truep
   (srt-list-digest (lambda (a b) (or a b))
     (mapcar (lambda (x) (memq x list)) symlist))))

(defsubst srt-get-funcsym (method)
  "Return function symbol from symbol such as :eq"
  (intern
   (replace-regexp-in-string "^:+" "" (symbol-name method))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  support functions
;;

(defun srt-get-value-fn (env)
  "Recursive search function for `srt-get-value'."
  (srt-aif (it (plist-get env :srt-if))
      (if (eval (car it))
  	  (cadr it)
  	(funcall #'srt-get-value-fn (member :srt-if (cddr env))))))

(defun srt-get-value (plist symbol)
  "Get reasonable value from PLIST.
Take SYMBOL value from PLIST and return the value by interpreting srt-if etc.

Example:
;; (srt-get-value
;;  '(x (:default 'a :srt-if (t 'b)))
;; 'x)
;; => 'b
;;
;; (srt-get-value
;;  '(x (:default 'a :srt-if (nil 'b)))
;;  'x)
;; => 'a"

;;   (let ((element (plist-get plist symbol))
;; 	(fn (lambda (env)
;;               (srt-aif (it (plist-get env :srt-if))
;;   		  (if (eval (car it))
;;   		      (cadr it)
;;   		    (funcall fn (member :srt-if (cddr env))))))))
;;     (srt-aif (it (funcall fn element))
;; 	it
;;       (plist-get element :default)))
  (let ((element (plist-get plist symbol)))
    (srt-aif (it (funcall #'srt-get-value-fn element))
	it
      (plist-get element :default))))

(defun srt-test (plist)
  "Actually execute GIVEN to check it matches EXPECT.
If match, return t, otherwise return nil."

  (let ((method   (srt-get-value plist :method))
	(given    (srt-get-value plist :given))
	(expect   (srt-get-value plist :expect))
	(err-type (srt-get-value plist :err-type)))
    (cond
     ((eq method :srt-error)
      (eval
       `(condition-case err
	    (eval ,given)
	  (,err-type t))))
     (t
      (let* ((funcsym (srt-get-funcsym method)))
	(funcall funcsym (eval given) (eval expect)))))))

(defun srt-testpass (name plist)
  "Output messages for test passed."

  (let ((mesheader (format "%s %s\n" srt-passed-label name)))
    (princ (concat mesheader))))

(defun srt-testfail (name plist &optional err)
  "Output messages for test failed."

  (let ((method   (srt-get-value plist :method))
	(given    (srt-get-value plist :given))
	(expect   (srt-get-value plist :expect))
	(err-type (srt-get-value plist :err-type)))
    (let* ((failp           (not err))
	   (errorp          (not failp))
	   (method-errorp   (eq method :srt-error))
	   (method-defaultp (not (or method-errorp))))
      (let ((mesheader) (mesmethod) (mesgiven) (mesreturned) (mesexpect)
	    (meserror) (mesbacktrace))
	(setq mesgiven  (format "Given:\n%s\n" (srt-pp given)))
	(setq mesbacktrace (format "Backtrace:\n%s\n"
				   (with-output-to-string
				     (backtrace))))
	(progn
	  (when errorp
	    (setq mesheader (format "%s %s\n" srt-error-label name))
	    (setq meserror  (format "Unexpected-error: %s\n" (srt-pp err))))
	  (when failp
	    (setq mesheader (format "%s %s\n" srt-fail-label name))))

	(progn
	  (when method-defaultp
	    (setq mesmethod (format "< Tested with %s >\n" method))
	    (setq mesexpect (format "Expected:\n%s\n" (srt-pp expect)))
	    (when failp
	      (setq mesreturned (format "Returned:\n%s\n" (srt-pp (eval given))))))
	  (when method-errorp
	    (setq meserror  (format "Unexpected-error: %s\n" (srt-pp err)))
	    (setq mesexpect (format "Expected-error:   %s\n" (srt-pp err-type)))))

	(princ (concat mesheader
		       (srt-aif (it mesmethod)   it)
		       (srt-aif (it mesgiven)    it)
		       (srt-aif (it meserror)    it)
		       (srt-aif (it mesreturned) it)
		       (srt-aif (it mesexpect)   it)
		       (if srt-show-backtrace
			   (srt-aif (it mesbacktrace) it))
		       "\n"
		       ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  main macro
;;

(defun srt-interpret-env-keyword (env)
  "Interpret a single keyword and return sexp.
ENV is list such as (KEYWORD VALUE)"
  (let ((symbol (car env))
	(value  (cadr env)))
    (cond
     ((eq symbol :srt-emacs^)
      (list 2 (let ((version (prin1-to-string (nth 0 value)))
		    (form    (nth 1 value)))
		`(:srt-if
		  ((version<= ,version emacs-version) ,form)))))

     ((eq symbol :srt-emacs=)
      (list 2 (let ((version (prin1-to-string (nth 0 value)))
		    (form    (nth 1 value)))
		`(:srt-if
		  ((version= ,version emacs-version) ,form)))))

     ((eq symbol :srt-emacs_)
      (list 2 (let ((version (prin1-to-string (nth 0 value)))
		    (form    (nth 1 value)))
		`(:srt-if
		  ((version<= emacs-version ,version) ,form)))))

     ((eq symbol :srt-if)
      (list 2 `(:srt-if ,value)))

     (t
      (list 1 `(:default ,symbol))))))

(defun srt-normalize-env (env)
  "Return normalize test environment list.

Example:
(srt-normalize-env :eq)
=> (:default :eq)

(srt-normalize-env '('b
		     :srt-if (t 'a)))
=> (:default 'b
    :srt-if (t 'a))
"
  (srt-alet (it ((result)))
    (if (and (listp env) (srt-list-memq srt-env-symbols env))
	(let ((i 0) (envc (length env)))
	  (while (< i envc)
	    (cl-multiple-value-bind (step value)
		(srt-interpret-env-keyword (nthcdr i env))
	      (srt-asetq (it result)
		(append it value))
	      (srt-inc i step))))
      (srt-asetq (it result)
	(append it `(:default ,env))))))

(defmacro srt-deftest (name keys)
  "Define a test case with the name A.
KEYS supported below form.

basic: (:COMPFUN FORM EXPECT)
error: (:srt-error EXPECTED-ERROR-TYPE FORM)"
  (declare (indent 1))
  (let ((fn #'srt-normalize-env))
    (cond
     ((eq (nth 0 keys) :srt-error)
      (let ((method   (funcall fn (nth 0 keys)))
	    (err-type (funcall fn (nth 1 keys)))
	    (given    (funcall fn (nth 2 keys))))
	`(add-to-list 'srt-test-cases
		      '(,name (:srt-testcase
			       :method   ,method
			       :err-type ,err-type
			       :given    ,given))
		      t)))
     (t
      (let ((method (funcall fn (nth 0 keys)))
	    (given  (funcall fn (nth 1 keys)))
	    (expect (funcall fn (nth 2 keys))))
	(if t ;; (fboundp (srt-get-funcsym (car method)))
	    `(add-to-list 'srt-test-cases
			  '(,name (:srt-testcase
				   :method ,method
				   :given  ,given
				   :expect ,expect))
			  t)
	  `(progn
	     (srt-testfail ',name (cdr
				   '(:srt-testcase
				     :method ,method
				     :given  ,given
				     :expect ,expect)))
	     (error "invalid test case"))))))))

(defun srt-prune-tests ()
  "Prune all the tests."
  (interactive)
  (setq srt-test-cases nil)
  (message "prune tests completed."))

(defun srt-run-tests ()
  "Run all the tests."
  (let ((testc  (length srt-test-cases))
	(failc  0)
	(errorc 0))
    (princ (format srt-header-message (length srt-test-cases)))
    (princ (format "%s\n" (emacs-version)))

    (dolist (test srt-test-cases)
      (let* ((name  (car  test))
	     (keys  (cadr test))
	     (plist (cdr  keys)))	; remove :srt-testcase symbol
	(condition-case err
	    (if (srt-test plist)
		(srt-testpass name plist)
	      (srt-testfail name plist)
	      (srt-inc failc))
	  (error
	   (srt-testfail name plist err)
	   (srt-inc errorc)))))

    (princ "\n\n")
    (if (or (< 0 failc) (< 0 errorc))
	(if srt-debug
	    (princ "Test failed!!\n")
	  (error (format srt-error-message
			 testc (- testc failc errorc) failc errorc)))
      (princ (format srt-passed-message
		     testc (- testc failc errorc) failc errorc)))))

(provide 'srt)
;;; srt.el ends here
