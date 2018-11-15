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

(defgroup srt nil
  "Simplify elisp test framework."
  :group 'lisp)

(defconst srt-version 1.2
  "srt.el version")

(defvar srt-test-cases nil
  "Test list such as ((TEST-NAME VALUE) (TEST-NAME VALUE))")

(defcustom srt-debug nil
  "If non nil, turn on debug mode.

- don't throw annoying error when test fail, just output message."
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
  (if step
      `(setq ,var (+ ,var ,step))
    `(setq ,var (+ ,var 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  small functions
;;

(defun srt-get-value (plist symbol)
  "Get reasonable value from PLIST.
Cut SYMBOL value and return the value obtained by interpreting srt-if etc."
  
  (let ((element (plist-get plist symbol)))
    (plist-get element :default)))

(defun srt-pp (sexp)
  "Return pretty printed sexp string."
  (let ((pp-string (pp-to-string sexp)))
    (replace-regexp-in-string "\n+$" "" pp-string)))

(defmacro srt-aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  support functions
;;

(defun srt-test (plist)
  "Actually execute GIVEN to check it matches EXPECT.
If match, return t, otherwise return nil."

  (let ((method   (srt-get-value plist :method))
	(given    (srt-get-value plist :given))
	(expect   (srt-get-value plist :expect))
	(err-type (srt-get-value plist :err-type)))
    (cond
     ((eq method :error)
      (eval
       `(condition-case err
	    (eval ,given)
	  (,err-type t))))
     (t
      (let* ((funcname
	      (replace-regexp-in-string "^:+" "" (symbol-name method)))
	     (funcsym (intern funcname)))
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
	   (method-errorp   (eq method :error))
	   (method-defaultp (not (or method-errorp))))
      (let ((mesheader) (meserror) (mesmethod) (mesgiven) (mesreturned) (mesexpect))
	(setq mesgiven  (format "Given:\n%s\n" (srt-pp given)))	
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
		       (srt-aif mesmethod it)
		       mesgiven
		       (srt-aif meserror it)
		       (srt-aif mesreturned it)
		       mesexpect
		       "\n"
		       ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  main macro
;;

(defmacro srt-deftest (name keys)
  "Define a test case with the name A.
KEYS supported below form.

basic: (:COMPFUN FORM EXPECT)
error: (:error EXPECTED-ERROR-TYPE FORM)"
  (declare (indent 1))
  (cond
   ((eq (nth 0 keys) :error)
    (let ((err-type (nth 1 keys))
	  (given    (nth 2 keys)))
      `(add-to-list 'srt-test-cases
		    '(,name (:srt-testcase
			     :method (:default :error)
			     :err-type (:default ,err-type)
			     :given (:default ,given)))
		    t)))
   (t
    (let ((method (nth 0 keys))
	  (given  (nth 1 keys))
	  (expect (nth 2 keys)))
      `(add-to-list 'srt-test-cases
		    '(,name (:srt-testcase
			     :method (:default ,method)
			     :given  (:default ,given)
			     :expect (:default ,expect)))
		    t)))))

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
