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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  utility functions
;;

(defmacro srt-inc (var &optional step)
  (if step
      `(setq ,var (+ ,var ,step))
    `(setq ,var (+ ,var 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  support functions
;;

(defun srt-test (keys)
  (let ((key  (nth 0 keys))
	(keyc (length keys)))
    (cond
     ((eq 3 keyc)
      (cond
       ((eq key :error)
	(let ((errtype (nth 1 keys))
	      (form    (nth 2 keys)))
	  (eval
	   `(condition-case err
		(eval ,form)
	      (,errtype t)))))
       (t
	(let ((form   (nth 1 keys))
	      (expect (nth 2 keys)))
	  (let* ((funcname
		  (replace-regexp-in-string "^:+" "" (symbol-name key)))
		 (funcsym (intern funcname)))
	    (funcall funcsym (eval form) (eval expect)))))
       (t nil))))))

(defun srt-testpass (name keys)
  (let ((mesheader (format "%s %s\n" srt-passed-label name)))
    (princ (concat mesheader))))

(defun srt-testfail (name keys &optional err)
  (let ((key (nth 0 keys))
	(keyc (length keys))
	(type) (form) (expect) (errtype))
    (cond
     ((eq 3 keyc)
      (cond
       ((eq key :error)
	(setq type    :error
	      errtype (nth 1 keys)
	      form    (nth 2 keys)))
       (t
	(setq type    :default
	      form    (nth 1 keys)
	      expect  (nth 2 keys))))))
    
    (let ((mesheader  (format "%s %s\n"
			      (if err
				  srt-error-label
				srt-fail-label)
			      name))
	  (meserr     (format "Error: %s\n" err))
	  (meskey     (format "< tested on %s >\n" key))
	  (mesform    (format "form:\n%s\n" (pp-to-string form)))
	  (mesreturn  (format "returned:\n%s\n" (pp-to-string (unless err (eval form)))))
	  (mesexpect  (format "expected:\n%s\n" (pp-to-string expect)))
	  (meserrtype (format "expected error type: %s\n" (pp-to-string errtype))))
      (princ (concat mesheader
		     (if (member type '(:default :error)) meserr)
		     (if (eq type :default) meskey)
		     (if (eq type :default) mesform)
		     (if (and (eq type :default) (not err)) mesreturn)
		     (if (eq type :default) mesexpect)
		     (if (eq type :error) meserrtype)
		     "\n"
		     )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  main macro
;;

(defmacro srt-deftest (name keys)
  (declare (indent 1))
  `(add-to-list 'srt-test-cases '(,name ,keys) t))

(defun srt-prune-tests ()
  (setq srt-test-cases nil)
  (message "prune tests completed."))

(defun srt-run-tests ()
  (let ((testc  (length srt-test-cases))
	(failc  0)
	(errorc 0))
    (princ (format srt-header-message (length srt-test-cases)))
    (princ (format "%s\n" (emacs-version)))

    (dolist (test srt-test-cases)
      (let ((name (car test))
	    (keys (cadr test)))
	(condition-case err
	    (if (srt-test keys)
		(srt-testpass name keys)
	      (srt-testfail name keys)
	      (srt-inc failc))
	  (error
	   (srt-testfail name keys err)
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
