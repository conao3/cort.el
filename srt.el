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

(defvar srt-version 0.6
  "srt.el version")

(defvar srt-test-cases nil
  "Test list such as ((TEST-NAME VALUE) (TEST-NAME VALUE))")

(defvar srt-debug nil
  "If non nil, turn on debug mode.

- don't throw annoying error when test fail, just output message.")

(defvar srt-enable-color (null window-system)
  "If non nil, enable color message to output with meta character.
Default, enable color if run test on CUI.
`window-system' returns nil on CUI")

(defvar srt-header-message
  (if srt-enable-color
      "\n\e[33mRunning %d tests...\e[m\n"
    "\nRunning %d tests...\n")
  "Header message")

(defvar srt-passed-label
  (if srt-enable-color
      "\e[36m[PASSED] \e[m"
    "[PASSED] ")
  "Passed label.")

(defvar srt-fail-label
  (if srt-enable-color
      "\e[31m[FAILED] \e[m"
    "[FAILED] ")
  "Fail label.")

(defvar srt-error-label
  (if srt-enable-color
      "\e[31m<ERROR>  \e[m"
    "<<ERROR>>")
  "Fail label.")

(defvar srt-error-message
  (if srt-enable-color
      "\e[31m===== Failed test(s) =====\e[m"
    "===== Failed test(s) =====")
  "Error message")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  for old Emacs
;;

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
	      (,errtype t)
	      (error nil)))))
       (t
	(let (
	      (form   (nth 1 keys))
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
	  (mesexpect  (format "expected:\n%s\n" (pp-to-string expect)))
	  (meserrtype (format "expected error type: %s\n" (pp-to-string errtype))))
      (princ (concat mesheader
		     (if (member type '(:default :error)) meserr)
		     (if (eq type :default) meskey)
		     (if (eq type :default) mesform)
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
  (let ((errorp nil))
    (princ (format srt-header-message (length srt-test-cases)))
    (princ (format "%s\n" (emacs-version)))

    (dolist (test srt-test-cases)
      (let ((name (car test))
	    (keys (cadr test)))
	(condition-case err
	    (if (srt-test keys)
		(srt-testpass name keys)
	      (srt-testfail name keys)
	      (setq errorp t))
	  (error
	   (srt-testfail name keys err)
	   (setq errorp t)))))

    (princ "\n\n")
    (when errorp
      (if srt-debug
	  (princ "Test failed!!\n")
	(error srt-error-message)))))

(provide 'srt)
;;; srt.el ends here
