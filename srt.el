;;; leaf.el ---                                      -*- lexical-binding: t; -*-

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

(defvar srt-errorp nil
  "When test fail, this frag will be t.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  support functions
;;

(defun srt-testpass (name key form expect)
  (let* ((mesheader  (format "[PASSED]  %s" (symbol-name name)))
	 (message    (format "%s\n" mesheader)))
    (princ message)))

(defun srt-testfail (name key form expect)
  (let* ((mesheader (format "[ERROR]  %s\n" (symbol-name name)))
	 (meskey    (format "<tested on %s>\n" key))
	 (mesform   (format "form:\n%s\n" (pp-to-string form)))
	 (mesexpect (format "expected:\n%s\n" (pp-to-string expect)))
	 (message   (concat mesheader meskey mesform mesexpect)))
    (princ message)
    (setq et-errorp t)))

(defun srt-test (key form expect &optional special)
  "Basically, KEY is a symbol of a function name, 
but when SPECIAL is t, special process for KEY

Example:
(et-test :eq 'a 'a)
  => t"
  (if (not special)
      (let* ((funcname
	      (replace-regexp-in-string "^:+" "" (symbol-name key)))
	     (funcsym (intern funcname)))
	(funcall funcsym form expect))
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  support macros
;;

(defmacro srt-match-expansion (form expect)
  `'(:equal
       ',(macroexpand-1 form)
       ,expect))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  main macro
;;
