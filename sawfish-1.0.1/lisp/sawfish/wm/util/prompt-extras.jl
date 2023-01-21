;; prompt-extras.jl -- more esoteric prompt variants
;; $Id: prompt-extras.jl,v 1.7 2000/09/08 15:14:02 john Exp $

;; Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

;; This file is part of sawmill.

;; sawmill is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; sawmill is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with sawmill; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; Most of this is originally from Jade

(declare (in-module sawfish.wm.util.prompt))

(require 'rep.io.files)

;;; completion/validation functions

(define (prompt-complete-filename word)
  (let* ((path (file-name-directory word))
	 (file (file-name-nondirectory word))
	 (files (directory-files path)))
    (mapcar (lambda (x)
	      (when (file-directory-p (setq x (concat path x)))
		(setq x (concat x ?/)))
	      x)
	    (delete-if (lambda (f)
			 (or (not (string-head-eq f file))
			     (string-match prompt-file-exclude f)))
		       files))))

(define (prompt-validate-filename name)
  (and (file-exists-p name) name))

(define (prompt-complete-directory word)
  (setq word (expand-file-name word))
  (let ((path (file-name-directory word))
	(file (file-name-nondirectory word)))
    (delq nil
	  (mapcar (lambda (x)
		    (when (file-directory-p (concat path x))
		      (concat path x ?/)))
		  (delete-if (lambda (f)
			       (not (string-head-eq f file)))
			     (directory-files path))))))

(define (prompt-validate-directory name)
  (and (file-directory-p name) name))

(define (prompt-abbreviate-filename name)
  (let ((abbrev (file-name-nondirectory name)))
    (if (string= abbrev "")
	(file-name-as-directory
	 (file-name-nondirectory (directory-file-name name)))
      abbrev)))

(define (prompt-complete-from-list word)
  (let (out)
    (mapc (lambda (x)
	    (when (string-match (concat ?^ (quote-regexp word))
				x nil prompt-list-fold-case)
	      (setq out (cons x out)))) prompt-list)
    out))

(define (prompt-validate-from-list name)
  (if (null prompt-list-fold-case)
      (and (member name prompt-list) name)
    (catch 'exit
      (mapc (lambda (x)
	      (when (string-match (concat ?^ (quote-regexp name) ?$) x nil t)
		(throw 'exit name))) prompt-list))))

;;; entry points

(define (prompt-for-file #!optional title existing start default)
  "Prompt for a file, if EXISTING is t only files which exist are
allowed to be entered."
  (unless (stringp title)
    (setq title "Enter filename:"))
  (setq start (if (stringp start)
		  (expand-file-name start)
		(file-name-as-directory default-directory)))
  (let* ((prompt-completion-fun prompt-complete-filename)
	 (prompt-validation-fun (and existing prompt-validate-filename))
	 (prompt-abbrev-fun prompt-abbreviate-filename)
	 (str (prompt title start)))
    (when (and (string= str "") default)
      (setq str default))
    str))

(define (prompt-for-directory #!optional title existing start default)
  "Prompt for a directory, if EXISTING is t only files which exist are
allowed to be entered."
  (unless (stringp title)
    (setq title "Enter filename:"))
  (unless (stringp start)
    (setq start (file-name-as-directory default-directory)))
  (let* ((prompt-completion-fun prompt-complete-directory)
	 (prompt-validation-fun (and existing prompt-validate-directory))
	 (prompt-abbrev-fun prompt-abbreviate-filename)
	 (str (prompt title start)))
    (when (and (string= str "") default)
      (setq str default))
    str))

(define (prompt-from-list options title #!optional start dont-validate)
  "Return a selected choice from the list of options (strings) OPTIONS.
PROMPT is the title displayed, START the starting choice.
Unless DONT-VALIDATE is t, only a member of PROMPT-LIST will be returned."
  (let ((prompt-list options)
	(prompt-completion-fun prompt-complete-from-list)
	(prompt-validation-fun (if dont-validate
				   nil
				 prompt-validate-from-list)))
    (prompt title start)))

(define (prompt-for-string #!optional title start)
  (let ((prompt-completion-fun prompt-complete-filename)
	(prompt-validation-fun nil))
    (prompt (or title "Enter string: ") start)))

(define (prompt-for-number #!optional title)
  (let (num)
    (while (not (numberp num))
      (setq num (read-from-string (prompt (or title "Enter number: ")))))
    num))

(define (pwd-prompt title)
  (let ((prompt-display-fun (lambda (string)
			     (make-string (length string) ?*)))
	(prompt-history nil))
    (prompt-for-string title)))
