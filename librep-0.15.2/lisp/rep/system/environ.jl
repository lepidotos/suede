#| environ.jl -- Functions to manipulate the process-environment

   $Id: environ.jl,v 1.7 2000/08/13 19:14:52 john Exp $

   Copyright (C) 1998 John Harper <john@dcs.warwick.ac.uk>

   This file is part of librep.

   librep is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   librep is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with librep; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

(declare (in-module rep.system))

(open-structures '(rep.regexp
		   rep.data))

;;;###autoload
(defun getenv (name)
  "Return the value of the environment variable NAME, a string. The variable
`process-environment' is used to find the value."
  (let ((regexp (concat (quote-regexp name) ?=)))
    (let loop ((rest process-environment))
      (cond ((null rest) nil)
	    ((string-looking-at regexp (car rest))
	     (substring (car rest) (match-end)))
	    (t (loop (cdr rest)))))))

;;;###autoload
(defun setenv (name value)
  "Set the current value of the environment variable NAME to the string VALUE.
The `process-environment' variable is destructively modified."
  (let ((regexp (concat (quote-regexp name) ?=)))
    (let loop ((rest process-environment))
      (cond ((null rest)
	     (setq process-environment (cons (concat name #\= value)
					     process-environment)))
	    ((string-looking-at regexp (car rest))
	     (rplaca rest (concat name #\= value)))
	    (t (loop (cdr rest)))))))

;;;###autoload
(defun unsetenv (name)
  "Delete the environment variable called NAME."
  (let ((re (concat (quote-regexp name) ?=)))
    (setq process-environment
	  (delete-if (lambda (x)
		       (string-looking-at re x)) process-environment))))
