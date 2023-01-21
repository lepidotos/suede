#| misc.jl -- miscellaneous scheme support

   $Id: misc.jl,v 1.4 2001/08/08 06:00:23 jsh Exp $

   Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

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
   along with Jade; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

(define-structure unscheme.misc

    (export procedure? apply map for-each force
	    \#make-promise call-with-current-continuation
	    call/cc dynamic-wind eval
	    scheme-report-environment null-environment
	    interaction-environment

	    call-with-input-file call-with-output-file
	    input-port? output-port? current-input-port
	    current-output-port with-input-from-file
	    with-output-to-file open-input-file
	    open-output-file close-input-port
	    close-output-port

	    read read-char peek-char eof-object?
	    write display newline write-char
	    load %load-suffixes)

    ((open rep
	   rep.io.files
	   rep.data.datums)
     (access rep.io.streams))

;;; control features

  (define procedure? functionp)

  (define (map proc . lists)
    (if (null (cdr lists))
	(mapcar proc (car lists))
      (let loop ((out nil)
		 (in lists))
	(if (car in)
	    (loop (cons (apply proc (mapcar car in)) out)
		  (mapcar cdr in))
	  (nreverse out)))))

  (define (for-each proc . lists)
    (if (null (cdr lists))
	(mapc proc (car lists))
      (let loop ((in lists))
	(when (car in)
	  (apply proc (mapcar car in))
	  (loop (mapcar cdr in))))))

  (define (force promise) (promise))

  (define (\#make-promise thunk)
    (let ((result-ready nil)
	  result)
      (lambda ()
	(unless result-ready
	  (let ((x (thunk)))
	    (unless result-ready
	      (setq result-ready t)
	      (setq result x))))
	result)))

  ;; XXX support these
  (define (scheme-report-environment version)
    (declare (unused version))
    nil)
  (define (null-environment version)
    (declare (unused version))
    nil)
  (define (interaction-environment) nil)

;;; input and output

  (define (call-with-input-file name proc)
    (let ((file (open-input-file name)))
      (prog1
	  (proc file)
	(close-file file))))

  (define (call-with-output-file name proc)
    (let ((file (open-output-file name)))
      (prog1
	  (proc file)
	(close-file file))))

  (define input-port? rep.io.streams#input-stream-p)
  (define output-port? rep.io.streams#output-stream-p)

  (define (current-input-port) standard-input)
  (define (current-output-port) standard-output)

  (define (with-input-from-file name thunk)
    (let ((standard-input (open-input-file name)))
      (prog1
	  (thunk)
	(close-file standard-input))))

  (define (with-output-to-file name thunk)
    (let ((standard-output (open-output-file name)))
      (prog1
	  (thunk)
	(close-file standard-output))))

  (define (open-input-file name) (open-file name 'read))
  (define (open-output-file name) (open-file name 'write))

  (define (close-input-port f)
    (unless (and (filep f) (null (file-binding f)))
      close-file))
  (define close-output-port close-input-port)

;;; input

  (define eof-object (make-datum nil 'scheme-eof-object))
  (define-datum-printer 'scheme-eof-object
			(lambda (x s)
			  (declare (unused x))
			  (rep.io.streams#write s "#<scheme-eof>")))

  (define (read #!optional port)
    (condition-case nil
	(rep.io.streams#read port)
      (end-of-stream eof-object)))

  (define (read-char #!optional port)
    (or (rep.io.streams#read-char (or port standard-input)) eof-object))

  (define (peek-char #!optional port)
    (or (rep.io.streams#peek-char (or port standard-input)) eof-object))

  (define eof-object? (lambda (obj) (eq obj eof-object)))

;;; output

  (define (write obj #!optional port)
    (rep.io.streams#format (or port standard-output) "%S" obj))

  (define (display obj #!optional port)
    (rep.io.streams#format (or port standard-output) "%s" obj))

  (define (newline #!optional port)
    (rep.io.streams#write (or port standard-output) #\newline))

  (define (write-char char #!optional port)
    (rep.io.streams#write (or port standard-output) char))

;;; system interface

  ;; load-file doesn't search path or add suffixes. So it fits r5rs
  (define load load-file)

  (define %load-suffixes '(".scm" . ".scmc")))
