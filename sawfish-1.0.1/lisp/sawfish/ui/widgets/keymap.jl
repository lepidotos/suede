#| nokogiri-widgets/keymap.jl

   $Id: keymap.jl,v 1.11 2001/04/20 21:39:56 jsh Exp $

   Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

   This file is part of sawfish.

   sawfish is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   sawfish is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with sawfish; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

(define-structure sawfish.ui.widgets.keymap ()

    (open rep
	  gui.gtk
	  rep.regexp
	  sawfish.gtk.stock
	  sawfish.gtk.widget)

;;; widget for representing keymaps

  (define (command-name command)
    (or (car command) command))

  (define (make-keymap-item changed-callback)

    (define (print x)
      (let ((command (car x)))
	(list (cdr x)
	      (if (consp command)
		  (concat (beautify-symbol-name (command-name command))
			  ": "
			  (mapconcat (lambda (x) (format nil "%s" x))
				     (cdr command) ", "))
		(beautify-symbol-name command)))))

    (define (dialog title callback #!key for value)
      (let ((widget (make-widget `(keymap:binding))))
	(when value
	  (widget-set widget value))
	(simple-dialog (_ "Edit binding") (widget-gtk-widget widget)
		       (lambda () (callback (widget-ref widget)))
		       for)))

    (define (validp x) (and (consp x) (symbolp (car x)) (stringp (cdr x))))

    (define (type op)
      (case op
	((print) print)
	((dialog) dialog)
	((validp) validp)))

    (let ((base (make-widget `(list ,type (,(_ "Key") ,(_ "Command")))
			     changed-callback)))
      ;; mold this to accept (keymap . LIST)
      (lambda (op)
	(case op
	  ((ref) (lambda ()
		   (cons 'keymap (widget-ref base))))
	  ((set) (lambda (x)
		   (widget-set base (cdr x))))
	  ((validp) (lambda (x)
		      (and (eq (car x) 'keymap)
			   (widget-valid-p base (cdr x)))))
	  (t (base op))))))

  (define-widget-type 'keymap make-keymap-item)

;;; widget for editing individual bindings

  (define (make-keymap:binding-item changed-callback)
    (make-widget `(pair command (labelled ,(_ "Key:") event) t t)
		 changed-callback))

  (define-widget-type 'keymap:binding make-keymap:binding-item)

;;; utils

  (define (beautify-symbol-name symbol)
    (cond ((stringp symbol) symbol)
	  ((not (symbolp symbol)) (format "%s" symbol))
	  (t
	   (let ((name (copy-sequence (symbol-name symbol))))
	     (while (string-match "[-:]" name)
	       (setq name (concat (substring name 0 (match-start))
				  ?  (substring name (match-end)))))
	     (aset name 0 (char-upcase (aref name 0)))
	     (_ name))))))
