#| nokogiri-widgets/frame-style.jl -- theme chooser widget

   $Id: frame-style.jl,v 1.7 2000/11/27 18:13:51 jsh Exp $

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

(define-structure sawfish.ui.widgets.frame-style ()

    (open rep
	  gui.gtk
	  rep.regexp
	  rep.io.files
	  sawfish.gtk.widget
	  sawfish.ui.i18n)

  (define (make-frame-style-item changed-callback doc options path)

    (let ((vbox (gtk-vbox-new nil 0))
	  (hbox (gtk-hbox-new nil 0))
	  (combo (gtk-combo-new))
	  (doc-label (gtk-label-new doc))
	  (readme-text (gtk-text-new))
	  (readme-scroller (gtk-scrolled-window-new))
	  (value (car options)))

      (gtk-box-set-spacing hbox box-spacing)
      (gtk-box-set-spacing vbox box-spacing)
      (gtk-container-add readme-scroller readme-text)
      (gtk-box-pack-start hbox doc-label)
      (gtk-box-pack-start hbox combo t t)
      (gtk-box-pack-start vbox readme-scroller t t)
      (gtk-box-pack-start vbox hbox nil nil)
      (gtk-label-set-justify doc-label 'left)
      ;;(gtk-text-set-word-wrap readme-text 1)
      (gtk-editable-set-editable readme-text nil)
      (gtk-entry-set-editable (gtk-combo-entry combo) nil)
      (gtk-scrolled-window-set-policy readme-scroller 'automatic 'automatic)

      (gtk-combo-set-popdown-strings combo (mapcar symbol-name options))
      (when value
	(gtk-entry-set-text (gtk-combo-entry combo) (symbol-name value)))

      (gtk-signal-connect (gtk-combo-entry combo) "changed"
			  (lambda ()
			    (setq value (intern (gtk-entry-get-text
						 (gtk-combo-entry combo))))
			    (update-readme value readme-text path)
			    (call-callback changed-callback)))

      (update-readme value readme-text path)
      (gtk-widget-show-all vbox)

      (lambda (op)
	(case op
	  ((gtk-widget) vbox)
	  ((clear) (lambda ()))
	  ((set) (lambda (x)
		   (gtk-entry-set-text
		    (gtk-combo-entry combo) (symbol-name x))))
	  ((ref) (lambda () value))
	  ((validp) (lambda (x) (memq x options)))))))

  (define-widget-type 'frame-style make-frame-style-item)
  (widget-accepts-doc-string 'frame-style)

  (define (gtk-text-set widget string)
    (gtk-text-set-point widget 0)
    (gtk-text-forward-delete widget (gtk-text-get-length widget))
    (gtk-text-insert widget nil nil nil string (length string))
    (gtk-text-set-point widget 0))

  (define (update-readme value text-widget theme-path)
    (catch 'out
      (let ((theme (symbol-name value)))
	(mapc (lambda (dir)
		(let ((full (expand-file-name theme dir)))
		  (when (catch 'out
			  (mapc (lambda (suf)
				  (let ((dir (format nil suf full theme)))
				    (condition-case nil
					(when (file-directory-p dir)
					  (setq full dir)
					  (throw 'out t))
				      (error))))
				'("%s" "%s.tar#tar/%s" "%s.tar.gz#tar/%s"
				  "%s.tar.Z#tar/%s" "%s.tar.bz2#tar/%s"))
			  nil)
		    (setq full (i18n-filename
				(expand-file-name "README" full)))
		    (if (file-exists-p full)
			(let ((text (make-string-output-stream))
			      (file (open-file full 'read)))
			  (unwind-protect
			      (progn
				(copy-stream file text)
				(setq text (get-output-stream-string text))
				(when (string-match "\\s+$" text)
				  (setq text (substring text 0 (match-start))))
				(gtk-text-set text-widget text))
			    (close-file file)))
		      (gtk-text-set text-widget ""))
		    (throw 'out t))))
	      theme-path)
	(gtk-text-set text-widget "")))))
