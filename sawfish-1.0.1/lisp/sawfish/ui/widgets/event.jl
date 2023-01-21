#| nokogiri-widgets/event.jl

   $Id: event.jl,v 1.4 2001/02/08 04:23:15 jsh Exp $

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

(define-structure sawfish.ui.widgets.event ()

    (open rep
	  rep.regexp
	  gui.gtk
	  sawfish.gtk.widget
	  sawfish.ui.wm)

  (define (make-event-item changed)

    (let ((entry (gtk-entry-new))
	  (grab (gtk-button-new-with-label (_ "Grab...")))
	  (hbox (gtk-hbox-new nil box-spacing)))

      (gtk-container-add hbox entry)
      (gtk-box-pack-end hbox grab)
      (gtk-widget-show-all hbox)

      (gtk-signal-connect entry "changed" (make-signal-callback changed))
      (gtk-signal-connect grab "clicked"
			  (lambda ()
			    (gtk-entry-set-text entry (wm-grab-key))))

      (lambda (op)
	(case op
	  ((gtk-widget) hbox)
	  ((clear) (lambda ()
		     (gtk-entry-set-text entry "")))
	  ((set) (lambda (x)
		   (gtk-entry-set-text entry x)))
	  ((ref) (lambda ()
		   (strip-surrounding-whitespace (gtk-entry-get-text entry))))
	  ((validp) stringp)))))

  (define-widget-type 'event make-event-item)

  (define (strip-surrounding-whitespace string)
    (if (string-match "^\\s*(.*?)\\s*$" string)
	(expand-last-match "\\1")
      string)))
