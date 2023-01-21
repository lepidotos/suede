;; auto-raise.jl -- auto-raise on focus
;; $Id: auto-raise.jl,v 1.22 2001/01/23 02:50:36 jsh Exp $

;; Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

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

(define-structure sawfish.wm.ext.auto-raise ()

    (open rep
	  rep.system
	  rep.io.timers
	  sawfish.wm.windows
	  sawfish.wm.custom
	  sawfish.wm.util.stacking)

  (define-structure-alias auto-raise sawfish.wm.ext.auto-raise)

  (defcustom raise-windows-on-focus nil
    "Raise windows when they are focused."
    :type boolean
    :user-level novice
    :require sawfish.wm.ext.auto-raise
    :group focus)

  (defcustom raise-window-timeout 500
    "Delay in milliseconds until focused windows are raised."
    :type number
    :depends raise-windows-on-focus
    :group focus)

  (defvar disable-auto-raise nil)

  (define rw-timer nil)
  (define rw-window nil)

  (define (rw-disable-timer)
    (when rw-timer
      (setq rw-window nil)
      (delete-timer rw-timer)
      (setq rw-timer nil)))

  (define (rw-on-focus w mode)
    (when (not disable-auto-raise)
      (if (or (window-get w 'raise-on-focus) raise-windows-on-focus)
	  (progn
	    (setq rw-window w)
	    (if rw-timer
		(set-timer rw-timer)
	      (let ((timer-callback (lambda (timer)
				      (if disable-auto-raise
					  (set-timer timer)
					(setq rw-timer nil)
					(maybe-raise-window rw-window))))
		    (delay (max 1 raise-window-timeout)))
		(setq rw-timer (make-timer timer-callback
					   (quotient delay 1000)
					   (mod delay 1000))))))
	(rw-disable-timer))))

  (define (rw-out-focus w mode)
    (when (and rw-timer (eq rw-window w))
      (rw-disable-timer)))

  (add-hook 'focus-in-hook rw-on-focus)
  (add-hook 'focus-out-hook rw-out-focus))
