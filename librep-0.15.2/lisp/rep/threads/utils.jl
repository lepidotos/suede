;; threads.jl -- some thread utilities
;; Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

;; $Id: utils.jl,v 1.8 2000/07/23 22:24:33 john Exp $

;; This file is part of librep.

;; librep is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; librep is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with librep; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(define-structure rep.threads.utils

    (export without-interrupts)

    (open rep)

  (defmacro without-interrupts forms
    "Evaluate `(progn FORMS)' with thread preemption disabled."
    `(unwind-protect
	 (progn
	   (thread-forbid)
	   ,@forms)
       (thread-permit))))
