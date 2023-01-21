;; anim-outline.jl -- simple window animations
;; $Id: outline.jl,v 1.7 2000/11/20 22:45:59 jsh Exp $

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

(define-structure sawfish.wm.animation.outline

    (export make-outline-animator)

    (open rep
	  sawfish.wm.misc
	  sawfish.wm.events
	  sawfish.wm.windows
	  sawfish.wm.window-anim
	  sawfish.wm.util.window-outline
	  rep.io.timers)

  (define-structure-alias anim-outline sawfish.wm.animation.outline)

  (defvar anim-outline-icon-coords (cons (screen-width) (screen-height)))

  (defvar anim-outline-steps 16)
  (defvar anim-outline-delay 20)

  (define (anim-outline-run w mode initial-coords initial-dims
			    final-coords final-dims)
    (let ((step 0)
	  (x-step (quotient (- (car final-coords) (car initial-coords))
			    anim-outline-steps))
	  (y-step (quotient (- (cdr final-coords) (cdr initial-coords))
			    anim-outline-steps))
	  (w-step (quotient (- (car final-dims) (car initial-dims))
			    anim-outline-steps))
	  (h-step (quotient (- (cdr final-dims) (cdr initial-dims))
			    anim-outline-steps))
	  (coords (cons (car initial-coords) (cdr initial-coords)))
	  (dims (cons (car initial-dims) (cdr initial-dims)))
	  timer)

      (define (clear)
	(unless (zerop step)
	  (erase-window-outline mode (car coords) (cdr coords)
				(car dims) (cdr dims))))
      
      (define (stop)
	(delete-timer timer)
	(record-window-animator w nil)
	(ungrab-server))

      (define (frame)
	(clear)
	(if (>= step anim-outline-steps)
	    (stop)
	  (rplaca coords (+ (car coords) x-step))
	  (rplacd coords (+ (cdr coords) y-step))
	  (rplaca dims (+ (car dims) w-step))
	  (rplacd dims (+ (cdr dims) h-step))
	  (draw-window-outline mode (car coords) (cdr coords)
			       (car dims) (cdr dims))
	  (setq step (1+ step))
	  (set-timer timer)))

      (define (animator win op)
	(when (eq op 'stop)
	  (clear)
	  (stop)))

      ;; kludged.. there may be Expose events waiting
      (accept-x-input)

      (grab-server)
      (record-window-animator w animator)
      (setq timer (make-timer frame nil anim-outline-delay))))

  (define (anim-outline-entry mode w op #!optional action)
    (when (eq op 'start)
      (case action
	((iconified)
	 (if (window-get w 'iconified)
	     (anim-outline-run w mode (window-position w)
			       (window-frame-dimensions w)
			       (or (window-get w 'icon-position)
				   anim-outline-icon-coords)
			       '(1 . 1)))))))

  (define (make-outline-animator mode)
    (lambda (w op #!optional action)
      (anim-outline-entry mode w op action)))

  (define wireframe-animator (make-outline-animator 'box))
  (define solid-animator (make-outline-animator 'solid))

  ;;###autoload
  (define-window-animator 'wireframe wireframe-animator)
  (define-window-animator 'solid solid-animator))
