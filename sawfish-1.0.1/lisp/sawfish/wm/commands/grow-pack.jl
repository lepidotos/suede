;; grow-pack.jl -- window resize and movement
;; $Id: grow-pack.jl,v 1.12 2001/03/09 20:06:05 jsh Exp $
;; Id: grow-pack.jl,v 1.9 2000/08/04 16:42:43 grossjoh Exp 

;; Copyright (C) 2000 Kai Grossjohann <Kai.Grossjohann@CS.Uni-Dortmund.DE>

;; This file is free software; you can redistribute it and/or modify it
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

;;; Commentary:

;; This package provides functions to `grow' or `pack' a window in
;; four directions.  `Growing' means to grow the window in the
;; indicated direction until it `bumps into' another window.
;; `Packing' means to move the window in the indicated direction until
;; it `bumps into' another window.

;; Copy this file into a directory which is on your load-path, then
;; use it.  I installed this package by placing the following into my
;; ~/.sawmillrc:
;;
;; (require 'grow-pack)
;; (require 'menus)
;; (setq window-ops-menu
;;       (append window-ops-menu
;;               (list (cons "Grow/pack" grow-pack-menu))))

(define-structure sawfish.wm.commands.grow-pack ()

    (open rep
	  sawfish.wm.windows
	  sawfish.wm.events
	  sawfish.wm.misc
	  sawfish.wm.util.rects
	  sawfish.wm.state.maximize
	  sawfish.wm.state.iconify
	  sawfish.wm.custom
	  sawfish.wm.commands
	  sawfish.wm.workspace
	  sawfish.wm.util.stacking)

  (define-structure-alias grow-pack sawfish.wm.commands.grow-pack)

;;; Code:

  (defcustom grow-window-repeat t
    "Whether growing an already grown window grows it again."
    :type boolean
    :group (min-max maximize))

  (defcustom grow-is-maximize t
    "Whether growing is considered to be maximization.  When you turn
this on, you can use `unmaximize-window' or something similar to get
back to the original size."
    :type boolean
    :group (min-max maximize))

  (defcustom pack-warp-pointer 'maybe
    "Whether and how to move the pointer when packing windows.

`maybe' means that the pointer is moved along with the window, if the
pointer was within the window area before packing.

`always' warps the pointer to the center of the window if it isn't
already in the window, then does like `maybe'.

`never' means not to warp the pointer."
    :type (choice maybe always never)
    :group (move))

  ;; Entry points.

  (defun grow-window-left (w)
    "Grows window to the left until it `bumps into' another window."
    (grow-window w 'left))

  (defun grow-window-right (w)
    "Grows window to the right until it `bumps into' another window."
    (grow-window w 'right))

  (defun grow-window-up (w)
    "Grows window upwards until it `bumps into' another window."
    (grow-window w 'up))

  (defun grow-window-down (w)
    "Grows window downwards until it `bumps into' another window."
    (grow-window w 'down))

  (defun pack-window-left (w)
    "Moves window to the left until it `bumps into' another window."
    (pack-window w 'left))

  (defun pack-window-right (w)
    "Moves window to the right until it `bumps into' another window."
    (pack-window w 'right))

  (defun pack-window-up (w)
    "Moves window upwards until it `bumps into' another window."
    (pack-window w 'up))

  (defun pack-window-down (w)
    "Moves window downwards until it `bumps into' another window."
    (pack-window w 'down))

  ;; Command defs

  ;;###autoload
  (define-command 'grow-window-left grow-window-left #:spec "%W")
  (define-command 'grow-window-right grow-window-right #:spec "%W")
  (define-command 'grow-window-up grow-window-up #:spec "%W")
  (define-command 'grow-window-down grow-window-down #:spec "%W")
  (define-command 'pack-window-left pack-window-left #:spec "%W")
  (define-command 'pack-window-right pack-window-right #:spec "%W")
  (define-command 'pack-window-up pack-window-up #:spec "%W")
  (define-command 'pack-window-down pack-window-down #:spec "%W")

  ;; Convenience variable.

  (defvar grow-pack-menu
    '(("Grow left" grow-window-left)
      ("Grow right" grow-window-right)
      ("Grow up" grow-window-up)
      ("Grow down" grow-window-down)
      ("Pack left" pack-window-left)
      ("Pack right" pack-window-right)
      ("Pack up" pack-window-up)
      ("Pack down" pack-window-down))
    "Menu of grow and pack operations.")

  ;; Implementation part.

  (defun gp-avoid-windows (w direction)
    "Returns list of windows to avoid when growing/filling window W in DIRECTION."
    (let* ((wpos    (window-position w))
	   (wdim    (window-frame-dimensions w))
	   (wleft   (car wpos))
	   (wtop    (cdr wpos))
	   (wright  (+ wleft (car wdim)))
	   (wbottom (+ wtop (cdr wdim)))
	   (nleft   wleft)
	   (ntop    wtop)
	   (nright  wright)
	   (nbottom wbottom)
	   (szhints (window-size-hints w))
	   (winc    (or (cdr (assq 'width-inc szhints)) 1))
	   (hinc    (or (cdr (assq 'height-inc szhints)) 1)))
      (when (eq direction 'left)
	(setq nleft 0)
	(when grow-window-repeat
	  (setq wleft (max (- wleft winc) 0))))
      (when (eq direction 'right)
	(setq nright (screen-width))
	(when grow-window-repeat
	  (setq wright (min (+ wright winc) (screen-width)))))
      (when (eq direction 'up)
	(setq ntop 0)
	(when grow-window-repeat
	  (setq wtop (max (- wtop hinc) 0))))
      (when (eq direction 'down)
	(setq nbottom (screen-height))
	(when grow-window-repeat
	  (setq wbottom (min (+ wbottom hinc) (screen-height)))))
      (filter-windows
       (lambda (x)
	 (let* ((xpos (window-position x))
		(xdim (window-frame-dimensions x))
		(xleft (car xpos))
		(xtop (cdr xpos))
		(xright (+ xleft (car xdim)))
		(xbottom (+ xtop (cdr xdim))))
	   ;; If window does not overlap W but does overlap the
	   ;; larger W, then we need to avoid this window.
	   (and (window-mapped-p x)
		(not (window-iconified-p x))
		(window-appears-in-workspace-p x current-workspace)
		(<= (rect-2d-overlap* (list xleft xtop xright xbottom)
				      (list wleft wtop wright wbottom)) 0)
		(> (rect-2d-overlap* (list xleft xtop xright xbottom)
				     (list nleft ntop nright nbottom)) 0)))))))

  (defun gp-surrounding-rect (wlist)
    "Returns the rectangle surrounding all given windows."
    (if wlist
	(let* ((w (car wlist))
	       (wrest (cdr wlist))
	       (wpos (window-position w))
	       (wdim (window-frame-dimensions w))
	       (rleft (car wpos))
	       (rtop  (cdr wpos))
	       (rright (+ rleft (car wdim)))
	       (rbottom (+ rtop (cdr wdim))))
	  (mapcar
	   (lambda (x)
	     (let* ((xpos (window-position x))
		    (xdim (window-frame-dimensions x))
		    (xleft (car xpos))
		    (xtop  (cdr xpos))
		    (xright (+ xleft (car xdim)))
		    (xbottom (+ xtop (cdr xdim))))
	       (when (< xleft rleft) (setq rleft xleft))
	       (when (< xtop  rtop) (setq rtop xtop))
	       (when (> xright rright) (setq rright xright))
	       (when (> xbottom rbottom) (setq rbottom xbottom))))
	   wrest)
	  (list rleft rtop rright rbottom))
      (list (screen-width) (screen-height) 0 0)))

  (defun grow-window (w direction)
    "Grows window W in DIRECTION."
    (let* ((avoid-wins (gp-avoid-windows w direction))
	   (surround   (gp-surrounding-rect avoid-wins))
	   (wpos       (window-position w))
	   (wdim       (window-dimensions w))
	   (fdim       (window-frame-dimensions w))
	   (wleft      (car wpos))
	   (wtop       (cdr wpos))
	   (wwidth     (car wdim))
	   (wheight    (cdr wdim))
	   (nwidth     wwidth)
	   (nheight    wheight)
	   (sleft      (nth 0 surround))
	   (stop       (nth 1 surround))
	   (sright     (nth 2 surround))
	   (sbottom    (nth 3 surround)))
      (when (eq direction 'left)
	(setq nwidth (- (+ wleft wwidth) sright)))
      (when (eq direction 'up)
	(setq nheight (- (+ wtop wheight) sbottom)))
      (when (eq direction 'right)
	(setq nwidth (- sleft wleft (- (car fdim) wwidth))))
      (when (eq direction 'down)
	(setq nheight (- stop wtop (- (cdr fdim) wheight))))
      (let
	  ((tem (cons nwidth nheight)))
	(maximize-truncate-dims w tem)	;truncate to column/row increments
	(setq nwidth (car tem))
	(setq nheight (cdr tem)))
      (when (eq direction 'left)
	(setq wleft (- wleft (- nwidth wwidth))))
      (when (eq direction 'up)
	(setq wtop (- wtop (- nheight wheight))))
      (when grow-is-maximize
	(unless (window-get w 'unmaximized-geometry)
	  (window-put w 'unmaximized-geometry (list (car wpos) (cdr wpos)
						    (car wdim) (cdr wdim))))
	(if (memq direction '(left right))
	    (window-put w 'maximized-horizontally t)
	  (window-put w 'maximzed-vertically t)))
      (move-resize-window-to w wleft wtop nwidth nheight)
      (when maximize-raises (raise-window* w))
      (when grow-is-maximize
	(call-window-hook 'window-maximized-hook w
			  (list (if (member direction '(left right))
				    'horizontal 'vertical)))
	(call-window-hook 'window-state-change-hook w (list '(maximized))))))

  (defun pack-window (w direction)
    (let* ((avoid-wins (gp-avoid-windows w direction))
	   (surround   (gp-surrounding-rect avoid-wins))
	   (wpos       (window-position w))
	   (wdim       (window-frame-dimensions w))
	   (wleft      (car wpos))
	   (wtop       (cdr wpos))
	   (wwidth     (car wdim))
	   (wheight    (cdr wdim))
	   (sleft      (nth 0 surround))
	   (stop       (nth 1 surround))
	   (sright     (nth 2 surround))
	   (sbottom    (nth 3 surround))
	   (wpointer   (query-pointer-window))
	   (xpointer   (car (query-pointer)))
	   (ypointer   (cdr (query-pointer)))
	   (xoffset    (- xpointer wleft))
	   (yoffset    (- ypointer wtop)))
      (when (eq direction 'left) (setq wleft sright))
      (when (eq direction 'up) (setq wtop sbottom))
      (when (eq direction 'right) (setq wleft (- sleft wwidth)))
      (when (eq direction 'down) (setq wtop (- stop wheight)))
      (move-window-to w wleft wtop)
      (cond ((eq pack-warp-pointer 'always)
	     (warp-cursor-to-window w))
	    ((eq pack-warp-pointer 'maybe)
	     (when (equal wpointer w)
	       (warp-cursor (+ wleft xoffset) (+ wtop yoffset)))))
      (call-window-hook 'after-move-hook w
			(list (list (if (memq direction '(left right))
					'horizontal 'vertical)))))))
;; grow-pack.jl ends here.
