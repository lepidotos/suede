;; wm-spec.jl -- implement the new (GNOME/KDE) wm hints spec

;; $Id: wm-spec.jl,v 1.25 2001/09/13 16:03:25 jsh Exp $

;; Copyright (C) 1999, 2000 John Harper <john@dcs.warwick.ac.uk>

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

(define-structure sawfish.wm.state.wm-spec

    (export define-wm-spec-window-type
	    define-wm-spec-window-state)

    (open rep
	  rep.system
	  sawfish.wm.misc
	  sawfish.wm.windows
	  sawfish.wm.workspace
	  sawfish.wm.viewport
	  sawfish.wm.state.maximize
	  sawfish.wm.state.iconify)

  ;; todo:

  ;; - _NET_WORKAREA
  ;; - _NET_WM_NAME		-- needs to be in C code?
  ;; - _NET_WM_STRUT
  ;; - _NET_WM_ICON

  ;; maybe add some state extensions for things the spec doesn't
  ;; cover but existed in the old GNOME spec; e.g. _GNOME_WM_STATE_FOO
  ;; for FOO being DO_NOT_COVER, SKIP_FOCUS, ..?


;;; constants

  (defconst _NET_WM_MOVERESIZE_SIZE_TOPLEFT 0)
  (defconst _NET_WM_MOVERESIZE_SIZE_TOP 1)
  (defconst _NET_WM_MOVERESIZE_SIZE_TOPRIGHT 2)
  (defconst _NET_WM_MOVERESIZE_SIZE_RIGHT 3)
  (defconst _NET_WM_MOVERESIZE_SIZE_BOTTOMRIGHT 4)
  (defconst _NET_WM_MOVERESIZE_SIZE_BOTTOM 5)
  (defconst _NET_WM_MOVERESIZE_SIZE_BOTTOMLEFT 6)
  (defconst _NET_WM_MOVERESIZE_SIZE_LEFT 7)
  (defconst _NET_WM_MOVERESIZE_MOVE 8)

  (defconst _NET_WM_STATE_REMOVE 0)
  (defconst _NET_WM_STATE_ADD 1)
  (defconst _NET_WM_STATE_TOGGLE 2)

  (define wm-spec-window-id nil)

  (define supported-protocols
    [_NET_CLIENT_LIST _NET_CLIENT_LIST_STACKING _NET_NUMBER_OF_DESKTOPS
     _NET_DESKTOP_GEOMETRY _NET_DESKTOP_VIEWPORT _NET_CURRENT_DESKTOP
     _NET_DESKTOP_NAMES _NET_ACTIVE_WINDOW _NET_CLOSE_WINDOW
     _NET_WM_MOVERESIZE _NET_WM_DESKTOP _NET_WM_WINDOW_TYPE _NET_WM_STATE])
  
  (defconst desktop-layer -4)
  (defconst dock-layer +4)

  (define supported-states '())


;;; setting the client list hints

  (define (update-client-list-hints #!key only-stacking-list)
    (define (set-prop lst prop)
      (let loop ((rest lst)
		 (collected '()))
	(cond ((null rest)
	       (set-x-property 'root prop
			       (apply vector (nreverse collected))
			       'WINDOW 32))
	      ((window-mapped-p (car rest))
	       (loop (cdr rest) (cons (window-id (car rest)) collected)))
	      (t (loop (cdr rest) collected)))))
    (unless only-stacking-list
      (set-prop (managed-windows) '_NET_CLIENT_LIST))
    (set-prop (nreverse (stacking-order)) '_NET_CLIENT_LIST_STACKING))


;; setting the desktop / viewport hints

  (define last-workspace nil)
  (define last-workspace-count 0)
  (define last-workspace-names nil)
  (define last-area nil)
  (define last-area-count nil)

  (define (update-workspace-hints)
    (let* ((limits (workspace-limits))
	   (port (screen-viewport))
	   (port-size viewport-dimensions)
	   (total-workspaces (1+ (- (cdr limits) (car limits)))))

      (define (set-ws-hints)
	;; _NET_NUMBER_OF_DESKTOPS
	(unless (equal last-workspace-count total-workspaces)
	  (setq last-workspace-count total-workspaces)
	  (set-x-property 'root '_NET_NUMBER_OF_DESKTOPS
			  (vector total-workspaces) 'CARDINAL 32))

	;; _NET_DESKTOP_NAMES
	(unless (equal last-workspace-names workspace-names)
	  (setq last-workspace-names workspace-names)
	  (set-x-text-property 'root '_NET_DESKTOP_NAMES
			       (apply vector workspace-names)))

	;; _NET_CURRENT_DESKTOP
	(unless (equal last-workspace
		       (- current-workspace (car limits)))
	  (setq last-workspace (- current-workspace (car limits)))
	  (set-x-property 'root '_NET_CURRENT_DESKTOP
			  (vector last-workspace) 'CARDINAL 32))

	;; _NET_DESKTOP_GEOMETRY
	(unless (equal last-area-count port-size)
	  (setq last-area-count port-size)
	  (set-x-property 'root '_NET_DESKTOP_GEOMETRY
			  (vector (* (car port-size) (screen-width))
				  (* (cdr port-size) (screen-height)))
			  'CARDINAL 32))

	;; _NET_DESKTOP_VIEWPORT
	(unless (equal last-area port)
	  (let ((view (make-vector (* total-workspaces 2))))
	    (let loop ((i 0))
	      (if (= i total-workspaces)
		  (set-x-property 'root '_NET_DESKTOP_VIEWPORT
				  view 'CARDINAL 32)
		(aset view (* i 2) (* (car port) (screen-width)))
		(aset view (1+ (* i 2)) (* (cdr port) (screen-width)))
		(loop (1+ i)))))))

      (define (set-window-hints w)
	(let
	    ;; XXX the gnome-wm standard sucks..!
	    ((space (and (not (window-sticky-p/workspace w))
			 (window-get w 'swapped-in))))
	  (if space
	      (set-x-property w '_NET_WM_DESKTOP
			      (vector (- space (car limits))) 'CARDINAL 32)
	    (set-x-property w '_NET_WM_DESKTOP
			    (vector #xffffffff) 'CARDINAL 32))))
		 
      ;; apparently some pagers don't like it if we place windows
      ;; on (temporarily) non-existent workspaces
      (when (< last-workspace-count total-workspaces)
	(set-ws-hints))

      (map-windows set-window-hints)

      (when (>= last-workspace-count total-workspaces)
	(set-ws-hints))))


;;; setting the focus hints

  (define last-focus nil)

  (define (update-focus-state)
    (let ((focus (input-focus)))
      (unless (eq last-focus focus)
	(setq last-focus focus)
	(set-x-property 'root '_NET_ACTIVE_WINDOW
			(vector (if focus (window-id focus) 0)) 'WINDOW 32))))


;;; setting the window state hints

  (define (update-client-state w)
    (let ((state (filter (lambda (x) (not (supported-state-p x)))
			 (window-get w 'wm-spec-last-states))))
      (mapc (lambda (x)
	      (when (and (not (pseudo-state-p x))
			 (call-state-fun w x 'get))
		(setq state (cons x state))))
	    supported-states)
      (set-x-property w '_NET_WM_STATE (apply vector state) 'ATOM 32)
      (window-put w 'wm-spec-last-states state)))


;;; honouring the initially set window state hints

  (define (update-icon-geometry w geom)
    (when (>= (length geom) 2)
      (window-put w 'icon-position (cons (aref geom 0) (aref geom 1)))))

  (define (wm-class-hacks w)
    (let ((class (get-x-text-property w 'WM_CLASS)))
      (when (and class (>= (length class) 2))
	(cond ((or (and (string= (aref class 1) "Panel")
			(string= (aref class 0) "panel_window"))
		   (and (string= (aref class 1) "kicker")
			(string= (aref class 0) "Panel")))
	       (window-put w 'focus-click-through t)
	       (window-put w 'avoid t)
	       (window-put w 'no-history t)
	       (window-put w 'never-iconify t)
	       (window-put w 'never-maximize t)
	       (window-put w 'sticky t)
	       (window-put w 'sticky-viewport t)
	       ;; XXX see gnome.jl for why this is needed..
	       (window-put w 'placed t))
	      ((string= (aref class 1) "gmc-desktop-icon")
	       (window-put w 'never-focus t)
	       (window-put w 'never-iconify t)
	       (window-put w 'never-maximize t))))))

  (define (honour-client-state w)
    ;; things the wm-hints doesn't supply
    (wm-class-hacks w)

    (let ((space (get-x-property w '_NET_WM_DESKTOP)))
      (when space
	(setq space (aref (nth 2 space) 0))
	(cond ((equal space #xffffffff)
	       (window-put w 'sticky t))
	      ((and (integerp space) (null (window-workspaces w)))
	       (set-window-workspaces w (list space))))))

    (let ((type (get-x-property w '_NET_WM_WINDOW_TYPE)))
      (when type
	(setq type (nth 2 type))
	;; _NET_WM_WINDOW_TYPE is a vector of atoms, the first atom
	;; about which we know something is the type we'll use
	(let loop ((i 0))
	  (cond ((= i (length type)))
		((get (aref type i) 'wm-spec-type)
		 ((get (aref type i) 'wm-spec-type) w))
		(t (loop (1+ i)))))))

    (let ((state (get-x-property w '_NET_WM_STATE)))
      (when state
	(setq state (nth 2 state))
	(do ((i 0 (1+ i)))
	    ((= i (length state)))
	  (call-state-fun w (aref state i) 'init))
	(window-put w 'wm-spec-last-states (vector->list state))))

    (let ((geom (get-x-property w '_NET_WM_ICON_GEOMETRY)))
      (when geom
	(update-icon-geometry w (nth 2 geom)))))


;;; helper functions

  (define (define-wm-spec-window-type x fun) (put x 'wm-spec-type fun))

  (define (define-wm-spec-window-state x fun #!key pseudo)
    (put x 'wm-spec-state fun)
    (unless (memq x supported-states)
      (setq supported-states (cons x supported-states)))
    (when pseudo
      (put x 'wm-spec-pseudo-state t)))

  (define (supported-state-p x) (get x 'wm-spec-state))
  (define (pseudo-state-p x) (get x 'wm-spec-pseudo-state))

  (define (call-state-fun w state mode)
    (let ((fun (get state 'wm-spec-state)))
      (when fun
	(fun w mode))))

  (define-wm-spec-window-type
   '_NET_WM_WINDOW_TYPE_DESKTOP
   (lambda (w)
     (require 'sawfish.wm.stacking)
     (mark-window-as-desktop w)
     (window-put w 'fixed-position t)
     ;; I thought these would be set by the application, but KDE doesn't..
     (window-put w 'type 'unframed)
     (window-put w 'sticky t)
     (window-put w 'sticky-viewport t)
     (set-window-depth w desktop-layer)))

  (define-wm-spec-window-type
   '_NET_WM_WINDOW_TYPE_DOCK
   (lambda (w)
     (require 'sawfish.wm.stacking)
     (set-window-depth w dock-layer)
     (window-put w 'window-list-skip t)
     (window-put w 'cycle-skip t)))

  (define-wm-spec-window-type
   '_NET_WM_WINDOW_TYPE_DIALOG
   (lambda (w)
     (require 'sawfish.wm.frames)
     (set-window-type w 'transient)))

  (define-wm-spec-window-state
   '_NET_WM_STATE_STICKY
   (lambda (w mode)
     (case mode
       ((init)   (window-put w 'sticky-viewport t))
       ((remove) (make-window-unsticky/viewport w))
       ((add)    (make-window-sticky/viewport w))
       ((toggle) (if (window-sticky-p/viewport w)
		     (make-window-unsticky/viewport w)
		   (make-window-sticky/viewport w)))
       ((get)    (window-sticky-p/viewport w)))))

  (define (wm-spec-maximize-handler direction)
    (lambda (w mode)
      (require 'sawfish.wm.state.maximize)
      (case mode
	((init)
	 (window-put w (if (eq direction 'vertical)
			   'queued-vertical-maximize
			 'queued-horizontal-maximize) t))
	((remove) (unmaximize-window w direction))
	((add)    (maximize-window w direction))
	((toggle) (maximize-window-toggle w direction))
	((get)    (case direction
		    ((vertical) (window-maximized-vertically-p w))
		    ((horizontal) (window-maximized-horizontally-p w))
		    (t (window-maximized-p w)))))))

  (define-wm-spec-window-state '_NET_WM_STATE_MAXIMIZED_VERT
			       (wm-spec-maximize-handler 'vertical))
  (define-wm-spec-window-state '_NET_WM_STATE_MAXIMIZED_HORZ
			       (wm-spec-maximize-handler 'horizontal))
  (define-wm-spec-window-state '_NET_WM_STATE_MAXIMIZED
			       (wm-spec-maximize-handler nil)
			       #:pseudo t)

  (define-wm-spec-window-state
   '_NET_WM_STATE_SHADED
   (lambda (w mode)
     (require 'sawfish.wm.state.shading)
     (case mode
       ((init)   (window-put w 'shaded t))
       ((add)    (shade-window w))
       ((remove) (unshade-window w))
       ((toggle) (toggle-window-shaded w))
       ((get)    (window-get w 'shaded)))))

  (define-wm-spec-window-state
   '_NET_WM_STATE_SKIP_PAGER
   (lambda (w mode)
     (case mode
       ((init add) (window-put w 'window-list-skip t))
       ((remove)   (window-put w 'window-list-skip nil))
       ((toggle)   (window-put w 'window-list-skip
			       (not (window-get w 'window-list-skip))))
       ((get)      (window-get w 'window-list-skip)))))


;;; client messages

  (define (client-message-handler w type data)
    (let ((handled t))
      (case type
	((_NET_CLOSE_WINDOW)
	 (when (windowp w)
	   (delete-window w)))

	((_NET_WM_MOVERESIZE)
	 (when (and (windowp w) (window-mapped-p w))
	   (require 'sawfish.wm.commands.move-resize)
	   (let ((mode (aref data 2)))
	     (if (eq mode _NET_WM_MOVERESIZE_MOVE)
		 (move-window-interactively w)
	       (let ((move-resize-moving-edges
		      (cond ((eq mode _NET_WM_MOVERESIZE_SIZE_TOPLEFT) '(top left))
			    ((eq mode _NET_WM_MOVERESIZE_SIZE_TOP) '(top))
			    ((eq mode _NET_WM_MOVERESIZE_SIZE_TOPRIGHT) '(top right))
			    ((eq mode _NET_WM_MOVERESIZE_SIZE_BOTTOMLEFT) '(bottom left))
			    ((eq mode _NET_WM_MOVERESIZE_SIZE_BOTTOM) '(bottom))
			    ((eq mode _NET_WM_MOVERESIZE_SIZE_BOTTOMRIGHT) '(bottom right))
			    ((eq mode _NET_WM_MOVERESIZE_SIZE_LEFT) '(left))
			    ((eq mode _NET_WM_MOVERESIZE_SIZE_RIGHT) '(right)))))
		 (resize-window-interactively w))))))

	((_NET_NUMBER_OF_DESKTOPS _NET_DESKTOP_GEOMETRY)
	 ;; XXX these conflict with user preferences
	 )

	((_NET_DESKTOP_VIEWPORT)
	 (set-viewport (aref data 0) (aref data 1)))

	((_NET_CURRENT_DESKTOP)
	 (select-workspace (workspace-id-from-logical (aref data 0))))

	((_NET_DESKTOP_NAMES)
	 (setq data (aref data 0))
	 (let loop ((i 0)
		    (out '()))
	   (if (= i (length data))
	       (setq workspace-names (nreverse out))
	     (loop (1+ i) (cons (aref data i) out)))))

	((_NET_ACTIVE_WINDOW)
	 (require 'sawfish.wm.util.display-window)
	 (when (and (windowp w) (window-mapped-p w))
	   (display-window w)))

	((_NET_WM_STATE)
	 (when (windowp w)
	   (let ((mode (cond ((eql (aref data 0) _NET_WM_STATE_REMOVE) 'remove)
			     ((eql (aref data 0) _NET_WM_STATE_ADD) 'add)
			     ((eql (aref data 0) _NET_WM_STATE_TOGGLE) 'toggle)))
		 (atom1 (x-atom-name (aref data 1)))
		 (atom2 (x-atom-name (aref data 2))))
	     (when (or (and (eq atom1 '_NET_WM_STATE_MAXIMIZED_VERT)
			    (eq atom2 '_NET_WM_STATE_MAXIMIZED_HORZ))
		       (and (eq atom2 '_NET_WM_STATE_MAXIMIZED_VERT)
			    (eq atom1 '_NET_WM_STATE_MAXIMIZED_HORZ)))
	       (setq atom1 '_NET_WM_STATE_MAXIMIZED)
	       (setq atom2 nil))
	     (when atom1
	       (call-state-fun w atom1 mode))
	     (when atom2
	       (call-state-fun w atom2 mode)))))

	((_NET_WM_DESKTOP)
	 (when (windowp w)
	   (let ((desktop (aref data 0)))
	     (if (eql desktop #xffffffff)
		 ;; making window sticky
		 (make-window-sticky/workspace w)
	       ;; changing the desktop
	       (make-window-unsticky/workspace w)
	       (send-window-to-workspace-from-first w desktop)))))

	(t (setq handled nil)))
      handled))


;;; property changes

  (define (property-change-handler w prop kind)
    (declare (unused kind))
    (case prop
      ((_NET_WM_ICON_GEOMETRY)
       (let ((geom (get-x-property w '_NET_WM_ICON_GEOMETRY)))
	 (when geom
	   (update-icon-geometry w (nth 2 geom)))))))


;;; utilities

  (define (vector->list vec)
    (do ((i 0 (1+ i))
	 (out '() (cons (aref vec i) out)))
	((= i (length vec)) (nreverse out))))


;;; initialisation

  (define (init)
    (setq wm-spec-window-id (create-window 'root -200 -200 5 5))

    (set-x-property 'root '_NET_SUPPORTING_WM_CHECK
		    (vector wm-spec-window-id) 'WINDOW 32)
    (set-x-property wm-spec-window-id '_NET_SUPPORTING_WM_CHECK
		    (vector wm-spec-window-id) 'WINDOW 32)
    (set-x-property wm-spec-window-id '_NET_WM_NAME "Sawfish" 'STRING 8)

    (set-x-property 'root '_NET_SUPPORTED supported-protocols 'ATOM 32)

    (let ((current-desktop (get-x-property 'root '_NET_CURRENT_DESKTOP)))
      (when (and current-desktop
		 (eq (car current-desktop) 'CARDINAL)
		 (>= (length (caddr current-desktop)) 1))
	(add-hook 'after-initialization-hook
		  ;; Don't do this yet, it can screw things up
		  (lambda ()
		    (select-workspace-from-first
		     (aref (caddr current-desktop) 0))))))

    (update-client-list-hints)
    (update-workspace-hints)

    (add-hook 'workspace-state-change-hook update-workspace-hints)
    (add-hook 'viewport-resized-hook update-workspace-hints)
    (add-hook 'viewport-moved-hook update-workspace-hints)

    (add-hook 'add-window-hook update-client-list-hints)
    (add-hook 'destroy-notify-hook update-client-list-hints)
    (add-hook 'map-notify-hook update-client-list-hints)
    (add-hook 'unmap-notify-hook update-client-list-hints)
    (add-hook 'after-restacking-hook update-client-list-hints)

    (add-hook 'before-add-window-hook honour-client-state)
    (add-hook 'add-window-hook update-client-state)
    (call-after-state-changed '(sticky shaded maximized stacking)
			      update-client-state)

    (add-hook 'focus-in-hook update-focus-state)
    (add-hook 'focus-out-hook update-focus-state)

    (add-hook 'client-message-hook client-message-handler)
    (add-hook 'property-notify-hook property-change-handler)

    (add-hook 'before-exit-hook exit)

    (map-windows update-client-state))

  (define (exit)
    (destroy-window wm-spec-window-id)
    (delete-x-property 'root '_NET_SUPPORTING_WM_CHECK)
    (delete-x-property 'root '_NET_PROTOCOLS)
    (delete-x-property 'root '_NET_DESKTOP_GEOMETRY)
    (delete-x-property 'root '_NET_DESKTOP_VIEWPORT))

  (unless (or wm-spec-window-id batch-mode)
    (init)))
