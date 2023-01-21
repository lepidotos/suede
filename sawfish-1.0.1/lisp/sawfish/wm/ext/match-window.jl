;; match-window.jl -- match windows to properties
;; $Id: match-window.jl,v 1.49 2001/08/24 02:43:28 jsh Exp $

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

;; Commentary:

;; This module provides a general mechanism for setting properties on
;; matched windows as they're created. It removes much of the need for
;; manually creating functions to put in before-add-window-hook.

;; For example, doing:

;;	(add-window-matcher 'WM_CLASS "Term" '(place-mode . interactive))

;; makes all terminal windows be placed interactively. Or even better:

;;	(add-window-matcher 'WM_CLASS "Netscape/Navigator"
;;			    '(ignore-program-position . t))

;; makes netscape windows a lot easier to live with.

(define-structure sawfish.wm.ext.match-window

    (export match-window-grab-x-property
	    define-match-window-group
	    define-match-window-property
	    define-match-window-setter
	    define-match-window-formatter
	    add-window-matcher
	    remove-window-matcher
	    match-window)

    (open rep
	  rep.system
	  rep.regexp
	  sawfish.wm
	  sawfish.wm.util.groups)

  (define-structure-alias match-window sawfish.wm.ext.match-window)


;;; configuration and customize stuff

  (i18n-defvar match-window-x-properties
    '((WM_NAME . "Name")
      (WM_CLASS . "Class")
      (WM_ICON_NAME . "Icon Name")
      (WM_WINDOW_ROLE . "Role")
      (WM_CLIENT_MACHINE . "Host")
      (WM_COMMAND . "Command")
      (WM_LOCALE_NAME . "Locale")))

  (i18n-defvar match-window-properties
    `((placement ,(_ "Placement")
       (avoid boolean)
       (ignore-program-position boolean)
       (place-mode ,(lambda () `(choice ,@placement-modes)))
       (position (pair number number))
       (dimensions (pair (number 1) (number 1)))
       (workspace (number 1))
       (viewport (pair (number 1) (number 1)))
       (depth (number -16 16))
       (placement-weight number)
       (maximized (choice all vertical horizontal)))
      (focus ,(_ "Focus")
       (raise-on-focus boolean)
       (focus-when-mapped boolean)
       (never-focus boolean)
       (focus-click-through boolean)
       (focus-mode ,(lambda () `(choice ,@focus-modes))))
      (appearance ,(_ "Appearance")
       (frame-type ,(lambda () `(choice ,@(mapcar car match-window-types))))
       (frame-style ,(lambda () `(symbol ,@(find-all-frame-styles t)))))
      (state ,(_ "State")
       (ignored boolean)
       (iconified boolean)
       (shaded boolean)
       (sticky boolean)
       (sticky-viewport boolean)
       (group ,(lambda ()
		 `(symbol ,@(delete-if-not symbolp (window-group-ids)))))
       (ungrouped boolean)
       (cycle-skip boolean)
       (window-list-skip boolean))
      (other ,(_ "Other")
       (unique-name boolean)
       (auto-gravity boolean)
       (shade-hover boolean)
       (transients-above (choice all parents none))
       (ignore-stacking-requests boolean))))

  ;; alist of (PROPERTY . FEATURE) mapping properties to the lisp
  ;; libraries implementing them
  (defvar match-window-features
    '((raise-on-focus . auto-raise)
      (shade-hover . shade-hover)))

  (defvar match-window-types
    '((normal . default)
      (title-only . shaped)
      (border-only . transient)
      (top-border . shaped-transient)
      (none . unframed)))

  (define (match-window-widget)
    (let ((props (mapcar
		  (lambda (sub)
		    (cons (cadr sub)
			  (mapcar (lambda (prop)
				    (if (functionp (cadr prop))
					(list* (car prop)
					       ((cadr prop))
					       (cddr prop))
				      prop))
				  (cddr sub)))) match-window-properties)))
      `(match-window ,props ,match-window-x-properties)))

  (put 'match-window 'custom-widget match-window-widget)

  ;;###autoload (defgroup match-window "Matched Windows" :layout single :require sawfish.wm.ext.match-window)
  (defgroup match-window "Matched Windows"
    :layout single
    :require sawfish.wm.ext.match-window)

  ;; List of (MATCH-ELTS . ACTION-ELTS)
  ;; Each MATCH-ELT is (PROP . REGEXP or NUMBER or SYMBOL)
  ;; Each ACTION-ELT is (PROP . VALUE)
  (defcustom match-window-profile nil
    nil
    :type match-window
    :group match-window
    :require sawfish.wm.ext.match-window)

  ;; used by sawmill-ui when grabbing property values
  (define (match-window-grab-x-property real-prop)
    (let ((window (select-window))
	  prop)
      (when window
	(setq prop (get-x-property window real-prop))
	(if (and prop (eq (car prop) 'STRING))
	    (setq prop (get-x-text-property window real-prop))
	  (setq prop (nth 2 prop)))
	(when prop
	  (cond ((get real-prop 'match-window-formatter)
		 (setq prop ((get real-prop 'match-window-formatter) prop)))
		((vectorp prop)
		 (setq prop (aref prop 0))))))
      (when (stringp prop)
	(setq prop (concat #\^ (quote-regexp prop) #\$)))
      prop))

  (define (define-match-window-group group name)
    (unless (assq group match-window-properties)
      (setq match-window-properties (nconc match-window-properties
					   (list (list group name))))))

  (define (define-match-window-property name group . def)
    (let* ((group-cell (or (assq group match-window-properties)
			   (error "Unknown match-window group: %s" group)))
	   (item-cell (assq name (cddr group-cell))))
      (if item-cell
	  (rplacd item-cell def)
	(rplacd (cdr group-cell) (nconc (cddr group-cell)
					(list (cons name def)))))))

  (define (define-match-window-setter name setter)
    (put name 'match-window-setter setter))

  (define (define-match-window-formatter name formatter)
    (put name 'match-window-formatter formatter))


;;; main entry point

  (define (add-window-matcher prop value #!rest actions)
    (catch 'out
      (let
	  ((pair (cons prop value))
	   (add-to (lambda (slot)
		     (mapc (lambda (action)
			     (let
				 ((tem (assq (car action) (cdr slot))))
			       (if tem
				   (rplacd tem (cdr action))
				 (rplacd slot (cons action (cdr slot))))))
			   actions))))
	;; does the list already contain a (PROP . VALUE) pair?
	(mapc (lambda (cell)
		(when (member pair (car cell))
		  (add-to cell)
		  (throw 'out t)))
	      match-window-profile)
	;; no
	(setq match-window-profile (cons (list (cons pair nil))
					 match-window-profile))
	(add-to (car match-window-profile)))))

  (define (remove-window-matcher prop value #!rest props)
    (let
	((pair (cons prop value))
	 (remove-from (lambda (slot)
			(mapc (lambda (p)
				(let
				    ((tem (assq p (cdr slot))))
				  (when tem
				    (rplacd slot (delq tem (cdr slot))))))
			      props))))
      (mapc (lambda (cell)
	      (when (member pair (car cell))
		(remove-from cell)))
	    match-window-profile)
      ;; remove any empty matchers
      (setq match-window-profile
	    (delete-if (lambda (cell)
			 (null (cdr cell))) match-window-profile))))


;;; matcher code

  (define (safe-string-match re . args)
    (condition-case data
	(apply string-match re args)
      (regexp-error
       (format standard-error
	       "regexp error in match-window: %s, %s" re (car data)))))

  (define (match-window w)
    (let ((prop-cache '()))

      ;; Get the X property P of window W, uses a cache, will
      ;; reformat properties with a match-window-formatter property 
      (define (get-prop p)
	(let
	    ((tem (assq p prop-cache)))
	  (if tem
	      (cdr tem)
	    (setq tem (get-x-property w p))
	    (when (and tem (eq (car tem) 'STRING))
	      (rplaca (cddr tem) (get-x-text-property w p)))
	    (when (and tem (get p 'match-window-formatter))
	      (rplaca (cddr tem) ((get p 'match-window-formatter)
				  (nth 2 tem))))
	    (setq prop-cache (cons (cons p tem) prop-cache))
	    tem)))

      ;; Return t if X property PROP of window W matches INPUT (a
      ;; regexp, number, or symbol)
      (define (match-prop prop input)
	(catch 'out
	  (cond ((and (stringp input)
		      (or (stringp prop)
			  (and (vectorp prop)
			       (> (length prop) 0)
			       (stringp (aref prop 0)))))
		 ;; regexp match
		 (if (vectorp prop)
		     (do ((i 0 (1+ i)))
			 ((= i (length prop)) nil)
		       (when (safe-string-match input (aref prop i))
			 (throw 'out t)))
		   (safe-string-match input prop)))
		((and (numberp input) (numberp prop))
		 (= input prop))
		(t (equal input prop)))))

      ;; Execute the list of actions for window W
      (define (run-actions actions)
	(mapc (lambda (cell)
		(let ((feature (cdr (assq (car cell) match-window-features))))
		  (when feature
		    (require feature)))
		((or (get (car cell) 'match-window-setter) window-put)
		 w (car cell) (cdr cell)))
	      actions)
	;; hack alert!
	(when (assq 'position actions)
	  (window-put w 'placed t)))

      (mapc (lambda (cell)
	      (when (catch 'out
		      (mapc (lambda (match)
			      (let ((prop (and (symbolp (car match))
					       (get-prop (car match)))))
				(when (or (not prop)
					  (not (match-prop (nth 2 prop)
							   (cdr match))))
				  (throw 'out nil))))
			    (car cell))
		      t)
		(run-actions (cdr cell))))
	    match-window-profile)))

  (add-hook 'before-add-window-hook match-window t)


;;; custom property formatters and setters

  (define-match-window-formatter 'WM_CLASS
   (lambda (vec)
     (format nil "%s/%s" (aref vec 1) (aref vec 0))))

  (define-match-window-formatter 'WM_COMMAND
   (lambda (vec)
     (let ((i 0)
	   parts)
       (while (< i (length vec))
	 (when parts
	   (setq parts (cons ?  parts)))
	 (setq parts (cons (aref vec i) parts))
	 (setq i (1+ i)))
       (apply concat (nreverse parts)))))

  (define-match-window-setter 'workspace
   (lambda (w prop value)
     (declare (unused prop))
     (unless (or (window-get w 'placed) (window-workspaces w))
       ;; translate from 1.. to 0..
       (set-window-workspaces w (list (1- value))))))

  (define-match-window-setter 'position
   (lambda (w prop value)
     (declare (unused prop))
     (let ((x (car value))
	   (y (cdr value)))
       (when (< x 0)
	 ;; XXX should change placement gravity
	 (setq x (+ (screen-width) x)))
       (when (< y 0)
	 ;; XXX should change placement gravity
	 (setq y (+ (screen-height) y)))
       (move-window-to w x y))))

  (define-match-window-setter 'dimensions
   (lambda (w prop value)
     (declare (unused prop))
     (resize-window-with-hints w (car value) (cdr value))))

  (define-match-window-setter 'viewport
   (lambda (w prop value)
     (declare (unused prop))
     (unless (window-get w 'placed)
       (set-screen-viewport (1- (car value)) (1- (cdr value)))
       (set-window-viewport w (1- (car value)) (1- (cdr value))))))

  (define-match-window-setter 'frame-type
   (lambda (w prop value)
     (declare (unused prop))
     (set-window-type w (or (cdr (assq value match-window-types)) value))))

  (define-match-window-setter 'ungrouped
   (lambda (w prop value)
     (declare (unused prop))
     (when value
       (add-window-to-new-group w))))

  (define-match-window-setter 'unique-name
    (lambda (w prop value)
     (declare (unused prop))
      (when value
	(uniquify-window-name w))))

  (define-match-window-setter 'focus-mode
   (lambda (w prop value)
     (declare (unused prop))
     (set-focus-mode w value)))

  (define-match-window-setter 'maximized
   (lambda (w prop value)
     (declare (unused prop))
     (when (memq value '(all vertical))
       (window-put w 'queued-vertical-maximize t))
     (when (memq value '(all horizontal))
       (window-put w 'queued-horizontal-maximize t)))))
