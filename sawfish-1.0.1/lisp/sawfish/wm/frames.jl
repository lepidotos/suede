;; frames.jl -- handle window framing
;; $Id: frames.jl,v 1.84 2001/04/13 00:38:59 jsh Exp $

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

(define-structure sawfish.wm.frames

    (compound-interface
     (structure-interface sawfish.wm.frames.subrs)
     (export after-setting-frame-option
	     define-frame-type-mapper
	     add-frame-style
	     reload-frame-style
	     reframe-window
	     reframe-all-windows
	     rebuild-frames-with-style
	     reframe-windows-with-style
	     set-frame-style
	     apply-frame-style
	     apply-frame-style-and-save
	     mark-frame-style-editable
	     frame-style-editable-p
	     window-type
	     set-window-type
	     window-type-remove-title
	     window-type-remove-border
	     window-type-add-title
	     window-type-add-border
	     find-all-frame-styles
	     frame-style-menu
	     frame-type-menu
	     remove-frame-class
	     add-frame-class
	     frame-class-removed-p
	     set-frame-part-value
	     def-frame-class
	     define-frame-class))
	    
    (open rep
	  rep.system
	  rep.regexp
	  rep.io.files
	  rep.io.timers
	  sawfish.wm.frames.subrs
	  sawfish.wm.windows.subrs
	  sawfish.wm.misc
	  sawfish.wm.custom
	  sawfish.wm.commands
	  sawfish.wm.events
	  sawfish.wm.gaol
	  sawfish.wm.session.init
	  sawfish.wm.workspace
	  sawfish.wm.state.maximize)

  ;; Commentary:

  ;; This sets the following window properties:

  ;;	frame-style		If set, the _user-chosen_ frame style
  ;;	current-frame-style	The current frame style
  ;;	type			The notional window type, defining
  ;;				 which parts of the frame are included

  ;; The different window types are:

  ;;	default			title bar and border
  ;;	transient		border only
  ;;	shaped			title-bar only
  ;;	shaped-transient	border-like title-bar only
  ;;	unframed		no frame at all
  ;;	icon
  ;;	dock

  ;; There is also a similar concept of frame types. The window type
  ;; never changes (unless the user explicitly does so). But the frame
  ;; type may depend on the current window state. For example, if it's
  ;; shaded, then it may have `shaded' or `shaded-transient' frame type.

  ;; These are the types that the frame styles see. Current types include
  ;; all the above window types plus:

  ;;	shaded			normal title bar only
  ;;	shaded-transient	transient title bar only

  ;; When framing windows the frame-type-fallback-alist variable will be
  ;; used to iterate frame types until a type that the theme implements
  ;; is found.

  ;; However, this separation of frame and window type shouldn't
  ;; discourage themes from detecting other window state. For example,
  ;; themes may want to display maximized windows differently from
  ;; non-maximized. (In fact the frame-type != window-type scheme is
  ;; largely just for compatibility)


;;; custom support

  (define (custom-make-frame-style-widget)
    `(frame-style ,(find-all-frame-styles t) ,theme-load-path))

  (put 'frame-style 'custom-widget custom-make-frame-style-widget)


;;; variables etc

  (defvar frame-part-classes nil
    "Alist of (CLASS . ALIST) associating classes of frame parts with state
they inherit.")

  (defvar override-frame-part-classes nil
    "Alist of (CLASS . ALIST) associating classes of frame parts with state
that overrides settings set elsewhere.")

  (defvar nil-frame nil
    "Frame definition used for unframed windows.")

  (defcustom default-frame-style nil
    "Default frame style:"
    :type frame-style
    :widget-flags (expand-vertically)
    :user-level novice
    :group appearance
    :after-set (lambda () (after-setting-default-frame)))

  (defcustom reload-themes-when-changed t
    "Automatically reload themes when they are updated."
    :type boolean
    :user-level expert
    :group misc)

  (defcustom frame-type-fallback-alist
    '((transient . default)
      (shaped . default)
      (shaped-transient . shaped)
      (shaded . shaped)
      (shaded-transient . shaped-transient)
      (icon . shaped-transient)
      (dock . icon))
    "Frame type fallbacks:"
    :tooltip "Associate frame types with type to try if the theme doesn't \
implement the requested type."
    :type (alist ((symbol default shaped transient
			  shaped-transient icon doc) "From")
		 ((symbol default shaped transient
			  shaped-transient icon doc) "To"))
    :widget-flags (expand-vertically framed)
    :group appearance
    :user-level expert
    :after-set (lambda () (after-setting-frame-option)))

  (defvar theme-update-interval 60
    "Number of seconds between checking if theme files have been modified.")

  (defvar user-theme-directory "~/.sawfish/themes"
    "Directory containing user-local themes.")

  (defvar system-theme-directory (expand-file-name
				  "../themes" sawfish-lisp-lib-directory)
    "Directory containing themes from the current sawfish version.")

  (defvar site-theme-directory (expand-file-name
				"../../themes" sawfish-lisp-lib-directory)
    "Directory containing system-wide themes.")

  (defvar theme-load-path (list user-theme-directory
				site-theme-directory
				system-theme-directory)
    "List of directories from which themes may be loaded.")

  (define frame-styles nil
    "List of (NAME . FUNCTION) defining all loaded frame styles.")

  ;; List of (NAME FILENAME MODTIME) mapping loaded frame styles to the
  ;; files they were loaded from; used to check if the theme needs reloading
  (define frame-style-files nil)

  ;; List of styles that can be edited using sawfish-themer
  (define editable-frame-styles nil)

  (define frame-type-mappers '()
    "List of functions that map (WINDOW FRAME-TYPE) -> FRAME-TYPE. Used when
deciding which frame type to ask a theme to generate.")

  ;; list of (REGEXP DIR-EXPAND NAME-EXPAND)
  (defvar theme-suffix-regexps
    '(("^(.*)/(.*)\\.tar(\\.gz|\\.Z|\\.bz2|)$" "\\0#tar/\\2" "\\2")))

  (defvar theme-suffixes '("" ".tar" ".tar.gz" ".tar.Z" ".tar.bz2"))

  (defvar themes-are-gaolled t
    "When non-nil themes are assumed to be malicious.")

  (defvar sawfish-themer-program "sawfish-themer")


;;; defcustom's for some built-in variables

  (defcustom default-font nil
    "Default font: \\w"
    :group appearance
    :type font
    :widget-flags (expand-horizontally)
    :user-level novice
    :after-set (lambda () (after-setting-frame-option)))

  (defcustom default-bevel-percent nil
    "Bevel intensity: \\wpercent."
    :group appearance
    :type (number 0 100)
    :user-level expert
    :after-set (lambda () (after-setting-frame-option)))


;;; managing frame types

  (define (define-frame-type-mapper fun)
    (unless (memq fun frame-type-mappers)
      (setq frame-type-mappers (cons fun frame-type-mappers))))

  (define (find-frame-definition w style)
    ;; 1. map window type to actual frame type
    (let loop-1 ((rest frame-type-mappers)
		 (type (window-type w)))
      (if (null rest)
	  ;; found the final frame type, so,
	  ;; 2. find the closest type that the style implements to this
	  (let loop-2 ((type type)
		       (seen (list type)))
	    (cond ((eq type 'unframed) nil-frame)
		  ((style w type))
		  (t (let ((next (or (cdr (assq type frame-type-fallback-alist))
				     'unframed)))
		       (if (memq next seen)
			   ;; been here before..
			   nil-frame
			 (loop-2 next (cons next seen)))))))
	;; else, apply this transformation and keep looping
	(loop-1 (cdr rest) ((car rest) w type)))))


;;; managing frame styles

  (define (add-frame-style name function)
    (let ((cell (assq name frame-styles)))
      (if cell
	  (rplacd cell function)
	(setq frame-styles (cons (cons name function) frame-styles)))
      (when load-filename
	(let
	    ;; if we're loading from a tar-file, then check the
	    ;; tar file itself, not its contents (for efficiency)
	    ((file (if (string-match "#tar/" load-filename)
		       (substring load-filename 0 (match-start))
		     load-filename)))
	  (setq cell (assq name frame-style-files))
	  (if cell
	      (rplacd cell (list file (file-modtime file)))
	    (setq frame-style-files (cons (list name file (file-modtime file))
					  frame-style-files)))))
      (unless default-frame-style
	(setq default-frame-style name))))

  (define (check-frame-availability name)
    (unless (assq name frame-styles)
      (load-frame-style name)
      (or (assq name frame-styles) (error "No such frame style: %s" name))))

  (define (reload-frame-style name)
    (when (assq name frame-styles)
      (load-frame-style name)
      (reframe-windows-with-style name)))

  ;; called periodically from a timer
  (define (frames-on-idle timer)
    (set-timer timer theme-update-interval)
    (when reload-themes-when-changed
      (mapc (lambda (cell)
	      (let ((style (nth 0 cell))
		    (file (nth 1 cell))
		    (modtime (nth 2 cell)))
		(when (time-later-p (file-modtime file) modtime)
		  (reload-frame-style style))))
	    frame-style-files)))


;;; applying frame styles to windows

  (define (reframe-window w)
    (if (window-get w 'ignored)
	(progn
	  (window-put w 'current-frame-style nil)
	  (set-window-frame w nil-frame))
      (let ((style (or (window-get w 'frame-style)
		       default-frame-style)))
	(check-frame-availability style)
	(let ((style-fun (cdr (assq style frame-styles))))
	  (set-window-frame w (if style-fun
				  (find-frame-definition w style-fun)
				nil-frame))
	  (window-put w 'current-frame-style style)))))

  (define (reframe-all-windows) (map-windows reframe-window))

  (define after-setting-frame-option reframe-all-windows)

  (define (after-setting-default-frame)
    (check-frame-availability default-frame-style)
    (after-setting-frame-option))

  (define (rebuild-frames-with-style style)
    (map-windows (lambda (w)
		   (when (eq (window-get w 'current-frame-style) style)
		     (rebuild-frame w)))))

  (define (reframe-windows-with-style style)
    (map-windows (lambda (w)
		   (when (eq (window-get w 'current-frame-style) style)
		     (reframe-window w)))))

  (define (set-frame-style w style)
    (unless (eq (window-get w 'frame-style) style)
      (window-put w 'frame-style style)
      (call-window-hook 'window-state-change-hook w (list '(frame-style)))
      (reframe-window w)))

  ;; return true iff successful
  (define (apply-frame-style style)
    (let ((old-style default-frame-style))
      (condition-case nil
	  (progn
	    (setq default-frame-style style)
	    (after-setting-default-frame)
	    t)
	(error
	 (setq default-frame-style old-style)
	 (after-setting-frame-option)
	 nil))))

  (define (save-current-frame-style)
    (require 'sawfish.wm.customize)
    (customize-set 'default-frame-style default-frame-style))

  (define (apply-frame-style-and-save style)
    (when (apply-frame-style style)
      (save-current-frame-style)))


;;; editable frame styles

  (define (mark-frame-style-editable style)
    (unless (memq style editable-frame-styles)
      (setq editable-frame-styles (cons style editable-frame-styles))))

  (define (frame-style-editable-p style) (memq style editable-frame-styles))

  (define-command 'edit-frame-style
    (lambda (style)
      (if (not (memq style editable-frame-styles))
	  (error "Frame style isn't editable")
	(let ((dir (find-frame-style style)))
	  (when dir
	    (system (format nil "%s %s &" sawfish-themer-program dir))))))
    #:spec (lambda () (list default-frame-style)))


;;; kludge different window decors by modifying the assumed window type

  (define (window-type w)
    (or (window-get w 'type)
	(if (window-transient-p w)
	    (if (window-shaped-p w)
		'shaped-transient
	      'transient)
	  (if (window-shaped-p w)
	      'shaped
	    'default))))

  (define (set-window-type w type)
    (unless (eq (window-get w 'type) type)
      (window-put w 'type type)
      (call-window-hook 'window-state-change-hook w (list '(type)))
      (reframe-window w)))

  (define (window-type-remove-title type)
    (case type
      ((default) 'transient)
      ((shaped shaped-transient) 'unframed)
      (t type)))

  (define (window-type-remove-border type)
    (case type
      ((default) 'shaped)
      ((transient shaped-transient) 'unframed)
      (t type)))

  (define (window-type-add-title type)
    (case type
      ((transient) 'default)
      ((unframed) 'shaped)
      (t type)))

  (define (window-type-add-border type)
    (case type
      ((shaped) 'default)
      ((unframed) 'transient)
      (t type)))

  ;; create some commands for setting the window type
  (mapc (lambda (type)
	  (define-command (intern (concat "set-frame:" (symbol-name type)))
	    (lambda (w) (set-window-type w type)) #:spec "%W"))
	'(default transient shaped shaped-transient unframed))


;;; loading ``themes'' (currently just frame styles)

  (define (frame-style-directory dir #!optional get-name)
    (if (and (file-directory-p dir)
	     (or (file-exists-p (expand-file-name "theme.jl" dir))
		 (file-exists-p (expand-file-name "theme.jlc" dir))))
	(if get-name
	    (file-name-nondirectory dir)
	  dir)
      ;; try the list of suffixes
      (catch 'out
	(mapc (lambda (cell)
		(when (string-match (car cell) dir)
		  (let ((full (expand-last-match (nth 1 cell))))
		    (when (file-directory-p full)
		      (throw 'out (if get-name
				      (expand-last-match (nth 2 cell))
				    full))))))
	      theme-suffix-regexps)
	nil)))

  (define (find-frame-style name)
    (catch 'out
      (mapc (lambda (dir)
	      (mapc (lambda (suf)
		      (let* ((t-dir (expand-file-name
				     (concat (symbol-name name) suf) dir))
			     tem)
			(when (file-exists-p t-dir)
			  (setq tem (frame-style-directory t-dir))
			  (when tem
			    (throw 'out tem)))))
		    theme-suffixes))
	    theme-load-path)
      nil))

  (define (load-frame-style name)
    (let ((dir (find-frame-style name)))
      (when dir
	(let ((image-load-path (cons dir image-load-path)))
	  (if themes-are-gaolled
	      (let ((gaol (make-gaol)))
		(gaol-load (expand-file-name "theme.jl" dir) gaol)
		(define-gaol-structure (intern (concat "themes."
						       (symbol-name name)))
				       gaol))
	    (load (expand-file-name "theme" dir) nil t))))))

  (define (find-all-frame-styles #!optional sorted)
    (let (lst tem)
      (mapc (lambda (dir)
	      (when (file-directory-p dir)
		(mapc (lambda (t-dir)
			(when (setq tem (frame-style-directory
					 (expand-file-name t-dir dir) t))
			  (unless (member tem lst)
			    (setq lst (cons tem lst)))))
		      (directory-files dir))))
	    theme-load-path)
      (when sorted
	(setq lst (sort lst string-lessp)))
      (mapcar intern lst)))

  (define (frame-style-menu w)
    (let ((styles (find-all-frame-styles t)))
      (nconc (mapcar (lambda (s)
		       (list (quote-menu-item (symbol-name s))
			     (lambda ()
			       (set-frame-style (current-event-window) s))
			     (cons 'check (eq (window-get w 'frame-style) s))
			     '(group . menu-style)))
		     styles)
	     (list '())
	     (list (list (_ "Default")
			 (lambda ()
			   (set-frame-style (current-event-window) nil))
			 (cons 'check (not (window-get w 'frame-style)))
			 '(group . menu-style))))))

  (define (frame-type-menu w)
    `((,(_ "Normal") set-frame:default
       (check . ,(eq (window-type w) 'default))
       (group . menu-type))
      (,(_ "Title-only") set-frame:shaped
       (check . ,(eq (window-type w) 'shaped))
       (group . menu-type))
      (,(_ "Border-only") set-frame:transient
       (check . ,(eq (window-type w) 'transient))
       (group . menu-type))
      (,(_ "Top-border") set-frame:shaped-transient
       (check . ,(eq (window-type w) 'shaped-transient))
       (group . menu-type))
      (,(_ "None") set-frame:unframed
       (check . ,(eq (window-type w) 'unframed))
       (group . menu-type))))


;;; removing frame parts

  (define (remove-frame-class w class)
    (window-put w 'removed-classes
		(cons class (delq class (window-get w 'removed-classes))))
    (when (window-framed-p w)
      (rebuild-frame w)))

  (define (add-frame-class w class)
    (window-put w 'removed-classes
		(delq class (window-get w 'removed-classes)))
    (when (window-framed-p w)
      (rebuild-frame w)))

  (define (frame-class-removed-p w class)
    (memq class (window-get w 'removed-classes)))


;;; manipulating the frame part classes variables

  (define (set-frame-part-value class key value #!optional override)
    (let* ((var (if override 'override-frame-part-classes 'frame-part-classes))
	   (item (assq class (symbol-value var)))
	   tem)
      (if item
	  (if (setq tem (assq key (cdr item)))
	      (rplacd tem value)
	    (rplacd item (cons (cons key value) (cdr item))))
	(set var (cons (cons class (list (cons key value)))
		       (symbol-value var))))))

  ;; (def-frame-class shade-button '((cursor . foo) ...)
  ;;   (bind-keys shade-button-keymap
  ;;     "Button1-Off" 'toggle-window-shaded))
  ;; 
  ;; the idea being that it will only create the frame part if it doesn't
  ;; exist, it will add all properties from the second argument unless
  ;; they're already set, then create and initialise the keymap from the
  ;; third argument (unless a keymap is already defined)

  (defmacro def-frame-class (class alist #!rest keymap-forms)
    (if keymap-forms
	`(when (define-frame-class ',class ,alist t)
	   ,@keymap-forms)
      `(define-frame-class ',class ,alist)))

  (define (define-frame-class class alist #!optional with-keymap)
    (let ((cell (assq class frame-part-classes))
	  (ok-to-bind nil))
      (if (not cell)
	  (progn
	    (setq cell (cons class alist))
	    (setq frame-part-classes (cons cell frame-part-classes)))
	(mapc (lambda (attr)
		(let ((tem (assq (car attr) (cdr cell))))
		  (unless tem
		    (rplacd cell (cons attr (cdr cell))))))
	      alist))
      (when with-keymap
	(let ((map-name (intern (concat (symbol-name class) "-keymap"))))
	  (unless (boundp map-name)
	    (make-variable-special map-name)
	    (set map-name (make-keymap))
	    (setq ok-to-bind t)
	    ;; so the theme can bind to the keymap..
	    (gaol-define-special map-name))
	  (set-frame-part-value class 'keymap map-name)))
      ok-to-bind))

  (define ((cursor-for-frame-part part) w)
    (if (frame-part-movable-p w part)
	(case part
	  ((top-border) 'top_side)
	  ((bottom-border) 'bottom_side)
	  ((left-border) 'left_side)
	  ((right-border) 'right_side)
	  ((top-left-corner) 'top_left_corner)
	  ((top-right-corner) 'top_right_corner)
	  ((bottom-left-corner) 'bottom_left_corner)
	  ((bottom-right-corner) 'bottom_right_corner))
      nil))


;;; initialisation

  (define-frame-class 'menu-button '((keymap . menu-button-keymap)))
  (define-frame-class 'close-button '((keymap . close-button-keymap)))
  (define-frame-class 'iconify-button '((keymap . iconify-button-keymap)))
  (define-frame-class 'maximize-button '((keymap . maximize-button-keymap)))
  (define-frame-class 'shade-button '((keymap . shade-button-keymap)))

  (define-frame-class 'title `((keymap . title-keymap)
			       (cursor . ,(cursor-for-frame-part 'title))))

  (mapc (lambda (x)
	  (define-frame-class x `((cursor . ,(cursor-for-frame-part x))
				  (keymap . border-keymap))))
	'(top-border bottom-border
	  left-border right-border
	  top-left-corner top-right-corner
	  bottom-left-corner bottom-right-corner))

  (gaol-add add-frame-style reframe-window rebuild-frames-with-style
	    reframe-windows-with-style reframe-all-windows window-type
	    def-frame-class define-frame-class after-setting-frame-option
	    mark-frame-style-editable  frame-part-get frame-part-put
	    frame-part-window frame-part-x-window frame-part-position
	    frame-part-dimensions frame-part-state map-frame-parts
	    refresh-frame-part refresh-window rebuild-frame-part)

  (add-hook 'add-window-hook reframe-window t)
  (add-hook 'shape-notify-hook reframe-window t)

  (make-timer frames-on-idle theme-update-interval)

  (sm-add-saved-properties 'type 'ignored 'frame-style)
  (add-swapped-properties 'frame-active-color 'frame-inactive-color))
