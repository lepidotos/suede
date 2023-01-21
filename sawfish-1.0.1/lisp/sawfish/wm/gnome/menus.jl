;; gnome-menu.jl -- replace the apps-menu by the gnome menu tree
;; $Id: menus.jl,v 1.25 2000/09/11 07:44:42 john Exp $

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

(define-structure sawfish.wm.gnome.menus

    (export gnome-menus-update
	    gnome-menus)

    (open rep
	  rep.system
	  rep.regexp
	  rep.io.files
	  sawfish.wm.commands)

  (define-structure-alias gnome-menu sawfish.wm.gnome.menus)

  (eval-when-compile (require 'sawfish.wm.menus))


;;; variables

  (defvar gnome-share-directory
    ;; search $PATH for a known GNOME binary..
    (catch 'out
      (let ((path (getenv "PATH"))
	    (point 0)
	    end tem)
	(while (< point (length path))
	  (setq end (if (string-match ":" path point)
			(match-start)
		      (length path)))
	  (setq tem (substring path point end))
	  (when (file-exists-p (expand-file-name "gnome-session" tem))
	    (throw 'out (expand-file-name "../share/gnome" tem)))
	  (setq point (1+ end))))
      nil))

  (defvar gnome-menu-lang (let ((lang (or (getenv "LANGUAGE")
					  (getenv "LC_ALL")
					  (getenv "LC_MESSAGES")
					  (getenv "LANG")))
				(all '()))
			    (when (and lang (not (string= lang "en")))
			      (setq all (cons lang all))
			      (when (string-match "[.@]" lang)
				(setq lang (substring lang 0 (match-start)))
				(setq all (cons lang all)))
			      (when (string-match "_" lang)
				(setq lang (substring lang 0 (match-start)))
				(setq all (cons lang all))))
			    all)
    "List of language codes used when constructing GNOME menus.")

  (defvar gnome-menu-roots (list (expand-file-name
				  "apps" gnome-share-directory)
				 "/etc/X11/applnk"	;on RedHat systems
				 "~/.gnome/apps")
    "List of directories to read GNOME menu entries from.")

  ;; previously read menus
  (define gnome-cached-menus nil)

  ;; split $PATH
  (define cached-path)


;;; code

  ;; search $PATH for an executable file..
  (define (gnome-exec-in-path filename)
    (let ((get-path (lambda ()
		      (unless cached-path
			(let ((path (getenv "PATH"))
			      (point 0)
			      out end)
			  (while (< point (length path))
			    (setq end (if (string-match ":" path point)
					  (match-start)
					(length path)))
			    (setq out (cons (substring path point end) out))
			    (setq point (1+ end)))
			  (setq cached-path (nreverse out))))
		      cached-path)))
      (catch 'out
	(mapc (lambda (d)
		(when (file-exists-p (expand-file-name filename d))
		  (throw 'out (expand-file-name filename d)))) (get-path))
	nil)))

  (define (gnome-menu-read-desktop-entry filename)
    (let ((file (condition-case nil
		    (open-file filename 'read)
		  (file-error nil)))
	  (section nil)
	  name exec tryexec terminal
	  line)
      (when file
	(unwind-protect
	    (while (setq line (read-line file))
	      (cond ((string-looking-at "\\[Desktop Entry\\]" line 0 t)
		     (setq section 'desktop-entry))
		    ((string-looking-at "\\s*$" line)
		     (setq section nil))
		    ((and (eq section 'desktop-entry)
			  (string-looking-at "Name=(.*)\n" line 0 t))
		     (setq name (expand-last-match "\\1")))
		    ((and (eq section 'desktop-entry) gnome-menu-lang
			  (string-looking-at "Name\\[([^]]+)\\]=(.*)\n" line 0 t)
			  (member (expand-last-match "\\1") gnome-menu-lang))
		     (setq name (expand-last-match "\\2")))
		    ((and (eq section 'desktop-entry)
			  (string-looking-at "Exec=(.*)\n" line 0 t))
		     (setq exec (expand-last-match "\\1")))
		    ((and (eq section 'desktop-entry)
			  (string-looking-at "TryExec=(.*)\n" line 0 t))
		     (setq tryexec (expand-last-match "\\1")))
		    ((and (eq section 'desktop-entry)
			  (string-looking-at "Terminal=(.*)\n" line 0 t))
		     (setq terminal (expand-last-match "\\1"))
		     (setq terminal (not (string-match
					  "^0|false$" terminal 0 t))))))
	  (close-file file))
	(cond ((string= (file-name-nondirectory filename) ".directory")
	       (let ((menus (gnome-menu-read-directory
			     (file-name-directory filename))))
		 (when menus
		   `(,(or name filename) ,@menus))))
	      ((and exec (or (not tryexec) (gnome-exec-in-path tryexec)))
	       ;; create a menu item
	       `(,(or name exec)
		 (system ,(concat (if terminal
				      (progn
					(require 'sawfish.wm.commands.xterm)
					;; XXX hope that this supports `-e'
					(concat xterm-program " -e " exec))
				    exec) " &"))))))))

  (define (gnome-menu-read-order filename)
    (let ((file (condition-case nil
		    (open-file filename 'read)
		  (file-error nil))))
      (when file
	(unwind-protect
	    (let
		(order tem)
	      (while (setq tem (read-line file))
		(when (string-match "\\S+" tem)
		  (setq tem (substring tem (match-start) (match-end))))
		(setq order (cons tem order)))
	      (nreverse order))
	  (close-file file)))))

  (define (gnome-menu-read-item dirname file)
    (unless (= (aref file 0) ?.)
      (setq file (expand-file-name file dirname))
      (cond
       ((file-regular-p file)
	(gnome-menu-read-desktop-entry file))
       ((file-directory-p file)
	(if (file-exists-p (expand-file-name ".directory" file))
	    (gnome-menu-read-desktop-entry
	     (expand-file-name ".directory" file))
	  (let
	      ((menus (gnome-menu-read-directory file)))
	    (when menus
	      (cons (file-name-nondirectory file) menus))))))))

  (define (gnome-menu-read-directory dirname)
    (let ((order (and (file-exists-p (expand-file-name ".order" dirname))
		      (gnome-menu-read-order
		       (expand-file-name ".order" dirname))))
	  menus unordered item)
      (mapc (lambda (file)
	      (when (file-exists-p (expand-file-name dirname file))
		(when (setq item (gnome-menu-read-item dirname file))
		  (setq menus (cons item menus)))))
	    order)
      (mapc (lambda (file)
	      (unless (or (= (aref file 0) ?.) (member file order))
		(when (setq item (gnome-menu-read-item dirname file))
		  (setq unordered (cons item unordered)))))
	    (directory-files dirname))
      (when (or menus unordered)
	(nconc (nreverse menus)
	       (sort unordered (lambda (x y)
				 (string-lessp (car x) (car y))))))))
  
  (define (gnome-menus-merge-dups menus)
    (let (ptr inner-ptr item tem)
      (setq ptr menus)
      (while ptr
	(setq item (car ptr))
	(when (and item (consp (cdr item)) (not (functionp (cdr item))))
	  (setq inner-ptr (cdr ptr))
	  (while inner-ptr
	    (setq tem (car inner-ptr))
	    (setq inner-ptr (cdr inner-ptr))
	    (when (and tem (string= (car item) (car tem))
		       (consp (cdr tem)) (not (functionp (cdr tem))))
	      ;; we've found a later occurrence of this sub-menu
	      (setq menus (delq tem menus))
	      (nconc item (cdr tem)))))
	(setq ptr (cdr ptr)))
      ;; now we've uniqued the top-level, recurse through any sub-menus
      (setq ptr menus)
      (while ptr
	(setq item (car ptr))
	(setq ptr (cdr ptr))
	(when (and item (consp (cdr item)) (not (functionp (cdr item))))
	  (rplacd item (gnome-menus-merge-dups (cdr item)))))
      menus))

  (define (gnome-menus-update)
    (setq gnome-cached-menus nil)
    (mapc (lambda (dir)
	    (when (and (stringp dir) (file-directory-p dir))
	      (setq gnome-cached-menus
		    (nconc gnome-cached-menus
			   (gnome-menu-read-directory dir)))))
	  gnome-menu-roots)
    (setq gnome-cached-menus (gnome-menus-merge-dups gnome-cached-menus))
    gnome-cached-menus)

  (define-command 'gnome-menus-update gnome-menus-update)

  (define (gnome-menus)
    (unless gnome-cached-menus
      (gnome-menus-update))
    gnome-cached-menus)


;;; init

  ;; take over the applications submenu of the root window menu

  (make-variable-special 'apps-menu)
  (setq apps-menu gnome-menus)

  ;; load the menus when we idle, it reduces the latency of the first
  ;; menu popup

  (letrec ((gnome-on-idle (lambda ()
			    (gnome-menus)
			    (remove-hook 'idle-hook gnome-on-idle))))
    (add-hook 'idle-hook gnome-on-idle)))
