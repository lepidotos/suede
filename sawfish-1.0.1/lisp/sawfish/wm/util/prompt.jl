;; prompt.jl -- read line from user
;; Time-stamp: <2000-02-25 22:02:54 tjp>
;;
;; Copyright (C) 2000 Topi Paavola <tjp@iki.fi>
;;   
;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Commentary:

;; See the documentation for prompt.

(define-structure sawfish.wm.util.prompt

    (export prompt
	    prompt-for-symbol
	    prompt-for-function
	    prompt-for-variable
	    prompt-for-command

	    ;; autoloaded from prompt-extras
	    prompt-for-file
	    prompt-for-directory
	    prompt-from-list
	    prompt-for-string
	    prompt-for-number
	    pwd-prompt

	    ;; autoloaded from prompt-wm
	    prompt-for-window
	    prompt-for-workspace)

    (open rep
	  rep.system
	  rep.regexp
	  rep.data.ring
	  sawfish.wm.misc
	  sawfish.wm.events
	  sawfish.wm.custom
	  sawfish.wm.commands)

  (define-structure-alias prompt sawfish.wm.util.prompt)

  (defcustom prompt-keymap (make-keymap)
    "Keymap containing bindings active when reading a string from the user."
    :group bindings
    :type keymap)

  (defvar prompt-max-display 20
    "Maximum number of completions to display under the input string.")

  (defvar prompt-word-regexp "[0-9a-z_]"
    "Regexp that determines which characters are to be considered part
of a word when moving.")

  (defvar prompt-file-exclude '"\\.(o|jlc|x)$|~$|^#.*#$|^\\.\\.?$"
    "A regexp, if it matches the file being considered for completion, the file
is rejected.")

  (defvar prompt-list nil
    "List of possible entries for prompt-from-list.")

  (defvar prompt-list-fold-case nil
    "Whether prompt-from-list should ignore case.")

  (defvar prompt-history (make-ring 16)
    "Ring buffer containing strings most-recently entered through the `prompt'
function.")

  (defvar prompt-window-position
    (cons (- (quotient (screen-width) 2) 200) -200)
    "A cons cell defining the screen position at which the `prompt' window is
displayed. See the `display-message' function for more details.")

  (defvar prompt-result nil)
  (defvar prompt-prompt nil)
  (defvar prompt-completion-fun nil)
  (defvar prompt-validation-fun nil)
  (defvar prompt-abbrev-fun nil)
  (defvar prompt-display-fun nil)
  (defvar prompt-position 0)
  (defvar prompt-completion-position nil)
  (defvar prompt-completions nil)
  (defvar prompt-completions-outdated nil)
  (defvar prompt-history-pos nil)
  (defvar prompt-saved nil)

  (defun prompt-exit ()
    "Cancel string input."
    (throw 'prompt-exit nil))

  (defun prompt-accept ()
    "End input and accept current string."
    (let ((result (if (not prompt-validation-fun)
		      prompt-result
		    (prompt-validation-fun prompt-result))))
      (if result
	  (progn
	    (unless (or (null prompt-history)
			(equal (get-from-ring prompt-history 1) prompt-result))
	      (add-to-ring prompt-history prompt-result))
	    (throw 'prompt-exit result))
	(beep))))

  (defun prompt-next (count)
    (interactive "p")
    (when prompt-history
      (setq count (- prompt-history-pos count))
      (if (zerop count)
	  (progn
	    (setq prompt-result prompt-saved)
	    (setq prompt-history-pos count))
	(let
	    ((string (get-from-ring prompt-history count)))
	  (when string
	    (when (zerop prompt-history-pos)
	      (setq prompt-saved prompt-result))
	    (setq prompt-result string)
	    (setq prompt-history-pos count))))
      (prompt-changed)
      (prompt-end-of-line)
      (prompt-update-display)))

  (defun prompt-previous (count)
    (interactive "p")
    (prompt-next (- count)))

  (defun prompt-changed ()
    (setq prompt-completions-outdated t))

  (defun prompt-clear ()
    "Clear input buffer."
    (setq prompt-result "")
    (setq prompt-position 0)
    (prompt-changed)
    (prompt-update-display))

  (defun prompt-backspace ()
    "Remove previous character from buffer."
    (when (> prompt-position 0)
      (let ((cutoff (max (- prompt-position 1) 0)))
	(setq prompt-result
	      (concat (substring prompt-result 0 cutoff)
		      (substring prompt-result (1+ cutoff))))
	(setq prompt-position (max 0 (1- prompt-position)))
	(prompt-changed)
	(prompt-update-display))))

  (defun prompt-kill-line ()
    "Delete rest of line."
    (setq prompt-result (substring prompt-result 0 prompt-position))
    (prompt-changed)
    (prompt-update-display))

  (defun prompt-move (num)
    "Move NUM characters forward or backward."
    (let ((new-pos (+ prompt-position num)))
      (and (>= new-pos 0) (<= new-pos (length prompt-result))
	   (setq prompt-position new-pos)
	   (prompt-update-display))))
         
  (defun prompt-forward-word ()
    "Move to next non-word character."
    (setq prompt-position (1+ prompt-position))
    (while (and (< prompt-position (length prompt-result))
		(string-looking-at prompt-word-regexp
				   prompt-result prompt-position t))
      (setq prompt-position (1+ prompt-position)))
    (setq prompt-position (min prompt-position
			       (length prompt-result)))
    (prompt-update-display))

  (defun prompt-backward-word ()
    "Move to previous non-word character."
    (setq prompt-position (1- prompt-position))
    (while (and (> prompt-position 0)
		(string-looking-at prompt-word-regexp
				   prompt-result prompt-position t))
      (setq prompt-position (1- prompt-position)))
    (setq prompt-position (max prompt-position 0))
    (prompt-update-display))

  (defun prompt-forward-character ()
    "Move forward one character."
    (prompt-move 1))

  (defun prompt-backward-character ()
    "Move backward one character."
    (prompt-move -1))

  (defun prompt-beginning-of-line ()
    "Move to beginning of line."
    (setq prompt-position 0)
    (prompt-update-display))

  (defun prompt-end-of-line ()
    "Move to end of line."
    (setq prompt-position (length prompt-result))
    (prompt-update-display))

  (defun prompt-complete ()
    (if (and (not prompt-completions-outdated) prompt-completion-position)
	(let
	    ((new (min (max 0 (- (length prompt-completions)
				 prompt-max-display))
		       (+ prompt-completion-position prompt-max-display))))
	  (setq prompt-completion-position
		(if (= new prompt-completion-position)
		    0
		  new)))
      (when prompt-completion-fun
	(let
	    (compl)
	  (setq prompt-completions (prompt-completion-fun prompt-result))
	  (setq compl (complete-string prompt-result prompt-completions))
	  (when compl
	    (when (string= compl prompt-result)
	      (setq prompt-completions-outdated nil))
	    (setq prompt-result compl)
	    (setq prompt-completions
		  (sort (delete-if-not (lambda (x)
					 (string-head-eq x compl))
				       prompt-completions)))
	    (prompt-end-of-line)
	    (when (cdr prompt-completions)
	      (setq prompt-completion-position 0))))))
    (prompt-update-display))

  (defun prompt-format-completions ()
    (when (numberp prompt-completion-position)
      (let ((compl (nthcdr prompt-completion-position prompt-completions))
	    (continued nil))
	(when (nthcdr prompt-max-display compl)
	  (setq compl (reverse (nthcdr (- (length compl) prompt-max-display)
				       (reverse compl))))
	  (setq continued "[...]\n"))
	(concat (and (/= prompt-completion-position 0) "[...]\n")
		(apply concat (mapcar (lambda (x)
					(format nil "%s\n"
						(if prompt-abbrev-fun
						    (prompt-abbrev-fun x)
						  x)))
				      compl))
		continued))))

  (defun prompt-update-display ()
    (let ((result (if prompt-display-fun
		      (prompt-display-fun prompt-result)
		    prompt-result)))
      (display-message (concat (prompt-format-completions)
			       "\n\n"
			       prompt-prompt
			       (substring result 0 prompt-position)
			       ?| (substring result prompt-position))
		       `((position . ,prompt-window-position)))))

  ;; Insert all unbound keys to result.
  (defun prompt-unbound-callback ()
    (let ((key (current-event-string)))
      (setq prompt-result
	    (concat (substring prompt-result 0 prompt-position)
		    key
		    (substring prompt-result prompt-position)))
      (setq prompt-position (+ prompt-position (length key)))
      (prompt-changed)
      (prompt-update-display)
      t))

  (defun prompt (#!optional title start)
    "Prompt the user for a string."
    (unless (stringp title)
      (setq title "Enter string:"))
    (unless (string-match " $" title)
      (setq title (concat title ? )))
    (call-with-keyboard-grabbed
     (lambda ()
       (unwind-protect
	   (let* ((override-keymap prompt-keymap)
		  (prompt-result (or start ""))
		  (prompt-prompt title)
		  (prompt-position (length prompt-result))
		  (prompt-history-pos 0)
		  (prompt-saved nil)
		  (prompt-completion-position nil)
		  (prompt-completions nil)
		  (prompt-completions-outdated t)
		  (unbound-key-hook (list prompt-unbound-callback)))
	     (prompt-update-display)
	     (catch 'prompt-exit
	       (recursive-edit)))
	 (display-message nil)))))

  (defun prompt-for-symbol (#!optional title predicate validator)
    (let ((prompt-completion-fun 
	   (lambda (x)
	     (mapcar symbol-name
		     (apropos (concat ?^ (quote-regexp x)) predicate))))
	  (prompt-validation-fun
	   (lambda (x)
	     (let
		 ((symbol (intern x)))
	       (if validator
		   (and (validator symbol) symbol)
		 symbol)))))
      (prompt title)))

  (defun prompt-for-function (#!optional title)
    "Prompt for a function."
    (prompt-for-symbol (or title "Enter name of function:")
		       (lambda (x)
			 (and (boundp x)
			      (let ((value (symbol-value x)))
				(or (functionp value)
				    (macrop value)
				    (special-form-p value)))))))

  (defun prompt-for-variable (#!optional title)
    "Prompt for a variable."
    (prompt-for-symbol (or title "Enter name of variable:") boundp))

  (defun prompt-for-command (#!optional title)
    (prompt-for-symbol title commandp commandp))


;;; autoloads

  (autoload 'prompt-for-file "sawfish/wm/util/prompt-extras")
  (autoload 'prompt-for-directory "sawfish/wm/util/prompt-extras")
  (autoload 'prompt-from-list "sawfish/wm/util/prompt-extras")
  (autoload 'prompt-for-string "sawfish/wm/util/prompt-extras")
  (autoload 'prompt-for-number "sawfish/wm/util/prompt-extras")
  (autoload 'pwd-prompt "sawfish/wm/util/prompt-extras")

  (autoload 'prompt-for-window "sawfish/wm/util/prompt-wm")
  (autoload 'prompt-for-workspace "sawfish/wm/util/prompt-wm")


;;; init keymap

  (bind-keys prompt-keymap
             "ESC" prompt-exit
             "C-g" prompt-exit
             "C-u" prompt-clear
             "BS" prompt-backspace
             "C-k" prompt-kill-line
             "Left" prompt-backward-character
             "C-b" prompt-backward-character
             "Right" prompt-forward-character
             "C-f" prompt-forward-character
             "C-Left" prompt-backward-word
             "M-b" prompt-backward-word
             "A-b" prompt-backward-word
             "C-Right" prompt-forward-word
             "M-f" prompt-forward-word
             "A-f" prompt-forward-word
             "C-a" prompt-beginning-of-line
             "C-e" prompt-end-of-line
             "TAB" prompt-complete
             "RET" prompt-accept
	     "Up" prompt-previous
	     "Down" prompt-next
	     "M-n" prompt-next
	     "M-p" prompt-previous
	     "A-n" prompt-next
	     "A-p" prompt-previous))
