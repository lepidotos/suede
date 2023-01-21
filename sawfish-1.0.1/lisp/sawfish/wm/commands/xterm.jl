#| xterm.jl -- command to launch an xterm

   $Id: xterm.jl,v 1.4 2000/09/11 07:44:42 john Exp $

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

(define-structure sawfish.wm.commands.xterm

    (export xterm)

    (open rep
	  rep.system
	  sawfish.wm.custom
	  sawfish.wm.commands)

  (defcustom xterm-program "xterm"
    "The program launched by the `xterm' command."
    :type string
    :user-level expert
    :group misc)

  (defcustom xterm-args nil
    "Optional arguments given to the `xterm' command."
    :type (optional string)
    :user-level expert
    :group misc)

  (define (xterm #!optional command)
    "Start a new xterm."
    (if (not command)
	(system (format nil "%s %s >/dev/null 2>&1 </dev/null &"
			xterm-program (or xterm-args "")))
      (system (format nil "%s %s -e %s >/dev/null 2>&1 </dev/null&"
		      xterm-program (or xterm-args "") command))))

  ;;###autoload
  (define-command 'xterm xterm))
