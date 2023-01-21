/* flippers.c -- viewport edge flipping mechanism
   $Id: flippers.c,v 1.6 2000/07/27 14:37:56 john Exp $

   Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

   This file is part of sawmill.

   sawmill is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   sawmill is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with sawmill; see the file COPYING.   If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */

#include "sawmill.h"

static Window edge_left, edge_right, edge_top, edge_bottom;

DEFSYM(left, "left");
DEFSYM(right, "right");
DEFSYM(top, "top");
DEFSYM(bottom, "bottom");
DEFSYM(enter_flipper_hook, "enter-flipper-hook");
DEFSYM(leave_flipper_hook, "leave-flipper-hook");

DEFUN("enable-flippers", Fenable_flippers, Senable_flippers, (void), rep_Subr0)
{
    XMapRaised (dpy, edge_left);
    XMapRaised (dpy, edge_right);
    XMapRaised (dpy, edge_top);
    XMapRaised (dpy, edge_bottom);
    return Qt;
}

DEFUN("disable-flippers", Fdisable_flippers, Sdisable_flippers,
      (void), rep_Subr0)
{
    XUnmapWindow (dpy, edge_left);
    XUnmapWindow (dpy, edge_right);
    XUnmapWindow (dpy, edge_top);
    XUnmapWindow (dpy, edge_bottom);
    return Qt;
}

static repv
flipper_to_sym (Window w)
{
    if (w == edge_left)
	return Qleft;
    else if (w == edge_right)
	return Qright;
    else if (w == edge_top)
	return Qtop;
    else if (w == edge_bottom)
	return Qbottom;
    else
	return Qnil;
}

static Window
sym_to_flipper (repv sym)
{
    if (sym == Qleft)
	return edge_left;
    else if (sym == Qright)
	return edge_right;
    else if (sym == Qtop)
	return edge_top;
    else if (sym == Qbottom)
	return edge_bottom;
    else
	return 0;
}

static void
event_handler (XEvent *ev)
{
    switch (ev->xany.type)
    {
	repv sym;

    case EnterNotify:
    case LeaveNotify:
	sym = flipper_to_sym (ev->xany.window);
	Fcall_hook ((ev->xany.type == EnterNotify)
		    ? Qenter_flipper_hook : Qleave_flipper_hook,
		    Fcons (sym, Qnil), Qnil);
	break;
    }
}

DEFUN("flippers-after-restacking", Fflippers_after_restacking, 
      Sflippers_after_restacking, (void), rep_Subr0)
{
    /* Must keep edge windows raised so they always get the pointer */
    XRaiseWindow (dpy, edge_left);
    XRaiseWindow (dpy, edge_right);
    XRaiseWindow (dpy, edge_top);
    XRaiseWindow (dpy, edge_bottom);
    return Qt;
}


/* DL hooks / initialisation */

static Window
create_flipper (Window parent, int x, int y, int width, int height)
{
    Window w;
    XSetWindowAttributes wa;
    wa.override_redirect = True;
    w = XCreateWindow (dpy, parent, x, y, width, height, 0, 0,
		       InputOnly, preferred_visual, CWOverrideRedirect, &wa);
    if (w != 0)
    {
	XSelectInput (dpy, w, EnterWindowMask | LeaveWindowMask
		      | VisibilityChangeMask);
	register_event_handler (w, event_handler);
    }
    return w;
}

repv
rep_dl_init (void)
{
    repv tem = rep_push_structure ("sawfish.wm.util.flippers");

    rep_ADD_SUBR(Senable_flippers);
    rep_ADD_SUBR(Sdisable_flippers);
    rep_ADD_SUBR(Sflippers_after_restacking);

    rep_INTERN (left);
    rep_INTERN (right);
    rep_INTERN (top);
    rep_INTERN (bottom);
    rep_INTERN_SPECIAL (enter_flipper_hook);
    rep_INTERN_SPECIAL (leave_flipper_hook);

    if (!batch_mode_p ())
    {
	edge_left = create_flipper (root_window, 0, 0, 1, screen_height);
	edge_right = create_flipper (root_window, screen_width - 1, 0,
				     1, screen_height);
	edge_top = create_flipper (root_window, 0, 0, screen_width, 1);
	edge_bottom = create_flipper (root_window, 0, screen_height - 1,
				      screen_width, 1);

	add_hook (Qafter_restacking_hook, rep_VAL(&Sflippers_after_restacking));
	Fenable_flippers ();
    }

    return rep_pop_structure (tem);
}
