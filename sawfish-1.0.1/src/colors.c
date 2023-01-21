/* colors.c -- Colour handling
   $Id: colors.c,v 1.22 2001/04/23 06:20:49 jsh Exp $

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

static Lisp_Color *color_list;
int color_type;

DEFSYM(default_foreground, "default-foreground");

DEFUN("get-color-rgb", Fget_color_rgb, Sget_color_rgb,
      (repv red, repv green, repv blue), rep_Subr3) /*
::doc:sawfish.wm.colors#get-color-rgb::
get-color-rgb RED GREEN BLUE
::end:: */
{
    Lisp_Color *f;
    rep_DECLARE1(red, rep_INTP);
    rep_DECLARE2(green, rep_INTP);
    rep_DECLARE3(blue, rep_INTP);

    if (dpy == 0)
	return Qnil;

    f = color_list;
    while (f != 0)
    {
	if (f->red == rep_INT(red)
	    && f->green == rep_INT(green)
	    && f->blue == rep_INT(blue))
	{
	    break;
	}
	f = f->next;
    }
    if (f == 0)
    {
	int pixel = best_color_match (rep_INT(red) / 256,
				      rep_INT(green) / 256,
				      rep_INT(blue) / 256);

	f = rep_ALLOC_CELL(sizeof(Lisp_Color));
	rep_data_after_gc += sizeof (Lisp_Color);
	f->car = color_type;
	f->next = color_list;
	color_list = f;

	f->red = rep_INT(red);
	f->green = rep_INT(green);
	f->blue = rep_INT(blue);
	f->pixel = pixel;
    }
    return rep_VAL(f);
}
    
DEFUN("get-color", Fget_color, Sget_color, (repv name), rep_Subr1) /*
::doc:sawfish.wm.colors#get-color::
get-color NAME

Return the color object representing the color named NAME, a standard
X11 color specifier.
::end:: */
{
    XColor exact_col;
    rep_DECLARE1(name, rep_STRINGP);

    if (dpy == 0)
	return Qnil;

    if (XParseColor (dpy, image_cmap, rep_STR(name), &exact_col) != 0)
    {
	return Fget_color_rgb (rep_MAKE_INT(exact_col.red),
			       rep_MAKE_INT(exact_col.green),
			       rep_MAKE_INT(exact_col.blue));
    }
    else
    {
	return Fsignal (Qerror,
			rep_list_2 (rep_string_dup("no such color"),
				    name));
    }
}

DEFUN("color-name", Fcolor_name, Scolor_name, (repv color), rep_Subr1) /*
::doc:sawfish.wm.colors#color-name::
color-name COLOR

Return the name of the color represented by the color object COLOR.
::end:: */
{
    char buf[32];
    rep_DECLARE1(color, COLORP);
    sprintf (buf, "#%04x%04x%04x",
	     VCOLOR(color)->red, VCOLOR(color)->green, VCOLOR(color)->blue);
    return rep_string_dup (buf);
}

DEFUN("color-rgb", Fcolor_rgb, Scolor_rgb, (repv color), rep_Subr1) /*
::doc:sawfish.wm.colors#color-rgb::
color-rgb COLOR

Returns a list of integers (RED GREEN BLUE) representing the actual
color values of the color represented by object COLOR. The individual
values range from zero to 65535.
::end:: */
{
    rep_DECLARE1(color, COLORP);
    return rep_list_3 (rep_MAKE_INT(VCOLOR(color)->red),
		       rep_MAKE_INT(VCOLOR(color)->green),
		       rep_MAKE_INT(VCOLOR(color)->blue));
}

DEFUN("color-rgb-8", Fcolor_rgb_8, Scolor_rgb_8, (repv color), rep_Subr1) /*
::doc:sawfish.wm.colors#color-rgb::
color-rgb-8 COLOR

Returns a list of integers (RED GREEN BLUE) representing the actual
color values of the color represented by object COLOR. The individual
values range from zero to 255.
::end:: */
{
    rep_DECLARE1(color, COLORP);
    return rep_list_3 (rep_MAKE_INT(VCOLOR(color)->red / 256),
		       rep_MAKE_INT(VCOLOR(color)->green / 256),
		       rep_MAKE_INT(VCOLOR(color)->blue / 256));
}

DEFUN("colorp", Fcolorp, Scolorp, (repv win), rep_Subr1) /*
::doc:sawfish.wm.colors#colorp::
colorp ARG

Returns t if ARG is a color object.
::end:: */
{
    return COLORP(win) ? Qt : Qnil;
}


/* type hooks */

static int
color_cmp (repv w1, repv w2)
{
    return w1 != w2;
}

static void
color_prin (repv stream, repv obj)
{
    char buf[256];
    sprintf (buf, "#<color #%04x%04x%04x>",
	     VCOLOR(obj)->red, VCOLOR(obj)->green, VCOLOR(obj)->blue);
    rep_stream_puts (stream, buf, -1, FALSE);
}

static void
color_sweep (void)
{
    Lisp_Color *w = color_list;
    color_list = 0;
    while (w != 0)
    {
	Lisp_Color *next = w->next;
	if (!rep_GC_CELL_MARKEDP(rep_VAL(w)))
	    rep_FREE_CELL(w);
	else
	{
	    rep_GC_CLR_CELL(rep_VAL(w));
	    w->next = color_list;
	    color_list = w;
	}
	w = next;
    }
}


/* initialisation */

void
colors_init (void)
{
    repv tem = rep_push_structure ("sawfish.wm.colors");
    color_type = rep_register_new_type ("color", color_cmp, color_prin,
					color_prin, color_sweep, 0,
					0, 0, 0, 0, 0, 0, 0);
    rep_ADD_SUBR(Sget_color_rgb);
    rep_ADD_SUBR(Sget_color);
    rep_ADD_SUBR(Scolor_name);
    rep_ADD_SUBR(Scolor_rgb);
    rep_ADD_SUBR(Scolor_rgb_8);
    rep_ADD_SUBR(Scolorp);
    rep_INTERN_SPECIAL(default_foreground);
    if (!batch_mode_p ())
    {
	repv black = Fget_color (rep_string_dup("#000000"));
	if (black == rep_NULL)
	    black = Qnil;
	Fset (Qdefault_foreground, black);
    }
    rep_pop_structure (tem);
}

void
colors_kill (void)
{
}
