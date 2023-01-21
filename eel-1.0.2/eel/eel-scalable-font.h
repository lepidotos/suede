/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */

/* eel-scalable-font.h - A GtkObject subclass for access to scalable fonts.

   Copyright (C) 1999, 2000 Eazel, Inc.

   The Gnome Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The Gnome Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the Gnome Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

   Authors: Ramiro Estrugo <ramiro@eazel.com>
*/

/* EelScalableFont is a GtkObject that provdes a simple
 * interface to Raph Levien's librsvg FreeType2 based anti aliased
 * text rendering.
 *
 * Currently, only Type1 fonts are supported.
 *
 * Fonts are automatically queried and used if available.  Right
 * now this is fairly simple code which does not handle all the 
 * complexities of the hell that is unix fonts.
 *
 * In the Star Trek future, we will use gnome-print (gnome-font?).
 * However, we will keep the interface to scalable font usage simple
 * and hidden behind this interface.
 */

#ifndef EEL_SCALABLE_FONT_H
#define EEL_SCALABLE_FONT_H

#include <gtk/gtkobject.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <libgnome/gnome-defs.h>
#include <eel/eel-string-list.h>
#include <eel/eel-art-extensions.h>
#include <libart_lgpl/art_rect.h>

BEGIN_GNOME_DECLS

#define EEL_TYPE_SCALABLE_FONT		  (eel_scalable_font_get_type ())
#define EEL_SCALABLE_FONT(obj)		  (GTK_CHECK_CAST ((obj), EEL_TYPE_SCALABLE_FONT, EelScalableFont))
#define EEL_SCALABLE_FONT_CLASS(klass)	  (GTK_CHECK_CLASS_CAST ((klass), EEL_TYPE_SCALABLE_FONT, EelScalableFontClass))
#define EEL_IS_SCALABLE_FONT(obj)	  (GTK_CHECK_TYPE ((obj), EEL_TYPE_SCALABLE_FONT))
#define EEL_IS_SCALABLE_FONT_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), EEL_TYPE_SCALABLE_FONT))

typedef struct EelScalableFont		EelScalableFont;
typedef struct EelScalableFontClass     EelScalableFontClass;
typedef struct EelScalableFontDetails   EelScalableFontDetails;

struct EelScalableFont
{
	/* Superclass */
	GtkObject object;

	/* Private things */
	EelScalableFontDetails *details;
};

struct EelScalableFontClass
{
	GtkObjectClass parent_class;
};

GtkType           eel_scalable_font_get_type                  (void);
EelScalableFont * eel_scalable_font_new                       (const char            *file_name);
EelDimensions     eel_scalable_font_measure_text              (const EelScalableFont *font,
							       int                    font_size,
							       const char            *text,
							       guint                  text_length);
int               eel_scalable_font_text_width                (const EelScalableFont *font,
							       int                    font_size,
							       const char            *text,
							       guint                  text_length);
void              eel_scalable_font_draw_text                 (const EelScalableFont *font,
							       GdkPixbuf             *destination_pixbuf,
							       int                    x,
							       int                    y,
							       ArtIRect               clip_area,
							       int                    font_size,
							       const char            *text,
							       guint                  text_length,
							       guint32                color,
							       int                    opacity);
int               eel_scalable_font_largest_fitting_font_size (const EelScalableFont *font,
							       const char            *text,
							       int                    available_width,
							       int                    minimum_acceptable_font_size,
							       int                    maximum_acceptable_font_size);
EelScalableFont  *eel_scalable_font_get_default_font          (void);
EelScalableFont  *eel_scalable_font_get_default_bold_font     (void);
EelScalableFont  *eel_scalable_font_make_bold                 (EelScalableFont       *font);

END_GNOME_DECLS

#endif /* EEL_SCALABLE_FONT_H */


