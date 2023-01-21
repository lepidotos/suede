/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */

/* eel-art-gtk-extensions.h - Access gtk/gdk attributes as libart rectangles.

   Copyright (C) 2000 Eazel, Inc.

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

/* The following functions accept gtk/gdk structures and
 * return their bounds and dimensions, where:
 *
 * bounds: The (x,y) and (width, height) of something.
 * dimensions: The (width, height) of something.
 *
 * These are very useful in code that uses libart functions
 * to do operations on ArtIRects (such as intersection)
 */

#ifndef EEL_ART_GTK_EXTENSIONS_H
#define EEL_ART_GTK_EXTENSIONS_H

#include <eel/eel-gdk-extensions.h>
#include <eel/eel-gtk-extensions.h>
#include <eel/eel-art-extensions.h>

BEGIN_GNOME_DECLS

/* Convert between GdkRectangle and ArtIRect and back */
GdkRectangle  eel_art_irect_to_gdk_rectangle            (ArtIRect         rectangle);
ArtIRect      eel_gdk_rectangle_to_art_irect            (GdkRectangle     gdk_rectangle);
EelDimensions eel_screen_get_dimensions                 (void);

/* GdkWindow parent-relative bounds */
ArtIRect      eel_gdk_window_get_bounds                 (const GdkWindow *gdk_window);

/* GdkWindow dimensions */
EelDimensions eel_gdk_window_get_dimensions             (const GdkWindow *gdk_window);

/* GdkWindow screen parent-relative bounds */
ArtIRect      eel_gdk_window_get_screen_relative_bounds (const GdkWindow *gdk_window);

/* Clip a dirty area (from exposures) to the on screen parts of a GdkWindow */
ArtIRect      eel_gdk_window_clip_dirty_area_to_screen  (const GdkWindow *gdk_window,
							 ArtIRect         dirty_area);

/* GtkWidget bounds and dimensions */
ArtIRect      eel_gtk_widget_get_bounds                 (const GtkWidget *gtk_widget);
EelDimensions eel_gtk_widget_get_dimensions             (const GtkWidget *gtk_widget);
EelDimensions eel_gtk_widget_get_preferred_dimensions   (const GtkWidget *gtk_widget);
EelArtIPoint  eel_gdk_get_pointer_position              (void);

END_GNOME_DECLS

#endif /* EEL_ART_GTK_EXTENSIONS_H */
