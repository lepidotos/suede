/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */

/* eel-smooth-widget.c - Functions and types shared by smooth widgets.

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

#include <config.h>

#include "eel-smooth-widget.h"
#include "eel-gdk-extensions.h"
#include "eel-art-gtk-extensions.h"

#include <gtk/gtksignal.h>

static EelArtIPoint smooth_widget_get_tile_origin_point              (const GtkWidget              *widget,
								      EelSmoothTileMode             tile_mode_vertical,
								      EelSmoothTileMode             tile_mode_horizontal);
static void         smooth_widget_paint_tile_opaque                  (GtkWidget                    *widget,
								      GdkGC                        *gc,
								      const GdkPixbuf              *tile_pixbuf,
								      ArtIRect               tile_bounds,
								      EelArtIPoint           tile_origin,
								      ArtIRect               dirty_area);
static void         smooth_widget_paint_tile_transparent             (GtkWidget                    *widget,
								      GdkGC                        *gc,
								      EelSmoothBackgroundMode       background_mode,
								      guint32                       solid_background_color,
								      const GdkPixbuf              *tile_pixbuf,
								      ArtIRect               tile_bounds,
								      int                           tile_opacity,
								      EelArtIPoint           tile_origin,
								      ArtIRect               dirty_area);
static void         smooth_widget_paint_content_opaque               (GtkWidget                    *widget,
								      GdkGC                        *gc,
								      ArtIRect               content_bounds,
								      ArtIRect               dirty_area,
								      EelSmoothPaintOpaqueCallback  paint_callback,
								      gpointer                      callback_data);
static void         smooth_widget_paint_content_transparent          (GtkWidget                    *widget,
								      GdkGC                        *gc,
								      EelSmoothBackgroundMode       background_mode,
								      guint32                       solid_background_color,
								      ArtIRect               content_bounds,
								      int                           content_opacity,
								      ArtIRect               dirty_area,
								      EelSmoothCompositeCallback    composite_callback,
								      gpointer                      callback_data);
static void         smooth_widget_paint_tile_and_content_transparent (GtkWidget                    *widget,
								      GdkGC                        *gc,
								      EelSmoothBackgroundMode       background_mode,
								      guint32                       solid_background_color,
								      const GdkPixbuf              *tile_pixbuf,
								      ArtIRect               tile_bounds,
								      int                           tile_opacity,
								      EelArtIPoint           tile_origin,
								      ArtIRect               content_bounds,
								      int                           content_opacity,
								      ArtIRect               dirty_area,
								      EelSmoothCompositeCallback    composite_callback,
								      gpointer                      callback_data);

/* We maintain a global list of smooth widgets.  The smoothness of all these
 * widgets can then be changed with eel_smooth_widget_global_set_is_smooth().
 *
 * We do this so that labels and images will repect the "smooth_graphics"
 * automatically without any intervention from the users of these two widgets.
 */
static GList *smooth_widget_list = NULL;

/* We maintain a global list of types that can be smooth widgets.
 * We do this mostly for type safety - to make sure smooth operations only
 * happen on smooth widgets.
 */
static GList *smooth_widget_type_list = NULL;

static void
smooth_widget_type_list_free (void)
{
	g_list_free (smooth_widget_type_list);
	smooth_widget_type_list = NULL;
}

static void
smooth_widget_list_free (void)
{
	g_list_free (smooth_widget_list);
	smooth_widget_list = NULL;
}

static gboolean
widget_is_smooth (const GtkWidget *widget)
{
	GList *node;

	for (node = smooth_widget_type_list; node ; node = node->next) {
		GtkType type;

		type = GPOINTER_TO_INT (node->data);

		if (GTK_CHECK_TYPE ((widget), type)) {
			return TRUE;
		}
	}

	return FALSE;
}

static void
smooth_widget_set_is_smooth (GtkWidget *widget, gboolean is_smooth)
{
	g_return_if_fail (widget_is_smooth (widget));

	gtk_signal_emit_by_name (GTK_OBJECT (widget), "set_is_smooth", is_smooth);
}

static void
smooth_widget_destroy (GtkWidget *widget, gpointer callback_data)
{
	g_return_if_fail (widget_is_smooth (widget));

	smooth_widget_list = g_list_remove (smooth_widget_list, widget);
}

static gboolean global_is_smooth = TRUE;

void
eel_smooth_widget_global_set_is_smooth (gboolean is_smooth)
{
	GList *node;

	global_is_smooth = is_smooth;

	for (node = smooth_widget_list; node != NULL; node = node->next) {
		smooth_widget_set_is_smooth (GTK_WIDGET (node->data), global_is_smooth);
	}
}

/**
 * eel_smooth_widget_register:
 * @widget: A smooth widget.
 *
 * Register a smooth widget.  For the life time of the widget, its 
 * 'is_smooth' attribute will be toggled when 
 * eel_smooth_widget_global_set_is_smooth() is called.
 */
void
eel_smooth_widget_register (GtkWidget *widget)
{
	g_return_if_fail (widget_is_smooth (widget));

	smooth_widget_set_is_smooth (widget, global_is_smooth);

	if (smooth_widget_list == NULL) {
		g_atexit (smooth_widget_list_free);
	}

	smooth_widget_list = g_list_prepend (smooth_widget_list, widget);

	/* Keep track of the widget's destruction so we can purge it */
	gtk_signal_connect (GTK_OBJECT (widget),
			    "destroy",
			    GTK_SIGNAL_FUNC (smooth_widget_destroy),
			    NULL);
}

/* Return the origin point for the widget given vertical
 * and horizontal tile modes.
 */
static EelArtIPoint
smooth_widget_get_tile_origin_point (const GtkWidget *widget,
				     EelSmoothTileMode tile_mode_vertical,
				     EelSmoothTileMode tile_mode_horizontal)

{
	EelArtIPoint origin_point;

	g_return_val_if_fail (widget_is_smooth (widget), eel_art_ipoint_zero);
	g_return_val_if_fail (GTK_WIDGET_REALIZED (widget), eel_art_ipoint_zero);
	g_return_val_if_fail (tile_mode_vertical >= EEL_SMOOTH_TILE_SELF, eel_art_ipoint_zero);
	g_return_val_if_fail (tile_mode_vertical <= EEL_SMOOTH_TILE_ANCESTOR, eel_art_ipoint_zero);
	g_return_val_if_fail (tile_mode_horizontal >= EEL_SMOOTH_TILE_SELF, eel_art_ipoint_zero);
	g_return_val_if_fail (tile_mode_horizontal <= EEL_SMOOTH_TILE_ANCESTOR, eel_art_ipoint_zero);

	origin_point = eel_art_ipoint_zero;

	/* Using '0' for the ancestor origin works because our buddy GTK+ 
	 * already makes our allocation.{x,y} the collected offsets of all
	 * our ancestors.
	 *
	 * It might be more correct to make this the allocation.{x,y} of the
	 * ancestor windowed widget - maybe.
	 */
	switch (tile_mode_vertical) {
	case EEL_SMOOTH_TILE_SELF:
		origin_point.y = widget->allocation.y;
		break;

	case EEL_SMOOTH_TILE_ANCESTOR:
		origin_point.y = 0;
		break;
	}

	switch (tile_mode_horizontal) {
	case EEL_SMOOTH_TILE_SELF:
		origin_point.x = widget->allocation.x;
		break;

	case EEL_SMOOTH_TILE_ANCESTOR:
		origin_point.x = 0;
		break;
	}

	return origin_point;
}

/* Return an area of the smooth widget's Gtk background as a pixbuf.
 * The resulting pixbuf needs to be unrefed when done using it.
 */
static GdkPixbuf *
smooth_widget_get_gtk_background (GtkWidget *widget, 
				  ArtIRect area)
{
	GdkPixbuf *background;
	int width;
	int height;
	
	g_return_val_if_fail (widget_is_smooth (widget), NULL);
	g_return_val_if_fail (GTK_WIDGET_REALIZED (widget), NULL);
	g_return_val_if_fail (!art_irect_empty (&area), NULL);

	width = eel_art_irect_get_width (area);
	height = eel_art_irect_get_height (area);
	
	background = eel_gdk_pixbuf_get_from_window_safe (widget->window,
							  area.x0,
							  area.y0,
							  width,
							  height);
	if (background == NULL) {
		ArtIRect fill_area;

		background = eel_gdk_pixbuf_get_global_buffer (width, height);
		fill_area = eel_art_irect_assign (0, 0, width, height);
		eel_gdk_pixbuf_fill_rectangle_with_color (background,
							  fill_area,
							  EEL_RGB_COLOR_WHITE);
		gdk_pixbuf_ref (background);
	} else {
		g_assert (gdk_pixbuf_get_width (background) == width);
		g_assert (gdk_pixbuf_get_height (background) == height);
	}

	return background;
}

/* Return an area of the smooth widget's background as a pixbuf.
 * The resulting pixbuf needs to be unrefed when done using it.
 */
static GdkPixbuf *
smooth_widget_get_background (GtkWidget *widget,
			      ArtIRect area,
			      EelSmoothBackgroundMode background_mode,
			      guint32 solid_background_color)
{
	GdkPixbuf *buffer;
	
	g_return_val_if_fail (widget_is_smooth (widget), NULL);
	g_return_val_if_fail (GTK_WIDGET_REALIZED (widget), NULL);
	g_return_val_if_fail (!art_irect_empty (&area), NULL);
	g_return_val_if_fail (background_mode >= EEL_SMOOTH_BACKGROUND_GTK, NULL);
	g_return_val_if_fail (background_mode <= EEL_SMOOTH_BACKGROUND_SOLID_COLOR, NULL);

	if (background_mode == EEL_SMOOTH_BACKGROUND_GTK) {
		buffer = smooth_widget_get_gtk_background (widget, area);
	} else {
		buffer = eel_gdk_pixbuf_get_global_buffer (eel_art_irect_get_width (area),
							   eel_art_irect_get_height (area));

		if (background_mode == EEL_SMOOTH_BACKGROUND_CALLBACK) {
			gtk_signal_emit_by_name (GTK_OBJECT (widget),
						 "draw_background",
						 buffer,
						 &area);
		} else {
			eel_gdk_pixbuf_fill_rectangle_with_color (buffer,
								  eel_gdk_pixbuf_whole_pixbuf,
								  solid_background_color);
		}

		gdk_pixbuf_ref (buffer);
	}
 	g_return_val_if_fail (eel_gdk_pixbuf_is_valid (buffer), NULL);

	return buffer;
}

/* Paint the smooth widget's tile in opaque mode */
static void
smooth_widget_paint_tile_opaque (GtkWidget *widget,
				 GdkGC *gc,
				 const GdkPixbuf *tile_pixbuf,
				 ArtIRect tile_bounds,
				 EelArtIPoint tile_origin,
				 ArtIRect dirty_area)
{
 	ArtIRect tile_dirty_area;
	
	g_return_if_fail (widget_is_smooth (widget));
	g_return_if_fail (GTK_WIDGET_REALIZED (widget));
	g_return_if_fail (widget->window != NULL);
	g_return_if_fail (eel_gdk_pixbuf_is_valid (tile_pixbuf));
	g_return_if_fail (!art_irect_empty (&tile_bounds));
	g_return_if_fail (!art_irect_empty (&dirty_area));
	
	/* Clip the tile to the dirty area */
	art_irect_intersect (&tile_dirty_area, &tile_bounds, &dirty_area);
	
	/* Nothing to do */
 	if (art_irect_empty (&tile_dirty_area)) {
 		return;
 	}

	/* Tile the drawable. */
	eel_gdk_pixbuf_draw_to_drawable_tiled (tile_pixbuf,
					       widget->window,
					       gc,
					       tile_dirty_area,
					       gdk_pixbuf_get_width (tile_pixbuf),
					       gdk_pixbuf_get_height (tile_pixbuf),
					       tile_origin.x,
					       tile_origin.y,
					       GDK_RGB_DITHER_NONE,
					       GDK_PIXBUF_ALPHA_BILEVEL,
					       EEL_STANDARD_ALPHA_THRESHHOLD);
}

/* Paint the smooth widget's tile in transparent alpha composited mode */
static void
smooth_widget_paint_tile_transparent (GtkWidget *widget,
				      GdkGC *gc,
				      EelSmoothBackgroundMode background_mode,
				      guint32 solid_background_color,
				      const GdkPixbuf *tile_pixbuf,
				      ArtIRect tile_bounds,
				      int tile_opacity,
				      EelArtIPoint tile_origin,
				      ArtIRect dirty_area)
{
	EelDimensions buffer_dimensions;
 	ArtIRect tile_dirty_area;
	ArtIRect tile_area;
	GdkPixbuf *buffer;

	g_return_if_fail (widget_is_smooth (widget));
	g_return_if_fail (GTK_WIDGET_REALIZED (widget));
	g_return_if_fail (gc != NULL);
	g_return_if_fail (background_mode >= EEL_SMOOTH_BACKGROUND_GTK);
	g_return_if_fail (background_mode <= EEL_SMOOTH_BACKGROUND_SOLID_COLOR);
	g_return_if_fail (tile_opacity >= EEL_OPACITY_FULLY_TRANSPARENT);
	g_return_if_fail (tile_opacity <= EEL_OPACITY_FULLY_OPAQUE);
	g_return_if_fail (eel_gdk_pixbuf_is_valid (tile_pixbuf));
	g_return_if_fail (!art_irect_empty (&tile_bounds));
	g_return_if_fail (!art_irect_empty (&dirty_area));
	
	art_irect_intersect (&tile_dirty_area, &tile_bounds, &dirty_area);
	
	/* Nothing to do */
 	if (art_irect_empty (&tile_dirty_area)) {
 		return;
 	}

	buffer = smooth_widget_get_background (widget,
					       tile_dirty_area,
					       background_mode,
					       solid_background_color);

	g_return_if_fail (eel_gdk_pixbuf_is_valid (buffer));
	
	buffer_dimensions = eel_gdk_pixbuf_get_dimensions (buffer);
	
	tile_area = eel_art_irect_assign (0, 0, buffer_dimensions.width, buffer_dimensions.height);

	/* Composite the tile into the buffer */	
	eel_gdk_pixbuf_draw_to_pixbuf_tiled (tile_pixbuf,
					     buffer,
					     tile_area,
					     gdk_pixbuf_get_width (tile_pixbuf),
					     gdk_pixbuf_get_height (tile_pixbuf),
					     tile_origin.x - tile_dirty_area.x0,
					     tile_origin.y - tile_dirty_area.y0,
					     tile_opacity,
					     GDK_INTERP_NEAREST);
	
	eel_gdk_pixbuf_draw_to_drawable (buffer,
					 widget->window,
					 gc,
					 0,
					 0,
					 tile_dirty_area,
					 GDK_RGB_DITHER_NONE,
					 GDK_PIXBUF_ALPHA_BILEVEL,
					 EEL_STANDARD_ALPHA_THRESHHOLD);
	
	gdk_pixbuf_unref (buffer);
}

/* Paint the smooth widget's content in opaque mode */
static void
smooth_widget_paint_content_opaque (GtkWidget *widget,
				    GdkGC *gc,
				    ArtIRect content_bounds,
				    ArtIRect dirty_area,
				    EelSmoothPaintOpaqueCallback paint_callback,
				    gpointer callback_data)
{
	ArtIRect content_dirty_area;

	g_return_if_fail (widget_is_smooth (widget));
	g_return_if_fail (GTK_WIDGET_REALIZED (widget));
	g_return_if_fail (gc != NULL);
	g_return_if_fail (!art_irect_empty (&content_bounds));
	g_return_if_fail (!art_irect_empty (&dirty_area));
	g_return_if_fail (paint_callback != NULL);

	/* Clip the content to the dirty area */
	art_irect_intersect (&content_dirty_area, &content_bounds, &dirty_area);
	
	/* No work to do */
	if (art_irect_empty (&content_dirty_area)) {
		return;
	}

	/* Draw content to screen */
	(* paint_callback) (widget,
			    widget->window,
			    gc,
			    content_dirty_area.x0 - content_bounds.x0,
			    content_dirty_area.y0 - content_bounds.y0,
			    content_dirty_area,
			    callback_data);
}

/* Paint the smooth widget's content in transparent alpha composited mode */
static void
smooth_widget_paint_content_transparent (GtkWidget *widget,
					 GdkGC *gc,
					 EelSmoothBackgroundMode background_mode,
					 guint32 solid_background_color,
					 ArtIRect content_bounds,
					 int content_opacity,
					 ArtIRect dirty_area,
					 EelSmoothCompositeCallback composite_callback,
					 gpointer callback_data)
{
	ArtIRect content_dirty_area;
	ArtIRect composite_area;
	GdkPixbuf *buffer;

	g_return_if_fail (widget_is_smooth (widget));
	g_return_if_fail (GTK_WIDGET_REALIZED (widget));
	g_return_if_fail (gc != NULL);
	g_return_if_fail (background_mode >= EEL_SMOOTH_BACKGROUND_GTK);
	g_return_if_fail (background_mode <= EEL_SMOOTH_BACKGROUND_SOLID_COLOR);
	g_return_if_fail (content_opacity >= EEL_OPACITY_FULLY_TRANSPARENT);
	g_return_if_fail (content_opacity <= EEL_OPACITY_FULLY_OPAQUE);
	g_return_if_fail (!art_irect_empty (&content_bounds));
	g_return_if_fail (!art_irect_empty (&dirty_area));
	g_return_if_fail (composite_callback != NULL);

	art_irect_intersect (&content_dirty_area, &content_bounds, &dirty_area);

	if (art_irect_empty (&content_dirty_area)) {
		return;
	}

	buffer = smooth_widget_get_background (widget,
					       content_dirty_area,
					       background_mode,
					       solid_background_color);

 	g_return_if_fail (eel_gdk_pixbuf_is_valid (buffer));

	composite_area = eel_art_irect_assign (0,
					       0,
					       eel_art_irect_get_width (content_dirty_area),
					       eel_art_irect_get_height (content_dirty_area));
	
	/* Composite the content into the buffer */
	(* composite_callback) (widget,
				buffer,
				content_dirty_area.x0 - content_bounds.x0,
				content_dirty_area.y0 - content_bounds.y0,
				composite_area,
				content_opacity,
				callback_data);
	
	/* Draw the composited buffer to the screen */
	eel_gdk_pixbuf_draw_to_drawable (buffer,
					 widget->window,
					 gc,
					 0,
					 0,
					 content_dirty_area,
					 GDK_RGB_DITHER_NONE,
					 GDK_PIXBUF_ALPHA_BILEVEL,
					 EEL_STANDARD_ALPHA_THRESHHOLD);

	gdk_pixbuf_unref (buffer);
}

/* Paint both the smooth widget's tile and content in transparent alpha
 * composited mode. */
static void
smooth_widget_paint_tile_and_content_transparent (GtkWidget *widget,
						  GdkGC *gc,
						  EelSmoothBackgroundMode background_mode,
						  guint32 solid_background_color,
						  const GdkPixbuf *tile_pixbuf,
						  ArtIRect tile_bounds,
						  int tile_opacity,
						  EelArtIPoint tile_origin,
						  ArtIRect content_bounds,
						  int content_opacity,
						  ArtIRect dirty_area,
						  EelSmoothCompositeCallback composite_callback,
						  gpointer callback_data)
{
 	ArtIRect tile_dirty_area;
 	ArtIRect content_dirty_area;
 	ArtIRect tile_and_content_intersection;
 	ArtIRect tile_and_content_union;

	g_return_if_fail (widget_is_smooth (widget));
	g_return_if_fail (GTK_WIDGET_REALIZED (widget));
	g_return_if_fail (gc != NULL);
	g_return_if_fail (background_mode >= EEL_SMOOTH_BACKGROUND_GTK);
	g_return_if_fail (background_mode <= EEL_SMOOTH_BACKGROUND_SOLID_COLOR);
	g_return_if_fail (tile_opacity >= EEL_OPACITY_FULLY_TRANSPARENT);
	g_return_if_fail (tile_opacity <= EEL_OPACITY_FULLY_OPAQUE);
	g_return_if_fail (eel_gdk_pixbuf_is_valid (tile_pixbuf));
	g_return_if_fail (!art_irect_empty (&tile_bounds));
	g_return_if_fail (content_opacity >= EEL_OPACITY_FULLY_TRANSPARENT);
	g_return_if_fail (content_opacity <= EEL_OPACITY_FULLY_OPAQUE);
	g_return_if_fail (!art_irect_empty (&content_bounds));
	g_return_if_fail (!art_irect_empty (&dirty_area));
	g_return_if_fail (composite_callback != NULL);

	art_irect_intersect (&tile_dirty_area, &tile_bounds, &dirty_area);
	art_irect_intersect (&content_dirty_area, &content_bounds, &dirty_area);

	/* Both empty, do nothing */
 	if (art_irect_empty (&tile_dirty_area) && art_irect_empty (&content_dirty_area)) {
 		return;
 	}

	/* Only the pixbuf is dirty */
 	if (art_irect_empty (&tile_dirty_area) && !art_irect_empty (&content_dirty_area)) {
		smooth_widget_paint_content_transparent (widget,
							 gc,
							 background_mode,
							 solid_background_color,
							 content_bounds,
							 content_opacity,
							 content_dirty_area,
							 composite_callback,
							 NULL);
 		return;
 	}

	/* Only the tile is dirty */
 	if (!art_irect_empty (&tile_dirty_area) && art_irect_empty (&content_dirty_area)) {
		smooth_widget_paint_tile_transparent (widget,
						      gc,
						      background_mode,
						      solid_background_color,
						      tile_pixbuf,
						      tile_bounds,
						      tile_opacity,
						      tile_origin,
						      tile_dirty_area);
		return;
 	}

	art_irect_intersect (&tile_and_content_intersection, &tile_dirty_area, &content_dirty_area);

	/* Both dirty without overlaping each other */
	if (art_irect_empty (&tile_and_content_intersection)) {
		smooth_widget_paint_tile_transparent (widget,
						      gc,
						      background_mode,
						      solid_background_color,
						      tile_pixbuf,
						      tile_bounds,
						      tile_opacity,
						      tile_origin,
						      tile_dirty_area);
		
		smooth_widget_paint_content_transparent (widget,
							 gc,
							 background_mode,
							 solid_background_color,
							 content_bounds,
							 content_opacity,
							 content_dirty_area,
							 composite_callback,
							 NULL);
		return;
	}

	/* Both dirty overlaping each other.  We use the union of the
	 * two rectangles even though its not the most efficient way.
	 */
	art_irect_union (&tile_and_content_union, &tile_dirty_area, &content_dirty_area);
	
	if (!art_irect_empty (&tile_and_content_union)) {
		GdkPixbuf *buffer;
		ArtIRect tile_area;
		ArtIRect composite_area;
		
		buffer = smooth_widget_get_background (widget,
						       tile_and_content_union,
						       background_mode,
						       solid_background_color);
		g_return_if_fail (eel_gdk_pixbuf_is_valid (buffer));
		
		tile_area = eel_art_irect_assign (tile_dirty_area.x0 - tile_and_content_union.x0,
						  tile_dirty_area.y0 - tile_and_content_union.y0,
						  eel_art_irect_get_width (tile_dirty_area),
						  eel_art_irect_get_height (tile_dirty_area));
		
		/* Composite the tile into the buffer */
		eel_gdk_pixbuf_draw_to_pixbuf_tiled (tile_pixbuf,
						     buffer,
						     tile_area,
						     gdk_pixbuf_get_width (tile_pixbuf),
						     gdk_pixbuf_get_height (tile_pixbuf),
						     tile_origin.x - tile_and_content_union.x0,
						     tile_origin.y - tile_and_content_union.y0,
						     tile_opacity,
						     GDK_INTERP_NEAREST);

		composite_area = eel_art_irect_assign (content_dirty_area.x0 - tile_dirty_area.x0,
						       content_dirty_area.y0 - tile_dirty_area.y0,
						       eel_art_irect_get_width (content_dirty_area),
						       eel_art_irect_get_height (content_dirty_area));
		
		/* Composite the content into the buffer */
		(* composite_callback) (widget,
					buffer,
					content_dirty_area.x0 - content_bounds.x0,
					content_dirty_area.y0 - content_bounds.y0,
					composite_area,
					content_opacity,
					callback_data);

		/* Draw the composited buffer to the screen */
		eel_gdk_pixbuf_draw_to_drawable (buffer,
						 widget->window,
						 gc,
						 0,
						 0,
						 tile_and_content_union,
						 GDK_RGB_DITHER_NONE,
						 GDK_PIXBUF_ALPHA_BILEVEL,
						 EEL_STANDARD_ALPHA_THRESHHOLD);

		gdk_pixbuf_unref (buffer);
	}
}

/**
 * eel_smooth_widget_paint:
 * @widget: A smooth widget.
 * @gc: GdkGC to use for copy area.
 * @is_smooth: Is the widget smooth ?
 * @background_mode: Background mode.
 * @solid_background_color: Background color for the case when
 * background_mode is EEL_SMOOTH_BACKGROUND_SOLID_COLOR.
 * @tile_pixbuf: Pixbuf to use for tiling or NULL.
 * @tile_bounds: The bounds of the tile or eel_art_irect_empty.
 * @tile_opacity: The opacity for the tile when is_smooth is TRUE.
 * @tile_mode_vertical: The vertical tiliing mode.
 * @tile_mode_horizontal: The horizontal tiliing mode.
 * @content_bounds: The bounds of the content or eel_art_irect_empty.
 * @content_opacity: The opacity for the content when is_smooth is TRUE.
 * @dirty_area: The dirty area, usually from expose_event().
 * @paint_callback: Content painting callback for when is_smooth is FALSE.
 * @composite_callback: Content compositing callback for when is_smooth is TRUE.
 * @callback_data: Data passed to the callbacks.
 *
 * Paint a smooth widget.  If the 'is_smooth' flag is TRUE, then all painting
 * will be done using alpha compositions.  In this mode the tile_opacity
 * and pixbuf_opacity are honored.  This mode is significantly slower.
 *
 * If the 'is_smooth' flag is FALSE, then all painting will be done
 * using simple copy_area operations.  This is mode is very fast
 * but doesnt look as good.  In this mode, the tile_opacity and
 * pixbuf_opacity are ignored.
 *
 * Also, when 'is_smooth' is TRUE, the 'background_mode' has a huge effect
 * on the performance of this function.  Here we list the 3 background modes
 * in order of SLOWEST to FASTEST:
 *
 * EEL_SMOOTH_BACKGROUND_GTK:
 *   Use the background as painted by Gtk.  This includes arbitrarily complex
 *   gradients, images and solid colors.  Using this mode will ensure that
 *   the content you are painting is smoothly displayed on top of anything
 *   that is rendered by Gtk.  Unfortunately, for this mode to work a lot
 *   of information needs to be transfered back and forth to the X server.
 *   That means it is very slow.  If you "know" what the contents of the 
 *   surface you are painting on, use on of the other two modes.
 *
 * EEL_SMOOTH_BACKGROUND_CALLBACK:
 *   Use a callback to draw the background.  This makes is possible to use 
 *   already existing background rendering code.  For example, Eel 
 *   uses a EelBackground object that can be used as a callback client
 *   for this mode.  The callback is implemented as a widget signal.  The
 *   signature of the callback is 'EelSmoothWidgetDrawBackground'.
 *
 * EEL_SMOOTH_BACKGROUND_SOLID_COLOR:
 *   Use a solid color for the background.  This is the fastest mode.  It also
 *   has the benefit that since the background is always known, the content
 *   you are rendering can be cached.
 *
 * If a tile_pixbuf is given, then it is rendered according to the value of
 * 'tile_mode_vertical' and 'tile_mode_horizontal'   Here we list the 2
 * valid tiling modes:
 *
 * The 'paint_callback' is invoked when not it smooth mode.
 * The 'composite_callback' is invoked when in smooth mode.
 */
void
eel_smooth_widget_paint (GtkWidget *widget,
			 GdkGC *gc,
			 gboolean is_smooth,
			 EelSmoothBackgroundMode background_mode,
			 guint32 solid_background_color,
			 const GdkPixbuf *tile_pixbuf,
			 ArtIRect tile_bounds,
			 int tile_opacity,
			 EelSmoothTileMode tile_mode_vertical,
			 EelSmoothTileMode tile_mode_horizontal,
			 ArtIRect content_bounds,
			 int content_opacity,
			 ArtIRect dirty_area,
			 EelSmoothPaintOpaqueCallback paint_callback,
			 EelSmoothCompositeCallback composite_callback,
			 gpointer callback_data)
{
	gboolean tile_empty;
	gboolean content_empty;
	EelArtIPoint tile_origin;

	g_return_if_fail (widget_is_smooth (widget));
	g_return_if_fail (GTK_WIDGET_REALIZED (widget));
	g_return_if_fail (gc != NULL);
	g_return_if_fail (background_mode >= EEL_SMOOTH_BACKGROUND_GTK);
	g_return_if_fail (background_mode <= EEL_SMOOTH_BACKGROUND_SOLID_COLOR);
	g_return_if_fail (tile_opacity >= EEL_OPACITY_FULLY_TRANSPARENT);
	g_return_if_fail (tile_opacity <= EEL_OPACITY_FULLY_OPAQUE);
	g_return_if_fail (tile_mode_vertical >= EEL_SMOOTH_TILE_SELF);
	g_return_if_fail (tile_mode_vertical <= EEL_SMOOTH_TILE_ANCESTOR);
	g_return_if_fail (tile_mode_horizontal >= EEL_SMOOTH_TILE_SELF);
	g_return_if_fail (tile_mode_horizontal <= EEL_SMOOTH_TILE_ANCESTOR);
	g_return_if_fail (content_opacity >= EEL_OPACITY_FULLY_TRANSPARENT);
	g_return_if_fail (content_opacity <= EEL_OPACITY_FULLY_OPAQUE);
	g_return_if_fail (!art_irect_empty (&dirty_area));
	g_return_if_fail (paint_callback != NULL);
	g_return_if_fail (composite_callback != NULL);
	
	tile_empty = art_irect_empty (&tile_bounds);
	content_empty = art_irect_empty (&content_bounds);

	tile_origin = smooth_widget_get_tile_origin_point (widget,
							   tile_mode_vertical,
							   tile_mode_horizontal);

	/* Smooth */
	if (is_smooth) {
		if (!content_empty && !tile_empty) {
			smooth_widget_paint_tile_and_content_transparent (widget,
									  gc,
									  background_mode,
									  solid_background_color,
									  tile_pixbuf,
									  tile_bounds,
									  tile_opacity,
									  tile_origin,
									  content_bounds,
									  content_opacity,
									  dirty_area,
									  composite_callback,
									  callback_data);
			
		} else if (!content_empty) {
			smooth_widget_paint_content_transparent (widget,
								 gc,
								 background_mode,
								 solid_background_color,
								 content_bounds,
								 content_opacity,
								 dirty_area,
								 composite_callback,
								 callback_data);
		} else {
			smooth_widget_paint_tile_transparent (widget,
							      gc,
							      background_mode,
							      solid_background_color,
							      tile_pixbuf,
							      tile_bounds,
							      tile_opacity,
							      tile_origin,
							      dirty_area);
		}
		/* Not smooth */
	} else {
		/* Draw the tile if needed */
		if (!tile_empty) {
			smooth_widget_paint_tile_opaque (widget,
							 gc,
							 tile_pixbuf,
							 tile_bounds,
							 tile_origin,
							 dirty_area);
		}
		
		/* Drae the content if needed */
		if (!content_empty) {
			smooth_widget_paint_content_opaque (widget,
							    gc,
							    content_bounds,
							    dirty_area,
							    paint_callback,
							    callback_data);
		}
	}
}

/**
 * eel_smooth_widget_get_tile_bounds:
 * @widget: A smooth widget.
 * @tile_pixbuf: The tile pixbuf.
 * @tile_width: The tile width.
 * @tile_height: The tile height.
 *
 * Return the bounds for the smooth widget's tile.  
 *
 * tile_width controls the horizontal extent of the tile.
 *
 * tile_height controls the vertical extent of the tile.
 *
 * tile_width/tile_height can be integer values greater than
 * zero.  We also allow two special values:
 *
 * EEL_SMOOTH_TILE_EXTENT_FULL:
 *   Tile occupies the full extent available either horizontall (tile_width)
 *   or vertically (tile_height).  This basically mean that the tile occupies
 *   the full 'GtkWidget allocation'
 *
 * EEL_SMOOTH_TILE_EXTENT_ONE_STEP:
 *   Tile occupies a single tile step.  A tile step is equal to the width and
 *   height of the tile pixbuf for the horizontal and veritcal cases 
 *   respectively.
 *
 */
ArtIRect
eel_smooth_widget_get_tile_bounds (const GtkWidget *widget,
				   const GdkPixbuf *tile_pixbuf,
				   int tile_width,
				   int tile_height)
{
	ArtIRect bounds;
	ArtIRect tile_bounds;
	ArtIRect clipped_tile_bounds;

	g_return_val_if_fail (widget_is_smooth (widget), eel_art_irect_empty);
	g_return_val_if_fail (tile_width >= EEL_SMOOTH_TILE_EXTENT_ONE_STEP, eel_art_irect_empty);
	g_return_val_if_fail (tile_height >= EEL_SMOOTH_TILE_EXTENT_ONE_STEP, eel_art_irect_empty);

	if (tile_pixbuf == NULL) {
		return eel_art_irect_empty;
	}

	tile_bounds.x0 = widget->allocation.x;
	tile_bounds.y0 = widget->allocation.y;
	
	if (tile_width == EEL_SMOOTH_TILE_EXTENT_ONE_STEP) {
		tile_bounds.x1 = tile_bounds.x0 + gdk_pixbuf_get_width (tile_pixbuf);
	} else if (tile_width == EEL_SMOOTH_TILE_EXTENT_FULL) {
		tile_bounds.x1 = tile_bounds.x0 + widget->allocation.width;
	} else {
		tile_bounds.x1 = tile_bounds.x0 + tile_width;
	}

	if (tile_height == EEL_SMOOTH_TILE_EXTENT_ONE_STEP) {
		tile_bounds.y1 = tile_bounds.y0 + gdk_pixbuf_get_height (tile_pixbuf);
	} else if (tile_height == EEL_SMOOTH_TILE_EXTENT_FULL) {
		tile_bounds.y1 = tile_bounds.y0 + widget->allocation.height;
	} else {
		tile_bounds.y1 = tile_bounds.y0 + tile_height;
	}

	/* Clip the tile bounds to the widget bounds */
	bounds = eel_gtk_widget_get_bounds (widget);
	art_irect_intersect (&clipped_tile_bounds, &tile_bounds, &bounds);

	return tile_bounds;
}

EelDimensions
eel_smooth_widget_get_preferred_dimensions (const GtkWidget *widget,
					    EelDimensions content_dimensions,
					    EelDimensions tile_dimensions,
					    int tile_width,
					    int tile_height)
{
	EelDimensions preferred_dimensions;

	g_return_val_if_fail (widget_is_smooth (widget), eel_dimensions_empty);
	g_return_val_if_fail (content_dimensions.width >= 0, eel_dimensions_empty);
	g_return_val_if_fail (content_dimensions.height >= 0, eel_dimensions_empty);
	g_return_val_if_fail (tile_dimensions.width >= 0, eel_dimensions_empty);
	g_return_val_if_fail (tile_dimensions.height >= 0, eel_dimensions_empty);
	g_return_val_if_fail (tile_width >= EEL_SMOOTH_TILE_EXTENT_ONE_STEP, eel_dimensions_empty);
	g_return_val_if_fail (tile_height >= EEL_SMOOTH_TILE_EXTENT_ONE_STEP, eel_dimensions_empty);
	
	if (tile_width == EEL_SMOOTH_TILE_EXTENT_ONE_STEP) {
		tile_width = tile_dimensions.width;
	} else {
		tile_width = 0;
	}

	if (tile_height == EEL_SMOOTH_TILE_EXTENT_ONE_STEP) {
		tile_height = tile_dimensions.height;
	} else {
		tile_height = 0;
	}
	
	preferred_dimensions.width = MAX (content_dimensions.width, tile_width) + (2 * GTK_MISC (widget)->xpad);
	preferred_dimensions.height = MAX (content_dimensions.height, tile_height) + (2 * GTK_MISC (widget)->ypad);

	/* Make sure the dimensions is not zero.  Gtk goes berserk with zero size widget */
	preferred_dimensions.width = MAX (preferred_dimensions.width, 2);
	preferred_dimensions.height = MAX (preferred_dimensions.height, 2);

	return preferred_dimensions;
}

void
eel_smooth_widget_register_type (GtkType type)
{
	if (smooth_widget_type_list == NULL) {
		g_atexit (smooth_widget_type_list_free);
	}

	smooth_widget_type_list = g_list_append (smooth_widget_type_list, GINT_TO_POINTER (type));
}
