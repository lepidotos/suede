/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */

/* eel-gnome-extensions.c - implementation of new functions that operate on
                            gnome classes. Perhaps some of these should be
  			    rolled into gnome someday.

   Copyright (C) 1999, 2000, 2001 Eazel, Inc.

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

   Authors: Darin Adler <darin@eazel.com>
*/

#include <config.h>
#include "eel-gnome-extensions.h"

#include "eel-art-extensions.h"
#include "eel-gdk-extensions.h"
#include "eel-glib-extensions.h"
#include "eel-stock-dialogs.h"
#include <X11/Xatom.h>
#include <errno.h>
#include <fcntl.h>
#include <gdk/gdkx.h>
#include <gtk/gtkwidget.h>
#include <libart_lgpl/art_rect.h>
#include <libart_lgpl/art_rgb.h>
#include <libgnome/gnome-config.h>
#include <libgnome/gnome-i18n.h>
#include <libgnome/gnome-util.h>
#include <libgnomeui/gnome-file-entry.h>
#include <libgnomeui/gnome-icon-list.h>
#include <libgnomeui/gnome-icon-sel.h>
#include <limits.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>

/* structure for the icon selection dialog */
struct IconSelectionData {
        GtkWidget *icon_selection;
	GtkWidget *file_entry;
	GtkWidget *top_level_window;
	GtkWindow *owning_window;
	gboolean dismissed;
	EelIconSelectionFunction selection_function;
	gpointer callback_data;
};

typedef struct IconSelectionData IconSelectionData;

static Atom xa_win_area;

ArtIRect
eel_gnome_canvas_world_to_window_rectangle (const GnomeCanvas *canvas,
					    ArtDRect world_rect)
{
	double x0, y0, x1, y1;
	ArtIRect window_rect;

	g_return_val_if_fail (GNOME_IS_CANVAS (canvas), eel_art_irect_empty);

	gnome_canvas_world_to_window (GNOME_CANVAS (canvas),
				      world_rect.x0,
				      world_rect.y0,
				      &x0, &y0);
	gnome_canvas_world_to_window (GNOME_CANVAS (canvas),
				      world_rect.x1,
				      world_rect.y1,
				      &x1, &y1);

	window_rect.x0 = x0;
	window_rect.y0 = y0;
	window_rect.x1 = x1;
	window_rect.y1 = y1;

	return window_rect;
}

ArtIRect
eel_gnome_canvas_world_to_canvas_rectangle (const GnomeCanvas *canvas,
					    ArtDRect world_rect)
{
	ArtIRect canvas_rect;

	g_return_val_if_fail (GNOME_IS_CANVAS (canvas), eel_art_irect_empty);

	gnome_canvas_w2c (GNOME_CANVAS (canvas),
			  world_rect.x0,
			  world_rect.y0,
			  &canvas_rect.x0,
			  &canvas_rect.y0);
	gnome_canvas_w2c (GNOME_CANVAS (canvas),
			  world_rect.x1,
			  world_rect.y1,
			  &canvas_rect.x1,
			  &canvas_rect.y1);

	return canvas_rect;
}

ArtIRect
eel_gnome_canvas_item_get_current_canvas_bounds (const GnomeCanvasItem *item)
{
	ArtIRect bounds;

	g_return_val_if_fail (GNOME_IS_CANVAS_ITEM (item), eel_art_irect_empty);

	bounds.x0 = item->x1;
	bounds.y0 = item->y1;
	bounds.x1 = item->x2;
	bounds.y1 = item->y2;

	return bounds;
}

void
eel_gnome_canvas_item_request_redraw (GnomeCanvasItem *item)
{
	g_return_if_fail (GNOME_IS_CANVAS_ITEM (item));

	gnome_canvas_request_redraw (item->canvas,
				     item->x1, item->y1,
				     item->x2, item->y2);
}

void
eel_gnome_canvas_request_redraw_rectangle (GnomeCanvas *canvas,
					   ArtIRect canvas_rectangle)
{
	g_return_if_fail (GNOME_IS_CANVAS (canvas));
	
	gnome_canvas_request_redraw (canvas,
				     canvas_rectangle.x0, canvas_rectangle.y0,
				     canvas_rectangle.x1, canvas_rectangle.y1);
}

ArtDRect
eel_gnome_canvas_item_get_world_bounds (const GnomeCanvasItem *item)
{
	ArtDRect world_bounds;

	g_return_val_if_fail (GNOME_IS_CANVAS_ITEM (item), eel_art_drect_empty);

	gnome_canvas_item_get_bounds (GNOME_CANVAS_ITEM (item),
				      &world_bounds.x0,
				      &world_bounds.y0,
				      &world_bounds.x1,
				      &world_bounds.y1);
	if (item->parent != NULL) {
		gnome_canvas_item_i2w (item->parent,
				       &world_bounds.x0,
				       &world_bounds.y0);
		gnome_canvas_item_i2w (item->parent,
				       &world_bounds.x1,
				       &world_bounds.y1);
	}

	return world_bounds;
}

ArtIRect
eel_gnome_canvas_item_get_canvas_bounds (const GnomeCanvasItem *item)
{
	ArtDRect world_bounds;

	g_return_val_if_fail (GNOME_IS_CANVAS_ITEM (item), eel_art_irect_empty);

	world_bounds = eel_gnome_canvas_item_get_world_bounds (item);

	return eel_gnome_canvas_world_to_canvas_rectangle 
		(item->canvas, world_bounds);
}

static void
eel_gnome_canvas_draw_pixbuf_helper (art_u8 *dst, int dst_rowstride,
				     const art_u8 *src, int src_rowstride,
				     int copy_width, int copy_height)
{
	art_u8 *dst_limit = dst + copy_height * dst_rowstride;
	int dst_bytes_per_row = copy_width * 3;
	
	while (dst < dst_limit) {
 		memcpy (dst, src, dst_bytes_per_row);
		dst += dst_rowstride;
		src += src_rowstride;
	}
}

static void
eel_gnome_canvas_draw_pixbuf_helper_alpha (art_u8 *dst, int dst_rowstride,
					   const art_u8 *src, int src_rowstride,
					   int copy_width, int copy_height)
{
	art_u8 *dst_limit = dst + copy_height * dst_rowstride;
	int dst_bytes_per_row = copy_width * 3;
	
	while (dst < dst_limit) {
	
		art_u8 *dst_p = dst;
		art_u8 *dst_p_limit = dst + dst_bytes_per_row;
		
		const art_u8 *src_p = src;
		
		while (dst_p < dst_p_limit) {
			int alpha = src_p[3];
			if (alpha) {
				if (alpha == 255) {
					dst_p[0] = src_p[0];
					dst_p[1] = src_p[1];
					dst_p[2] = src_p[2];
				} else {
		  			int tmp;
					art_u8 bg_r = dst_p[0];
					art_u8 bg_g = dst_p[1];
					art_u8 bg_b = dst_p[2];

					tmp = (src_p[0] - bg_r) * alpha;
					dst_p[0] = bg_r + ((tmp + (tmp >> 8) + 0x80) >> 8);
					tmp = (src_p[1] - bg_g) * alpha;
					dst_p[1] = bg_g + ((tmp + (tmp >> 8) + 0x80) >> 8);
					tmp = (src_p[2] - bg_b) * alpha;
					dst_p[2] = bg_b + ((tmp + (tmp >> 8) + 0x80) >> 8);		  
				}
			}
			
			dst_p += 3;
			src_p += 4;
		}
		
		dst += dst_rowstride;
		src += src_rowstride;
	}
}

/* Draws a pixbuf into a canvas update buffer (unscaled). The x,y coords are the location
 * of the pixbuf in canvas space (NOT relative to the canvas buffer).
 */
void
eel_gnome_canvas_draw_pixbuf (GnomeCanvasBuf *buf, const GdkPixbuf *pixbuf, int x, int y)
{
	art_u8 *dst;
	int pixbuf_width, pixbuf_height;

	/* copy_left/top/right/bottom define the rect of the pixbuf (pixbuf relative)
	 * we will copy into the canvas buffer
	 */
	int copy_left, copy_top, copy_right, copy_bottom;
	
	dst = buf->buf;

	pixbuf_width = gdk_pixbuf_get_width (pixbuf);
	pixbuf_height = gdk_pixbuf_get_height (pixbuf);

	if (x > buf->rect.x0) {
		copy_left = 0;
		dst += (x - buf->rect.x0) * 3;
	} else {
		copy_left = buf->rect.x0 - x;
	}
	
	if (x + pixbuf_width > buf->rect.x1) {
		copy_right = buf->rect.x1 - x;
	} else {
		copy_right = pixbuf_width;		
	}
	
	if (copy_left >= copy_right) {
		return;
	}
	
	if (y > buf->rect.y0) {
		dst += (y - buf->rect.y0) * buf->buf_rowstride;
		copy_top = 0;
	} else {
		copy_top = buf->rect.y0 - y;
	}
	
	if (y + pixbuf_height > buf->rect.y1) {
		copy_bottom = buf->rect.y1 - y;
	} else {
		copy_bottom = pixbuf_height;		
	}

	if (copy_top >= copy_bottom) {
		return;
	}

	if (gdk_pixbuf_get_has_alpha (pixbuf)) {
		eel_gnome_canvas_draw_pixbuf_helper_alpha (
			dst,
			buf->buf_rowstride,
			gdk_pixbuf_get_pixels (pixbuf) + copy_left * 4 + copy_top * gdk_pixbuf_get_rowstride (pixbuf),
			gdk_pixbuf_get_rowstride (pixbuf),
			copy_right - copy_left,
			copy_bottom - copy_top);
	} else {
		eel_gnome_canvas_draw_pixbuf_helper (
			dst,
			buf->buf_rowstride,
			gdk_pixbuf_get_pixels (pixbuf) + copy_left * 3 + copy_top * gdk_pixbuf_get_rowstride (pixbuf),
			gdk_pixbuf_get_rowstride (pixbuf),
			copy_right - copy_left,
			copy_bottom - copy_top);
	}
}

void
eel_gnome_canvas_fill_rgb (GnomeCanvasBuf *buf, art_u8 r, art_u8 g, art_u8 b)
{
	art_u8 *dst = buf->buf;
	int width = buf->rect.x1 - buf->rect.x0;
	int height = buf->rect.y1 - buf->rect.y0;

	if (buf->buf_rowstride == width * 3) {
	 	art_rgb_fill_run (dst, r, g, b, width * height);
	} else {
		art_u8 *dst_limit = dst + height * buf->buf_rowstride;
		while (dst < dst_limit) {
	 		art_rgb_fill_run (dst, r, g, b, width);
			dst += buf->buf_rowstride;
		}
	}
}

GtkButton *
eel_gnome_dialog_get_button_by_index (GnomeDialog *dialog, int index)
{
	gpointer data;

	g_return_val_if_fail (GNOME_IS_DIALOG (dialog), NULL);
	g_return_val_if_fail (index >= 0, NULL);

	data = g_list_nth_data (GNOME_DIALOG (dialog)->buttons, index);
	if (data == NULL) {
		return NULL;
	}

	return GTK_BUTTON (data);
}

void
eel_gnome_canvas_item_request_update_deep (GnomeCanvasItem *item)
{
	GList *p;

	gnome_canvas_item_request_update (item);
	if (GNOME_IS_CANVAS_GROUP (item)) {
		for (p = GNOME_CANVAS_GROUP (item)->item_list; p != NULL; p = p->next) {
			eel_gnome_canvas_item_request_update_deep (p->data);
		}
	}
}

void
eel_gnome_canvas_request_update_all (GnomeCanvas *canvas)
{
	eel_gnome_canvas_item_request_update_deep (canvas->root);
}

/* The gnome_canvas_set_scroll_region function doesn't do an update,
 * even though it should. The update is in there with an #if 0 around
 * it, with no explanation of why it's commented out. For now, work
 * around this by requesting an update explicitly.
 */
void
eel_gnome_canvas_set_scroll_region (GnomeCanvas *canvas,
				    double x1, double y1,
				    double x2, double y2)
{
	double old_x1, old_y1, old_x2, old_y2;

	/* Change the scroll region and do an update if it changes. */
	gnome_canvas_get_scroll_region (canvas, &old_x1, &old_y1, &old_x2, &old_y2);
	if (old_x1 != x1 || old_y1 != y1 || old_x2 != x2 || old_y2 != y2) {
		gnome_canvas_set_scroll_region (canvas, x1, y1, x2, y2);
		eel_gnome_canvas_request_update_all (canvas);
		gnome_canvas_item_request_update (canvas->root);
	}
}

/* The code in GnomeCanvas (the scroll_to function to be exact) always
 * centers the contents of the canvas if the contents are smaller than
 * the canvas, and it does some questionable math when computing
 * that. This code is working to undo that mistake.
 */
void
eel_gnome_canvas_set_scroll_region_left_justify (GnomeCanvas *canvas,
						 double x1, double y1,
						 double x2, double y2)
{
	double height, width;

	/* To work around the logic in scroll_to that centers the
	 * canvas contents if they are smaller than the canvas widget,
	 * we must do the exact opposite of what it does. The -1 here
	 * is due to the ill-conceived ++ in scroll_to.
	 */
	width = (GTK_WIDGET (canvas)->allocation.width - 1) / canvas->pixels_per_unit;
	height = (GTK_WIDGET (canvas)->allocation.height - 1) / canvas->pixels_per_unit;
	eel_gnome_canvas_set_scroll_region
		(canvas, x1, y1,
		 MAX (x2, x1 + width), MAX (y2, y1 + height));
}

/* Set a new scroll region without eliminating any of the currently-visible area. */
void
eel_gnome_canvas_set_scroll_region_include_visible_area (GnomeCanvas *canvas,
							 double x1, double y1,
							 double x2, double y2)
{
	double old_x1, old_y1, old_x2, old_y2;
	double old_scroll_x, old_scroll_y;
	double height, width;

	gnome_canvas_get_scroll_region (canvas, &old_x1, &old_y1, &old_x2, &old_y2);

	/* The -1 here is due to the ill-conceived ++ in scroll_to. */
	width = (GTK_WIDGET (canvas)->allocation.width - 1) / canvas->pixels_per_unit;
	height = (GTK_WIDGET (canvas)->allocation.height - 1) / canvas->pixels_per_unit;

	old_scroll_x = gtk_layout_get_hadjustment (GTK_LAYOUT (canvas))->value;
	old_scroll_y = gtk_layout_get_vadjustment (GTK_LAYOUT (canvas))->value;

	x1 = MIN (x1, old_x1 + old_scroll_x);
	y1 = MIN (y1, old_y1 + old_scroll_y);
	x2 = MAX (x2, old_x1 + old_scroll_x + width);
	y2 = MAX (y2, old_y1 + old_scroll_y + height);

	eel_gnome_canvas_set_scroll_region
		(canvas, x1, y1, x2, y2);
}


/* Code from GMC, contains all the voodoo needed to start
 * a terminal from the file manager nicely
 */

static int
max_open_files (void)
{
	static int files;

	if (files != 0) {
		return files;
	}

#ifdef HAVE_SYSCONF
	files = sysconf (_SC_OPEN_MAX);
	if (files != -1) {
		return files;
	}
#endif
#ifdef OPEN_MAX
	files = OPEN_MAX;
#else
	files = 256;
#endif
	return files;
}

int 
eel_gnome_shell_execute (const char *command)
{
	struct sigaction ignore, save_intr, save_quit, save_stop;
	int status, i;
	int pid;
	
	ignore.sa_handler = SIG_IGN;
	sigemptyset (&ignore.sa_mask);
	ignore.sa_flags = 0;
	status = 0;
    
	sigaction (SIGINT, &ignore, &save_intr);    
	sigaction (SIGQUIT, &ignore, &save_quit);

	pid = fork ();
	if (pid < 0){
		return -1;
	}
	
	if (pid == 0){
		int top;
		struct sigaction default_pipe;

		top = max_open_files ();
		sigaction (SIGINT,  &save_intr, NULL);
		sigaction (SIGQUIT, &save_quit, NULL);

		/*
		 * reset sigpipe
		 */
		default_pipe.sa_handler = SIG_DFL;
		sigemptyset (&default_pipe.sa_mask);
		default_pipe.sa_flags = 0;
		
		sigaction (SIGPIPE, &default_pipe, NULL);
		
		for (i = 0; i < top; i++)
			close (i);

		/* Setup the file descriptor for the child */
		   
		/* stdin */
		open ("/dev/null", O_APPEND);

		/* stdout */
		open ("/dev/null", O_RDONLY);

		/* stderr */
		open ("/dev/null", O_RDONLY);
		
		pid = fork ();
		if (pid == 0){
			execl ("/bin/sh", "/bin/sh", "-c", command, (char *) 0);
			/* See note below for why we use _exit () */
			_exit (127);		/* Exec error */
		}
		/* We need to use _exit instead of exit to avoid
		 * calling the atexit handlers (specifically the gdk atexit
		 * handler
		 */
		_exit (0);
	}
	waitpid (pid, &status, 0);
	sigaction (SIGINT,  &save_intr, NULL);
	sigaction (SIGQUIT, &save_quit, NULL);
	sigaction (SIGTSTP, &save_stop, NULL);

	return WEXITSTATUS(status);
}

/* Return a command string containing the path to a terminal on this system. */

static char *
try_terminal_command (const char *program,
		      const char *args)
{
	char *program_in_path, *quoted, *result;

	if (program == NULL) {
		return NULL;
	}

	program_in_path = gnome_is_program_in_path (program);
	if (program_in_path == NULL) {
		return NULL;
	}

	quoted = eel_shell_quote (program_in_path);
	if (args == NULL || args[0] == '\0') {
		return quoted;
	}
	result = g_strconcat (quoted, " ", args, NULL);
	g_free (quoted);
	return result;
}

static char *
try_terminal_command_argv (int argc,
			   char **argv)
{
	GString *string;
	int i;
	char *quoted, *result;

	if (argc == 0) {
		return NULL;
	}

	if (argc == 1) {
		return try_terminal_command (argv[0], NULL);
	}
	
	string = g_string_new (argv[1]);
	for (i = 2; i < argc; i++) {
		quoted = eel_shell_quote (argv[i]);
		g_string_append_c (string, ' ');
		g_string_append (string, quoted);
		g_free (quoted);
	}
	result = try_terminal_command (argv[0], string->str);
	g_string_free (string, TRUE);

	return result;
}

static char *
get_terminal_command_prefix (gboolean for_command)
{
	int argc;
	char **argv;
	char *command;
	guint i;
	static const char *const commands[][3] = {
		{ "gnome-terminal", "--start-factory-server --use-factory -x", "--login" },
		{ "dtterm",         "-e",                                      "-ls" },
		{ "nxterm",         "-e",                                      "-ls" },
		{ "color-xterm",    "-e",                                      "-ls" },
		{ "rxvt",           "-e",                                      "-ls" },
		{ "xterm",          "-e",                                      "-ls" },
	};

	/* Try the terminal from preferences. Use without any
	 * arguments if we are just doing a standalone terminal.
	 */
	gnome_config_get_vector ("/Gnome/Applications/Terminal",
				 &argc, &argv);
	if (argc != 0) {
		if (for_command) {
			command = try_terminal_command_argv (argc, argv);
		} else {
			/* Strip off the arguments in a lame attempt
			 * to make it be an interactive shell.
			 */
			command = try_terminal_command (argv[0], NULL);
		}
		if (command != NULL) {
			return command;
		}
	}

	/* Try well-known terminal applications in same order that gmc did. */
	for (i = 0; i < EEL_N_ELEMENTS (commands); i++) {
		command = try_terminal_command (commands[i][0],
						commands[i][for_command ? 1 : 2]);
		if (command != NULL) {
			return command;
		}
	}
	
	return NULL;
}

char *
eel_gnome_make_terminal_command (const char *command)
{
	char *prefix, *quoted, *terminal_command;

	if (command == NULL) {
		return get_terminal_command_prefix (FALSE);
	}
	prefix = get_terminal_command_prefix (TRUE);
	quoted = eel_shell_quote (command);
	terminal_command = g_strconcat (prefix, " /bin/sh -c ", quoted, NULL);
	g_free (prefix);
	g_free (quoted);
	return terminal_command;
}

void
eel_gnome_open_terminal (const char *command)
{
	char *command_line;
	
	command_line = eel_gnome_make_terminal_command (command);
	if (command_line == NULL) {
		g_message ("Could not start a terminal");
		return;
	}
	eel_gnome_shell_execute (command_line);
	g_free (command_line);
}

/* create a new icon selection dialog */


static gboolean
widget_destroy_callback (gpointer callback_data)
{
	IconSelectionData *selection_data = (IconSelectionData*) callback_data;
	gtk_widget_destroy (selection_data->top_level_window);
	g_free (selection_data);	
	return FALSE;
}

/* set the image of the file object to the selected file */
static void
icon_selected_callback (GtkWidget *button, IconSelectionData *selection_data)
{
	const gchar *icon_path;
	GtkWidget *entry;
	struct stat buf;
	
	gnome_icon_selection_stop_loading ( GNOME_ICON_SELECTION (selection_data->icon_selection));

	entry = gnome_file_entry_gtk_entry (GNOME_FILE_ENTRY (selection_data->file_entry));
	icon_path = gtk_entry_get_text (GTK_ENTRY (entry));

	/* if a specific file wasn't selected, put up a dialog to tell the
	 * user to pick something, and leave the picker up
	 */
	stat (icon_path, &buf);
	if (S_ISDIR (buf.st_mode)) {
		eel_show_error_dialog (_("No image was selected.  You must click on an image to select it."),
				       _("No selection made"),
				       selection_data->owning_window);
	} else {	 
		/* invoke the callback to inform it of the file path */
		selection_data->selection_function (icon_path, selection_data->callback_data);
	}
		
	/* we have to get rid of the icon selection dialog, but we can't do it now, since the
	 * file entry might still need it.  Do it when the next idle rolls around
	 */ 
	selection_data->dismissed = TRUE;
	selection_data->top_level_window = gtk_widget_get_toplevel (button);
	gtk_idle_add ((GtkFunction) widget_destroy_callback, selection_data);
}

/* handle the cancel button being pressed */
static void
icon_cancel_pressed (GtkWidget *button, IconSelectionData *selection_data)
{
	/* nothing to do if it's already been dismissed */
	if (selection_data->dismissed) {
		return;
	}
	
	gnome_icon_selection_stop_loading(GNOME_ICON_SELECTION (selection_data->icon_selection));	
	/* remove pending idle routine, if necessary */
	g_free (selection_data);
	gtk_widget_destroy (gtk_widget_get_toplevel (button));
}

/* handle an icon being selected by updating the file entry */
static void
list_icon_selected_callback (GnomeIconList *gil, gint num, GdkEvent *event, IconSelectionData *selection_data)
{
	const gchar *icon;
	GtkWidget *entry;
	
	icon = gnome_icon_selection_get_icon (GNOME_ICON_SELECTION (selection_data->icon_selection), TRUE);

	if (icon != NULL) {
		entry = gnome_file_entry_gtk_entry (GNOME_FILE_ENTRY (selection_data->file_entry));
		gtk_entry_set_text(GTK_ENTRY (entry), icon);
	}

	/* handle double-clicks as if the user pressed OK */
	if(event && event->type == GDK_2BUTTON_PRESS && ((GdkEventButton *)event)->button == 1) {
		icon_selected_callback (selection_data->file_entry, selection_data);
	}
}

/* handle the file entry changing */
static void
entry_activated (GtkWidget *widget, IconSelectionData *selection_data)
{
	struct stat buf;
	GtkWidget *icon_selection;
	gchar *filename;

	filename = gtk_entry_get_text (GTK_ENTRY (widget));
	if (!filename) {
		return;
	}
	
	stat (filename, &buf);
	if (S_ISDIR (buf.st_mode)) {
		icon_selection = selection_data->icon_selection;
		gnome_icon_selection_clear (GNOME_ICON_SELECTION (icon_selection), TRUE);
		gnome_icon_selection_add_directory (GNOME_ICON_SELECTION (icon_selection), filename);
		gnome_icon_selection_show_icons(GNOME_ICON_SELECTION (icon_selection));
	} else {
		/* We pretend like ok has been called */
		icon_selected_callback (selection_data->file_entry, selection_data);
	}
}

/* here's the actual routine that creates the icon selector */

GtkWidget *
eel_gnome_icon_selector_new (const char *title,
			     const char *icon_directory,
			     GtkWindow *owner,
			     EelIconSelectionFunction selected,
			     gpointer callback_data)
{
	GtkWidget *dialog, *icon_selection;
	GtkWidget *entry, *file_entry;
	IconSelectionData *selection_data;
	
	dialog = gnome_dialog_new (title,
				   GNOME_STOCK_BUTTON_OK,
				   GNOME_STOCK_BUTTON_CANCEL,
				   NULL);

	gnome_dialog_close_hides (GNOME_DIALOG (dialog), TRUE);
	gnome_dialog_set_close  (GNOME_DIALOG(dialog), TRUE);

	gtk_window_set_policy (GTK_WINDOW (dialog), TRUE, TRUE, TRUE);

	icon_selection = gnome_icon_selection_new();

	file_entry = gnome_file_entry_new (NULL,NULL);
	
	selection_data = g_new0 (IconSelectionData, 1);
 	selection_data->icon_selection = icon_selection;
 	selection_data->file_entry = file_entry;
 	selection_data->owning_window = owner;
	selection_data->selection_function = selected;
 	selection_data->callback_data = callback_data;
 	
	gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(dialog)->vbox),
			   file_entry, FALSE, FALSE, 0);

	gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(dialog)->vbox),
			   icon_selection, TRUE, TRUE, 0);

	gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
	if (owner != NULL) {
		gtk_window_set_transient_for (GTK_WINDOW (dialog), owner);
 	}
 	gtk_window_set_wmclass (GTK_WINDOW (dialog), "file_selector", "Eel");
	gtk_widget_show_all (dialog);
	
	entry = gnome_file_entry_gtk_entry (GNOME_FILE_ENTRY (file_entry));
	
	if (icon_directory == NULL) {
		gtk_entry_set_text(GTK_ENTRY (entry), DATADIR "/pixmaps");
		gnome_icon_selection_add_directory(GNOME_ICON_SELECTION (icon_selection), DATADIR "/pixmaps");	
	} else {
		gtk_entry_set_text(GTK_ENTRY (entry), icon_directory);
		gnome_icon_selection_add_directory(GNOME_ICON_SELECTION (icon_selection), icon_directory);
	}
	
	gnome_icon_selection_show_icons( GNOME_ICON_SELECTION (icon_selection));	
	gnome_dialog_button_connect(GNOME_DIALOG(dialog), 
				    0, /* OK button */
				    GTK_SIGNAL_FUNC (icon_selected_callback),
				    selection_data);
	
	gnome_dialog_button_connect (GNOME_DIALOG(dialog), 
				     1, /* Cancel button */
				     GTK_SIGNAL_FUNC (icon_cancel_pressed),
				     selection_data);
	
	gtk_signal_connect_after(GTK_OBJECT (GNOME_ICON_SELECTION (selection_data->icon_selection)->gil),
				 "select_icon",
				 GTK_SIGNAL_FUNC(list_icon_selected_callback),
				 selection_data);

	gtk_signal_connect_while_alive( GTK_OBJECT (entry), "activate",
					GTK_SIGNAL_FUNC(entry_activated),
					selection_data, GTK_OBJECT (file_entry));

	return dialog;
}


/* Set an icon on GnomeStock widget.  If the setting fails register this
 * as a new file.  Returns FALSE if even that failed */
gboolean
eel_gnome_stock_set_icon_or_register (GnomeStock *stock, const char *icon)
{
	GnomeStockPixmapEntryPath *new_entry;

	g_return_val_if_fail (stock != NULL, FALSE);
	g_return_val_if_fail (GNOME_IS_STOCK (stock), FALSE);
	g_return_val_if_fail (icon != NULL, FALSE);

	/* if we can set the icon and everything goes well we have succeeded */
	if (gnome_stock_set_icon (stock, icon)) {
		return TRUE;
	}

	/* If the icon string is not an existing file then this is just a
	 * bogus request and we return FALSE */
	if ( ! g_file_exists (icon)) {
		return FALSE;
	}

	/* If icon exists but gnome_stock_set_icon fails,
	 * that means this file has NOT been registered with
	 * gnome stock.  Unfortunately gnome_stock is a
	 * worthless pile of dung and doesn't do this for us.
	 * Do note however that it DOES register this stuff
	 * when it first creates the toolbars from
	 * GnomeUIInfo.  Go figure.
	 *
	 * Note that all this will be completely different in gnome 2.0
	 */

	new_entry = g_malloc (sizeof (GnomeStockPixmapEntryPath));
	new_entry->type = GNOME_STOCK_PIXMAP_TYPE_PATH;
	new_entry->label = NULL;
	new_entry->pathname = g_strdup (icon);
	new_entry->width = 0;
	new_entry->height = 0;

	/* Register this under the "icon" as that's what
	 * we'll look it up under later.
	 */
	gnome_stock_pixmap_register (icon, GNOME_STOCK_PIXMAP_REGULAR,
				     (GnomeStockPixmapEntry *) new_entry);

	return gnome_stock_set_icon (stock, icon);
}

static void
get_win_area (Window xid, int *area_x, int *area_y)
{
	Atom actual_type;
	int actual_format;
	gulong nitems, bytes_after;
	gulong *prop;

	if (xa_win_area == 0) {
		xa_win_area = XInternAtom (GDK_DISPLAY (), "_WIN_AREA", False);
	}

	gdk_error_trap_push ();

	if (XGetWindowProperty (GDK_DISPLAY (), xid,
				xa_win_area, 0, 2, False, AnyPropertyType,
				&actual_type, &actual_format, &nitems,
				&bytes_after, (guchar **) &prop) == Success
	    && nitems == 2) {
		*area_x = prop[0];
		*area_y = prop[1];
	}

	gdk_flush ();
	gdk_error_trap_pop ();
}

static void
set_win_area (Window xid, int area_x, int area_y)
{
	gulong data[2];

	data[0] = area_x;
	data[1] = area_y;

	if (xa_win_area == 0) {
		xa_win_area = XInternAtom (GDK_DISPLAY (), "_WIN_AREA", False);
	}

	gdk_error_trap_push ();

	XChangeProperty (GDK_DISPLAY (), xid, xa_win_area, XA_CARDINAL,
			 32, PropModeReplace, (guchar *) data, 2);

	gdk_flush ();
	gdk_error_trap_pop ();
}

void
eel_gnome_win_hints_get_area (GtkWidget *window, int *area_x, int *area_y)
{
	/* Default to 0,0. Should be mostly harmless */
	*area_x = 0;
	*area_y = 0;

	g_return_if_fail (GTK_WIDGET_REALIZED (window));

	get_win_area (GDK_WINDOW_XWINDOW (window->window), area_x, area_y);
}

void
eel_gnome_win_hints_get_current_area (int *area_x, int *area_y)
{
	*area_x = 0;
	*area_y = 0;

	get_win_area (RootWindow (GDK_DISPLAY (),
				  DefaultScreen (GDK_DISPLAY ())),
		      area_x, area_y);
}

void
eel_gnome_win_hints_set_area (GtkWidget *window, int area_x, int area_y)
{
	g_return_if_fail (GTK_WIDGET_REALIZED (window));

	set_win_area (GDK_WINDOW_XWINDOW (window->window), area_x, area_y);
}

void
eel_gnome_win_hints_set_current_area (int area_x, int area_y)
{
	set_win_area (RootWindow (GDK_DISPLAY (),
				  DefaultScreen (GDK_DISPLAY ())),
		      area_x, area_y);
}
