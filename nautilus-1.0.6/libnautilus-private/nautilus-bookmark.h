/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */

/* nautilus-bookmark.h - interface for individual bookmarks.

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

   Authors: John Sullivan <sullivan@eazel.com>
*/

#ifndef NAUTILUS_BOOKMARK_H
#define NAUTILUS_BOOKMARK_H

#include "nautilus-icon-factory.h"

#include <gtk/gtkwidget.h>
#include <gdk/gdktypes.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

typedef struct NautilusBookmark NautilusBookmark;

#define NAUTILUS_TYPE_BOOKMARK \
	(nautilus_bookmark_get_type ())
#define NAUTILUS_BOOKMARK(obj) \
	(GTK_CHECK_CAST ((obj), NAUTILUS_TYPE_BOOKMARK, NautilusBookmark))
#define NAUTILUS_BOOKMARK_CLASS(klass) \
	(GTK_CHECK_CLASS_CAST ((klass), NAUTILUS_TYPE_BOOKMARK, NautilusBookmarkClass))
#define NAUTILUS_IS_BOOKMARK(obj) \
	(GTK_CHECK_TYPE ((obj), NAUTILUS_TYPE_BOOKMARK))
#define NAUTILUS_IS_BOOKMARK_CLASS(klass) \
	(GTK_CHECK_CLASS_TYPE ((klass), NAUTILUS_TYPE_BOOKMARK))

typedef struct NautilusBookmarkDetails NautilusBookmarkDetails;

struct NautilusBookmark {
	GtkObject object;
	NautilusBookmarkDetails *details;	
};

struct NautilusBookmarkClass {
	GtkObjectClass parent_class;

	/* Signals that clients can connect to. */

	/* The appearance_changed signal is emitted when the bookmark's
	 * name or icon has changed.
	 */
	void	(* appearance_changed) (NautilusBookmark *bookmark);

	/* The contents_changed signal is emitted when the bookmark's
	 * URI has changed.
	 */
	void	(* contents_changed) (NautilusBookmark *bookmark);
};

typedef struct NautilusBookmarkClass NautilusBookmarkClass;

GtkType               nautilus_bookmark_get_type            (void);
NautilusBookmark *    nautilus_bookmark_new       	    (const char           *uri,
							     const char           *name);
NautilusBookmark *    nautilus_bookmark_new_with_icon	    (const char		  *uri,
							     const char		  *name,
							     NautilusScalableIcon *icon);
NautilusBookmark *    nautilus_bookmark_copy                (NautilusBookmark     *bookmark);
char *                nautilus_bookmark_get_name            (NautilusBookmark     *bookmark);
char *                nautilus_bookmark_get_uri             (NautilusBookmark     *bookmark);
NautilusScalableIcon *nautilus_bookmark_get_icon	    (NautilusBookmark     *bookmark);
void		      nautilus_bookmark_set_name 	    (NautilusBookmark 	  *bookmark, 
							     const char 	  *new_name);
gboolean	      nautilus_bookmark_uri_known_not_to_exist (NautilusBookmark  *bookmark);
int                   nautilus_bookmark_compare_with        (gconstpointer         a,
							     gconstpointer         b);
int                   nautilus_bookmark_compare_uris        (gconstpointer         a,
							     gconstpointer         b);

/* Helper functions for displaying bookmarks */
gboolean              nautilus_bookmark_get_pixmap_and_mask (NautilusBookmark     *bookmark,
							     guint                 icon_size,
							     GdkPixmap           **pixmap_return,
							     GdkBitmap           **mask_return);
GdkPixbuf *           nautilus_bookmark_get_pixbuf          (NautilusBookmark     *bookmark,
							     guint		   icon_size,
							     gboolean              optimize_for_anti_aliasing);
GtkWidget *           nautilus_bookmark_menu_item_new       (NautilusBookmark     *bookmark);


#endif /* NAUTILUS_BOOKMARK_H */
