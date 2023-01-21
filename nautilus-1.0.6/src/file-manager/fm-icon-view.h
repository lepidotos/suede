/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */

/* fm-icon-view.h - interface for icon view of directory.

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

   Authors: John Sullivan <sullivan@eazel.com>
*/

#ifndef FM_ICON_VIEW_H
#define FM_ICON_VIEW_H

#include "fm-directory-view.h"

typedef struct FMIconView FMIconView;
typedef struct FMIconViewClass FMIconViewClass;

#define FM_TYPE_ICON_VIEW			(fm_icon_view_get_type ())
#define FM_ICON_VIEW(obj)			(GTK_CHECK_CAST ((obj), FM_TYPE_ICON_VIEW, FMIconView))
#define FM_ICON_VIEW_CLASS(klass)		(GTK_CHECK_CLASS_CAST ((klass), FM_TYPE_ICON_VIEW, FMIconViewClass))
#define FM_IS_ICON_VIEW(obj)			(GTK_CHECK_TYPE ((obj), FM_TYPE_ICON_VIEW))
#define FM_IS_ICON_VIEW_CLASS(klass)		(GTK_CHECK_CLASS_TYPE ((klass), FM_TYPE_ICON_VIEW))

typedef struct FMIconViewDetails FMIconViewDetails;

struct FMIconView {
	FMDirectoryView parent;
	FMIconViewDetails *details;
};

struct FMIconViewClass {
	FMDirectoryViewClass parent_class;

	/* Methods that can be overriden for settings you don't want to come from metadata.
	 */
	 
	/* Note: get_directory_sort_by must return a string that can/will be g_freed.
	 */
	char *	 (* get_directory_sort_by)       (FMIconView *icon_view, 
						  NautilusFile *file);
	void     (* set_directory_sort_by)       (FMIconView *icon_view, 
						  NautilusFile *file, 
						  const char* sort_by);

	gboolean (* get_directory_sort_reversed) (FMIconView *icon_view, 
						  NautilusFile *file);
	void     (* set_directory_sort_reversed) (FMIconView *icon_view, 
						  NautilusFile *file, 
						  gboolean sort_reversed);

	gboolean (* get_directory_auto_layout)   (FMIconView *icon_view, 
						  NautilusFile *file);
	void     (* set_directory_auto_layout)   (FMIconView *icon_view, 
						  NautilusFile *file, 
						  gboolean auto_layout);
	
	gboolean (* get_directory_tighter_layout) (FMIconView *icon_view, 
						   NautilusFile *file);
	void     (* set_directory_tighter_layout)   (FMIconView *icon_view, 
						     NautilusFile *file, 
						     gboolean tighter_layout);

	/* Override "clean_up" if your subclass has its own notion of where icons should be positioned */
	void	 (* clean_up)			 (FMIconView *icon_view);

	/* supports_auto_layout is a function pointer that subclasses may
	 * override to control whether or not the automatic layout options
	 * should be enabled. The default implementation returns TRUE.
	 */
	gboolean (* supports_auto_layout)	 (FMIconView *view);

};

/* GtkObject support */
GtkType fm_icon_view_get_type (void);

#endif /* FM_ICON_VIEW_H */
