/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */

/* eel-image-table.h - An image table.

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

#ifndef EEL_IMAGE_TABLE_H
#define EEL_IMAGE_TABLE_H

#include <eel/eel-wrap-table.h>
#include <eel/eel-labeled-image.h>
#include <eel/eel-smooth-widget.h>

BEGIN_GNOME_DECLS

#define EEL_TYPE_IMAGE_TABLE            (eel_image_table_get_type ())
#define EEL_IMAGE_TABLE(obj)            (GTK_CHECK_CAST ((obj), EEL_TYPE_IMAGE_TABLE, EelImageTable))
#define EEL_IMAGE_TABLE_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), EEL_TYPE_IMAGE_TABLE, EelImageTableClass))
#define EEL_IS_IMAGE_TABLE(obj)         (GTK_CHECK_TYPE ((obj), EEL_TYPE_IMAGE_TABLE))
#define EEL_IS_IMAGE_TABLE_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), EEL_TYPE_IMAGE_TABLE))

typedef struct EelImageTable		EelImageTable;
typedef struct EelImageTableClass	EelImageTableClass;
typedef struct EelImageTableDetails	EelImageTableDetails;

struct EelImageTable
{
	/* Superclass */
	EelWrapTable wrap_table;

	/* Private things */
	EelImageTableDetails *details;
};

struct EelImageTableClass
{
	EelWrapTableClass parent_class;
	EelSmoothWidgetSetIsSmooth set_is_smooth;
};

typedef struct
{
	int x;
	int y;
	int button;
	guint state;
} EelImageTableEvent;

/* Public GtkImageTable methods */
GtkType    eel_image_table_get_type                    (void);
GtkWidget *eel_image_table_new                         (gboolean       homogeneous);
void       eel_image_table_set_smooth_background_color (EelImageTable *image_table,
							guint32        smooth_background_color);
void       eel_image_table_set_is_smooth               (EelImageTable *image_table,
							gboolean       is_smooth);
GtkWidget *eel_image_table_add_empty_image             (EelImageTable *image_table);

END_GNOME_DECLS

#endif /* EEL_IMAGE_TABLE_H */


