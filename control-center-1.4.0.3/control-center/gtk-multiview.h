/* gtk-multiview.h
 * Copyright (C) 2000  Jonathan Blandford
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */
#ifndef __GTK_MULTIVIEW_H__
#define __GTK_MULTIVIEW_H__

#include <gtk/gtk.h>

#ifdef __cplusplus
extern "C" {
#pragma }
#endif /* __cplusplus */

#define GTK_TYPE_MULTIVIEW			(gtk_multiview_get_type ())
#define GTK_MULTIVIEW(obj)			(GTK_CHECK_CAST ((obj), GTK_TYPE_MULTIVIEW, GtkMultiview))
#define GTK_MULTIVIEW_CLASS(klass)		(GTK_CHECK_CLASS_CAST ((klass), GTK_TYPE_MULTIVIEW, GtkMultiviewClass))
#define GTK_IS_MULTIVIEW(obj)			(GTK_CHECK_TYPE ((obj), GTK_TYPE_MULTIVIEW))
#define GTK_IS_MULTIVIEW_CLASS(klass)		(GTK_CHECK_CLASS_TYPE ((obj), GTK_TYPE_MULTIVIEW))


typedef struct _GtkMultiview       GtkMultiview;
typedef struct _GtkMultiviewClass  GtkMultiviewClass;

struct _GtkMultiview
{
  GtkContainer parent;

  /*< private >*/
  GtkWidget *current;
  GList *children;
};

struct _GtkMultiviewClass
{
  GtkContainerClass parent_class;
};

GtkType    gtk_multiview_get_type      (void);
GtkWidget *gtk_multiview_new           (void);
void       gtk_multiview_prepend_child (GtkMultiview *multiview,
					GtkWidget    *child);
void       gtk_multiview_insert_child  (GtkMultiview *multiview,
					GtkWidget    *back_child,
					GtkWidget    *child);
void       gtk_multiview_append_child  (GtkMultiview *multiview,
					GtkWidget    *child);
GtkWidget *gtk_multiview_get_current   (GtkMultiview *multiview);
void       gtk_multiview_set_current   (GtkMultiview *multiview,
					GtkWidget    *child);


#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __GTK_MULTIVIEW_H__ */
