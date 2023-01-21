/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */

/* eel-gtk-extensions.h - interface for new functions that operate on
  			       gtk classes. Perhaps some of these should be
  			       rolled into gtk someday.

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

   Authors: John Sullivan <sullivan@eazel.com>
            Ramiro Estrugo <ramiro@eazel.com>
*/

#ifndef EEL_GTK_EXTENSIONS_H
#define EEL_GTK_EXTENSIONS_H

#include <gdk-pixbuf/gdk-pixbuf.h>
#include <gtk/gtkclist.h>
#include <gtk/gtkmenu.h>
#include <gtk/gtkmenuitem.h>
#include <gtk/gtkpixmap.h>
#include <gtk/gtkwindow.h>
#include <eel/eel-gdk-extensions.h>

#define EEL_DEFAULT_POPUP_MENU_DISPLACEMENT 	2
#define EEL_STANDARD_CLOSE_WINDOW_CONTROL_KEY 	'w'


/* GtkButton */
void              eel_gtk_button_auto_click                       (GtkButton              *button);
void              eel_gtk_button_set_standard_padding             (GtkButton              *button);
void              eel_gtk_button_set_padding                      (GtkButton              *button,
								   int                     pad_amount);

/* GtkCList */
int               eel_gtk_clist_get_first_selected_row            (GtkCList               *list);
int               eel_gtk_clist_get_last_selected_row             (GtkCList               *list);
void              eel_gtk_clist_set_double_click_button           (GtkCList               *clist,
								   GtkButton              *button);

/* signals */
guint             eel_gtk_signal_connect_free_data                (GtkObject              *object,
								   const gchar            *name,
								   GtkSignalFunc           func,
								   gpointer                data);
guint             eel_gtk_signal_connect_free_data_custom         (GtkObject              *object,
								   const gchar            *name,
								   GtkSignalFunc           func,
								   gpointer                data,
								   GtkDestroyNotify        destroy_func);
void              eel_gtk_signal_connect_full_while_alive         (GtkObject              *object,
								   const gchar            *name,
								   GtkSignalFunc           func,
								   GtkCallbackMarshal      marshal,
								   gpointer                data,
								   GtkDestroyNotify        destroy_func,
								   gboolean                object_signal,
								   gboolean                after,
								   GtkObject              *alive_object);
void              eel_gtk_signal_connect_while_realized           (GtkObject              *object,
								   const char             *name,
								   GtkSignalFunc           callback,
								   gpointer                callback_data,
								   GtkWidget              *realized_widget);
void              eel_nullify_when_destroyed                      (gpointer                object_pointer_address);
void              eel_nullify_cancel                              (gpointer                object_pointer_address);

/* list of GtkObject */
GList *           eel_gtk_object_list_ref                         (GList                  *list);
void              eel_gtk_object_list_unref                       (GList                  *list);
void              eel_gtk_object_list_free                        (GList                  *list);
GList *           eel_gtk_object_list_copy                        (GList                  *list);

/* GtkWidget */
void              eel_gtk_widget_set_shown                        (GtkWidget              *widget,
								   gboolean                shown);
gboolean          eel_point_in_allocation                         (const GtkAllocation    *allocation,
								   int                     x,
								   int                     y);
void              eel_gtk_widget_set_font                         (GtkWidget              *widget,
								   GdkFont                *font);
void              eel_gtk_widget_set_font_by_name                 (GtkWidget              *widget,
								   const char             *font_name);
gboolean          eel_point_in_widget                             (GtkWidget              *widget,
								   int                     x,
								   int                     y);
void              eel_gtk_widget_set_background_color             (GtkWidget              *widget,
								   const char             *color_spec);
void              eel_gtk_widget_set_foreground_color             (GtkWidget              *widget,
								   const char             *color_spec);
GtkWidget        *eel_gtk_widget_find_windowed_ancestor           (GtkWidget              *widget);
guint             eel_get_current_event_time                      (void);
GdkFont          *eel_gtk_get_system_font                         (void);

/* GtkContainer */
GtkWidget        *eel_gtk_container_get_first_child               (GtkContainer           *container);
void              eel_gtk_container_foreach_deep                  (GtkContainer           *container,
								   GtkCallback             callback,
								   gpointer                callback_data);

/* GtkWindow */
void              eel_gtk_window_set_initial_geometry             (GtkWindow              *window,
								   EelGdkGeometryFlags     geometry_flags,
								   int                     left,
								   int                     top,
								   guint                   width,
								   guint                   height);
void              eel_gtk_window_set_initial_geometry_from_string (GtkWindow              *window,
								   const char             *geometry_string,
								   guint                   minimum_width,
								   guint                   minimum_height);
void              eel_gtk_window_set_up_close_accelerator         (GtkWindow              *window);
gboolean          eel_gtk_window_event_is_close_accelerator       (GtkWindow              *window,
								   GdkEventKey            *event);
void              eel_gtk_window_present                          (GtkWindow              *window);

/* selection data */
GtkSelectionData *eel_gtk_selection_data_copy_deep                (const GtkSelectionData *selection_data);
void              eel_gtk_selection_data_free_deep                (GtkSelectionData       *selection_data);

/* GtkMenu and GtkMenuItem */
char             *eel_truncate_text_for_menu_item                 (const char             *text);
void              eel_pop_up_context_menu                         (GtkMenu                *menu,
								   gint16                  offset_x,
								   gint16                  offset_y,
								   GdkEventButton         *event);
GtkMenuItem *     eel_gtk_menu_append_separator                   (GtkMenu                *menu);
GtkMenuItem *     eel_gtk_menu_insert_separator                   (GtkMenu                *menu,
								   int                     index);
void              eel_gtk_menu_set_item_visibility                (GtkMenu                *menu,
								   int                     index,
								   gboolean                visible);
/* GtkStyle */
void              eel_gtk_style_set_font                          (GtkStyle               *style,
								   GdkFont                *font);
void              eel_gtk_style_set_font_by_name                  (GtkStyle               *style,
								   const char             *font_name);

/* GtkLabel */
void              eel_gtk_label_make_bold                         (GtkLabel               *label);
void              eel_gtk_label_make_larger                       (GtkLabel               *label,
								   guint                   num_steps);
void              eel_gtk_label_make_smaller                      (GtkLabel               *label,
								   guint                   num_steps);

/* GtkPixmap */
GtkPixmap        *eel_gtk_pixmap_new_empty                        (void);

/* GtkAdjustment */
void              eel_gtk_adjustment_set_value                    (GtkAdjustment          *adjustment,
								   float                   value);
void              eel_gtk_adjustment_clamp_value                  (GtkAdjustment          *adjustment);

/* GdkColor */
void              eel_gtk_style_shade                             (GdkColor               *a,
								   GdkColor               *b,
								   gdouble                 k);

/* Make the given class name act like an existing GtkType for
 * gtk style/theme purposes. */
void              eel_gtk_class_name_make_like_existing_type      (const char             *class_name,
								   GtkType                 existing_gtk_type);
GList *           eel_get_window_list_ordered_front_to_back       (void);

gboolean	  eel_gtk_window_is_on_current_workspace_and_area (GtkWindow		   *window);

/* dnd */
void              eel_drag_set_icon_pixbuf                        (GdkDragContext         *context,
								   GdkPixbuf              *pixbuf,
								   gint                    hot_x,
								   gint                    hot_y);
/* Standard implementations of some GtkWidget methods */
void              eel_gtk_widget_standard_realize                 (GtkWidget              *widget);
void              eel_gtk_widget_standard_draw                    (GtkWidget              *widget,
								   GdkRectangle           *area);
void              eel_gtk_bin_standard_size_allocate              (GtkWidget              *widget,
								   GtkAllocation          *allocation);
void              eel_gtk_bin_standard_size_request               (GtkWidget              *widget,
								   GtkRequisition         *requisition);

/* marshals */

#define eel_gtk_marshal_BOOL__POINTER_POINTER eel_gtk_marshal_INT__POINTER_POINTER
#define eel_gtk_marshal_INT__POINTER_STRING eel_gtk_marshal_INT__POINTER_POINTER
#define eel_gtk_marshal_NONE__BOXED_BOXED gtk_marshal_NONE__POINTER_POINTER
#define eel_gtk_marshal_NONE__POINTER_STRING_STRING gtk_marshal_NONE__POINTER_POINTER_POINTER
#define eel_gtk_marshal_NONE__STRING_POINTER gtk_marshal_NONE__POINTER_POINTER
#define eel_gtk_marshal_NONE__STRING_POINTER_STRING gtk_marshal_NONE__POINTER_POINTER_POINTER
#define eel_gtk_marshal_NONE__STRING_STRING_POINTER_STRING eel_gtk_marshal_NONE__POINTER_POINTER_POINTER_POINTER
#define eel_gtk_marshal_POINTER__POINTER_INT_INT_STRING_POINTER eel_gtk_marshal_POINTER__POINTER_INT_INT_POINTER_POINTER
#define eel_gtk_marshal_POINTER__POINTER_STRING_POINTER eel_gtk_marshal_POINTER__POINTER_POINTER_POINTER
#define eel_gtk_marshal_STRING__NONE eel_gtk_marshal_POINTER__NONE
#define eel_gtk_marshal_STRING__POINTER eel_gtk_marshal_POINTER__POINTER
#define eel_gtk_marshal_STRING__POINTER_INT eel_gtk_marshal_POINTER__POINTER_INT
#define eel_gtk_marshal_STRING__POINTER_POINTER eel_gtk_marshal_POINTER__POINTER_POINTER
#define eel_gtk_marshal_STRING__POINTER_POINTER_POINTER eel_gtk_marshal_POINTER__POINTER_POINTER_POINTER
#define eel_gtk_marshal_STRING__POINTER_POINTER_STRING eel_gtk_marshal_POINTER__POINTER_POINTER_POINTER
#define eel_gtk_marshal_STRING__POINTER_STRING eel_gtk_marshal_POINTER__POINTER_POINTER

void eel_gtk_marshal_INT__NONE                                             (GtkObject     *object,
									    GtkSignalFunc  func,
									    gpointer       func_data,
									    GtkArg        *args);
void eel_gtk_marshal_NONE__DOUBLE                                          (GtkObject     *object,
									    GtkSignalFunc  func,
									    gpointer       func_data,
									    GtkArg        *args);
void eel_gtk_marshal_NONE__DOUBLE_DOUBLE_DOUBLE                            (GtkObject     *object,
									    GtkSignalFunc  func,
									    gpointer       func_data,
									    GtkArg        *args);
void eel_gtk_marshal_NONE__INT_INT_INT                                     (GtkObject     *object,
									    GtkSignalFunc  func,
									    gpointer       func_data,
									    GtkArg        *args);
void eel_gtk_marshal_NONE__POINTER_INT_INT_DOUBLE                          (GtkObject     *object,
									    GtkSignalFunc  func,
									    gpointer       func_data,
									    GtkArg        *args);
void eel_gtk_marshal_NONE__POINTER_POINTER_INT_INT_INT                     (GtkObject     *object,
									    GtkSignalFunc  func,
									    gpointer       func_data,
									    GtkArg        *args);
void eel_gtk_marshal_NONE__POINTER_INT_INT_INT                             (GtkObject     *object,
									    GtkSignalFunc  func,
									    gpointer       func_data,
									    GtkArg        *args);
void eel_gtk_marshal_BOOL__INT_POINTER_INT_INT_UINT                        (GtkObject     *object,
									    GtkSignalFunc  func,
									    gpointer       func_data,
									    GtkArg        *args);     
void eel_gtk_marshal_NONE__INT_POINTER_INT_INT_UINT                        (GtkObject     *object,
									    GtkSignalFunc  func,
									    gpointer       func_data,
									    GtkArg        *args);
void eel_gtk_marshal_NONE__POINTER_POINTER_POINTER_POINTER                 (GtkObject     *object,
									    GtkSignalFunc  func,
									    gpointer       func_data,
									    GtkArg        *args);
void eel_gtk_marshal_NONE__POINTER_INT_POINTER_POINTER                     (GtkObject     *object,
									    GtkSignalFunc  func,
									    gpointer       func_data,
									    GtkArg        *args);
void eel_gtk_marshal_NONE__POINTER_POINTER_POINTER_POINTER_POINTER_POINTER (GtkObject     *object,
									    GtkSignalFunc  func,
									    gpointer       func_data,
									    GtkArg        *args);
void eel_gtk_marshal_NONE__POINTER_POINTER_POINTER_INT_INT_INT             (GtkObject     *object,
									    GtkSignalFunc  func,
									    gpointer       func_data,
									    GtkArg        *args);
void eel_gtk_marshal_NONE__POINTER_POINTER_POINTER_POINTER_INT_INT_UINT    (GtkObject     *object,
									    GtkSignalFunc  func,
									    gpointer       func_data,
									    GtkArg        *args);
void eel_gtk_marshal_NONE__POINTER_INT_INT_DOUBLE_DOUBLE                   (GtkObject     *object,
									    GtkSignalFunc  func,
									    gpointer       func_data,
									    GtkArg        *args);
void eel_gtk_marshal_INT__POINTER_POINTER                                  (GtkObject     *object,
									    GtkSignalFunc  func,
									    gpointer       func_data,
									    GtkArg        *args);
void eel_gtk_marshal_INT__POINTER_INT                                      (GtkObject     *object,
									    GtkSignalFunc  func,
									    gpointer       func_data,
									    GtkArg        *args);
void eel_gtk_marshal_POINTER__NONE                                         (GtkObject     *object,
									    GtkSignalFunc  func,
									    gpointer       func_data,
									    GtkArg        *args);
void eel_gtk_marshal_POINTER__INT                                          (GtkObject     *object,
									    GtkSignalFunc  func,
									    gpointer       func_data,
									    GtkArg        *args);
void eel_gtk_marshal_POINTER__POINTER                                      (GtkObject     *object,
									    GtkSignalFunc  func,
									    gpointer       func_data,
									    GtkArg        *args);
void eel_gtk_marshal_POINTER__POINTER_INT	                           (GtkObject     *object,
									    GtkSignalFunc  func,
									    gpointer       func_data,
									    GtkArg        *args);
void eel_gtk_marshal_POINTER__POINTER_POINTER                              (GtkObject     *object,
									    GtkSignalFunc  func,
									    gpointer       func_data,
									    GtkArg        *args);
void eel_gtk_marshal_POINTER__POINTER_POINTER_POINTER                      (GtkObject     *object,
									    GtkSignalFunc  func,
									    gpointer       func_data,
									    GtkArg        *args);
void eel_gtk_marshal_POINTER__INT_INT_POINTER_POINTER             	   (GtkObject     *object,
									    GtkSignalFunc  func,
									    gpointer       func_data,
									    GtkArg        *args);
void eel_gtk_marshal_POINTER__POINTER_INT_INT_POINTER_POINTER              (GtkObject     *object,
									    GtkSignalFunc  func,
									    gpointer       func_data,
									    GtkArg        *args);

#endif /* EEL_GTK_EXTENSIONS_H */
