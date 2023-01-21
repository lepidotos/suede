/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */

/* eel-preferences-box.h - A preferences box is a widget that manages
                           prefernece panes.  Only one pane can be
                           visible at any given time.

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

   Authors: Ramiro Estrugo <ramiro@eazel.com>
*/


#include <config.h>
#include "eel-preferences-box.h"

#include <eel/eel-gtk-extensions.h>
#include <eel/eel-gtk-macros.h>
#include <eel/eel-string.h>
#include <gtk/gtkclist.h>
#include <gtk/gtkdialog.h>
#include <gtk/gtknotebook.h>
#include <libgnome/gnome-i18n.h>
#include <libgnome/gnome-util.h>
#include <libgnomeui/gnome-stock.h>

#define NUM_CATEGORY_COLUMNS 1
#define CATEGORY_COLUMN 0
#define SPACING_BETWEEN_CATEGORIES_AND_PANES 4
#define STRING_LIST_DEFAULT_TOKENS_DELIMETER ","

typedef struct
{
	char *pane_name;
	EelPreferencesPane *pane_widget;
} PaneInfo;

struct EelPreferencesBoxDetails
{
	GtkWidget *category_list;
	GtkWidget *pane_notebook;
	GList *panes;
	char *selected_pane;
	guint select_row_signal_id;
};

/* EelPreferencesBoxClass methods */
static void      eel_preferences_box_initialize_class   (EelPreferencesBoxClass *preferences_box_class);
static void      eel_preferences_box_initialize         (EelPreferencesBox      *preferences_box);

/* GtkObjectClass methods */
static void      eel_preferences_box_destroy            (GtkObject              *object);

/* Misc private stuff */
static void      preferences_box_category_list_recreate (EelPreferencesBox      *preferences_box);
static void      preferences_box_select_pane            (EelPreferencesBox      *preferences_box,
							 const char             *name);

/* PaneInfo functions */
static PaneInfo *pane_info_new                          (const char             *pane_name);
static void      pane_info_free                         (PaneInfo               *info);

/* Category list callbacks */
static void      category_list_select_row_callback      (GtkCList               *clist,
							 int                     row,
							 int                     column,
							 GdkEventButton         *event,
							 gpointer                user_data);

/* Convience functions */
static int       preferences_box_find_row               (GtkCList               *clist,
							 char                   *pane_name);
static void      user_level_changed_callback            (gpointer                callback_data);

EEL_DEFINE_CLASS_BOILERPLATE (EelPreferencesBox, eel_preferences_box, GTK_TYPE_HBOX)

/*
 * EelPreferencesBoxClass methods
 */
static void
eel_preferences_box_initialize_class (EelPreferencesBoxClass *preferences_box_class)
{
	GtkObjectClass *object_class;
	
	object_class = GTK_OBJECT_CLASS (preferences_box_class);

	/* GtkObjectClass */
	object_class->destroy = eel_preferences_box_destroy;
}

static void
eel_preferences_box_initialize (EelPreferencesBox *preferences_box)
{
	preferences_box->details = g_new0 (EelPreferencesBoxDetails, 1);

	eel_preferences_add_callback_while_alive ("user_level",
						  user_level_changed_callback,
						  preferences_box,
						  GTK_OBJECT (preferences_box));
}

/*
 * GtkObjectClass methods
 */
static void
eel_preferences_box_destroy (GtkObject *object)
{
	EelPreferencesBox *preferences_box;
	
	g_return_if_fail (EEL_IS_PREFERENCES_BOX (object));
	
	preferences_box = EEL_PREFERENCES_BOX (object);

	if (preferences_box->details->panes) {
		GList *panes;
		
		panes = preferences_box->details->panes;

		while (panes) {
			PaneInfo * info = panes->data;
			
			g_assert (info != NULL);
			pane_info_free (info);
			panes = panes->next;
		}
		
		g_list_free (preferences_box->details->panes);
	}

	g_free (preferences_box->details->selected_pane);
	g_free (preferences_box->details);
	
	/* Chain destroy */
	EEL_CALL_PARENT (GTK_OBJECT_CLASS, destroy, (object));
}

/*
 * Misc private stuff
 */
static void
preferences_box_select_pane (EelPreferencesBox *preferences_box,
			     const char *pane_name)
{
	GList *pane_iterator;
	
	g_return_if_fail (EEL_IS_PREFERENCES_BOX (preferences_box));
	g_return_if_fail (preferences_box->details != NULL);
	g_return_if_fail (preferences_box->details->panes != NULL);
	g_return_if_fail (pane_name != NULL);

	/* Show only the corresponding pane widget */
	pane_iterator = preferences_box->details->panes;

	while (pane_iterator) {
		PaneInfo *info = pane_iterator->data;

		g_assert (info != NULL);
		
		if (eel_str_is_equal (pane_name, info->pane_name)) {
 			gtk_widget_show (GTK_WIDGET (info->pane_widget));
 			gtk_notebook_set_page (GTK_NOTEBOOK (preferences_box->details->pane_notebook), 
 					       g_list_position (preferences_box->details->panes, pane_iterator));

			g_free (preferences_box->details->selected_pane);
			preferences_box->details->selected_pane = g_strdup (pane_name);
			return;
		}
		
		pane_iterator = pane_iterator->next;
	}

	g_warning ("Pane '%s' could not be found.", pane_name);
}

static int
preferences_box_find_row (GtkCList *clist, char *pane_name)
{
	int i;
	char *pane = NULL;
	
	for (i=0; i < GTK_CLIST (clist)->rows; i++) {
		gtk_clist_get_text (GTK_CLIST (clist), i, 0, &pane);
		
		if (eel_str_is_equal (pane, pane_name)) {
			return i;
		}
	}
	
	return -1;
}

static void
preferences_box_category_list_recreate (EelPreferencesBox *preferences_box)
{
	GList *iterator;
	int row = 0;

	g_return_if_fail (EEL_IS_PREFERENCES_BOX (preferences_box));
	g_return_if_fail (GTK_IS_CLIST (preferences_box->details->category_list));

	/* Block the select_row signal so that the 1st item doesnt get selected.
	 * Otherwise, we lose the selected_pane.
	 */
	g_assert (preferences_box->details->select_row_signal_id != 0);
	gtk_signal_handler_block (GTK_OBJECT (preferences_box->details->category_list),
				  preferences_box->details->select_row_signal_id);

	gtk_clist_clear (GTK_CLIST (preferences_box->details->category_list));

	for (iterator = preferences_box->details->panes; iterator != NULL; iterator = iterator->next) {
		PaneInfo *info = iterator->data;
	
		g_assert (EEL_IS_PREFERENCES_PANE (info->pane_widget));

		if (eel_preferences_pane_get_num_visible_groups (info->pane_widget) > 0) {
			char *text_array[NUM_CATEGORY_COLUMNS];
			
			text_array[CATEGORY_COLUMN] = info->pane_name;
			gtk_clist_append (GTK_CLIST (preferences_box->details->category_list), text_array);

			if (eel_str_is_equal (info->pane_name, preferences_box->details->selected_pane)) {
				row = preferences_box_find_row (GTK_CLIST (preferences_box->details->category_list),
								info->pane_name);
				
				if (row == -1) {
					row = 0;
				}
			}
		}
	}
	
	gtk_signal_handler_unblock (GTK_OBJECT (preferences_box->details->category_list),
				    preferences_box->details->select_row_signal_id);
	
	/* You have to do this to get the highlighted row in the clist to change for some reason */
	gtk_clist_select_row (GTK_CLIST (preferences_box->details->category_list), row, 0);

	category_list_select_row_callback (GTK_CLIST (preferences_box->details->category_list),
					   row,
					   0,
					   NULL,
					   preferences_box);
}

/*
 * PaneInfo functions
 */
static PaneInfo *
pane_info_new (const char *pane_name)
{
	PaneInfo * info;

	g_assert (pane_name != NULL);

	info = g_new0 (PaneInfo, 1);

	info->pane_name = g_strdup (pane_name);

	return info;
}

static void
pane_info_free (PaneInfo *info)
{
	g_assert (info != NULL);
	
	g_free (info->pane_name);
	g_free (info);
}

/*
 * Category list callbacks
 */
static void
category_list_select_row_callback (GtkCList *clist,
				   int row,
				   int column,
				   GdkEventButton *event,
				   gpointer callback_data)
{
	const char *pane_name = NULL;
	
	g_return_if_fail (EEL_IS_PREFERENCES_BOX (callback_data));

	/* The cast here is needed because of the broken gtk_clist api */
	if (gtk_clist_get_text (clist, row, CATEGORY_COLUMN, (char **) &pane_name) != 1) {
		return;
	}

	g_return_if_fail (pane_name != NULL);

	preferences_box_select_pane (EEL_PREFERENCES_BOX (callback_data), pane_name);
}

/*
 * EelPreferencesBox public methods
 */
GtkWidget*
eel_preferences_box_new (void)
{
	EelPreferencesBox *preferences_box;

	preferences_box = EEL_PREFERENCES_BOX
		(gtk_widget_new (eel_preferences_box_get_type (), NULL));

	/* Configure ourselves */
 	gtk_box_set_homogeneous (GTK_BOX (preferences_box), FALSE);
 	gtk_box_set_spacing (GTK_BOX (preferences_box), SPACING_BETWEEN_CATEGORIES_AND_PANES);

	/* The category list */
	preferences_box->details->category_list = gtk_clist_new (NUM_CATEGORY_COLUMNS);

	preferences_box->details->select_row_signal_id =
		gtk_signal_connect (GTK_OBJECT (preferences_box->details->category_list), 
				    "select_row",
				    GTK_SIGNAL_FUNC (category_list_select_row_callback),
				    preferences_box);

	gtk_clist_set_selection_mode (GTK_CLIST (preferences_box->details->category_list), 
				      GTK_SELECTION_BROWSE);

	gtk_clist_set_column_auto_resize (GTK_CLIST (preferences_box->details->category_list),
					  CATEGORY_COLUMN,
					  TRUE);
	
	gtk_box_pack_start (GTK_BOX (preferences_box),
			    preferences_box->details->category_list,
			    FALSE,
			    TRUE,
			    0);

	/* The gtk notebook that the panes go into. */
	preferences_box->details->pane_notebook = gtk_notebook_new ();

	gtk_notebook_set_show_tabs (GTK_NOTEBOOK (preferences_box->details->pane_notebook), FALSE);
	gtk_notebook_set_show_border (GTK_NOTEBOOK (preferences_box->details->pane_notebook), FALSE);
	
	gtk_box_pack_start (GTK_BOX (preferences_box),
			    preferences_box->details->pane_notebook,
			    TRUE,
			    TRUE,
			    0);

	gtk_widget_show (preferences_box->details->category_list);
	gtk_widget_show (preferences_box->details->pane_notebook);

	return GTK_WIDGET (preferences_box);
}

static EelPreferencesPane *
preferences_box_add_pane (EelPreferencesBox *preferences_box,
			  const char *pane_title)
{
	PaneInfo *info;

	g_return_val_if_fail (EEL_IS_PREFERENCES_BOX (preferences_box), NULL);
	g_return_val_if_fail (pane_title != NULL, NULL);

	info = pane_info_new (pane_title);
	
	preferences_box->details->panes = g_list_append (preferences_box->details->panes, info);
	
	info->pane_widget = EEL_PREFERENCES_PANE (eel_preferences_pane_new ());
	
	gtk_notebook_append_page (GTK_NOTEBOOK (preferences_box->details->pane_notebook),
				  GTK_WIDGET (info->pane_widget),
				  NULL);

	return info->pane_widget;
}

void
eel_preferences_box_update (EelPreferencesBox	*preferences_box)
{
	GList *iterator;

	g_return_if_fail (EEL_IS_PREFERENCES_BOX (preferences_box));

	for (iterator = preferences_box->details->panes; iterator != NULL; iterator = iterator->next) {
		PaneInfo *info = iterator->data;
		
		g_assert (EEL_IS_PREFERENCES_PANE (info->pane_widget));

		eel_preferences_pane_update (info->pane_widget);
	}

	preferences_box_category_list_recreate (preferences_box);
}

static PaneInfo *
preferences_box_find_pane (const EelPreferencesBox *preferences_box,
			   const char *pane_name)
{
	GList *node;
	PaneInfo *info;

	g_return_val_if_fail (EEL_IS_PREFERENCES_BOX (preferences_box), NULL);

	for (node = preferences_box->details->panes; node != NULL; node = node->next) {
		g_assert (node->data != NULL);
		info = node->data;
		if (eel_str_is_equal (info->pane_name, pane_name)) {
			return info;
		}
	}

	return NULL;
}

static EelPreferencesPane *
preferences_box_find_pane_widget (const EelPreferencesBox *preferences_box,
				  const char *pane_name)
{
	PaneInfo *info;

	g_return_val_if_fail (EEL_IS_PREFERENCES_BOX (preferences_box), NULL);

	info = preferences_box_find_pane (preferences_box, pane_name);
	if (info == NULL) {
		return NULL;
	}

	return info->pane_widget;
}

static void
preferences_box_populate_pane (EelPreferencesBox *preferences_box,
			       const char *pane_name,
			       const EelPreferencesItemDescription *items)
{
	EelPreferencesPane *pane;
	EelPreferencesGroup *group;
	EelPreferencesItem *item;
	EelStringList *group_names;
	const char *translated_group_name;
	guint i;

	g_return_if_fail (EEL_IS_PREFERENCES_BOX (preferences_box));
	g_return_if_fail (pane_name != NULL);
	g_return_if_fail (items != NULL);

	/* Create the pane if needed */
	pane = preferences_box_find_pane_widget (preferences_box, pane_name);
	if (pane == NULL) {
		pane = EEL_PREFERENCES_PANE (preferences_box_add_pane (preferences_box, pane_name));
	}

	group_names = eel_string_list_new (TRUE);

	for (i = 0; items[i].group_name != NULL; i++) {
		translated_group_name = gettext (items[i].group_name);
		if (!eel_string_list_contains (group_names, translated_group_name)) {
			eel_string_list_insert (group_names, translated_group_name);
			eel_preferences_pane_add_group (pane,
							translated_group_name);
		}
	}

	for (i = 0; items[i].group_name != NULL; i++) {
		group = EEL_PREFERENCES_GROUP (eel_preferences_pane_find_group (pane,
										gettext (items[i].group_name)));
		g_return_if_fail (EEL_IS_PREFERENCES_GROUP (group));

		if (items[i].preference_name != NULL) {
			if (items[i].preference_description != NULL) {
				eel_preferences_set_description (items[i].preference_name,
								 gettext (items[i].preference_description));
			}
		
			item = EEL_PREFERENCES_ITEM (eel_preferences_group_add_item (group,
										     items[i].preference_name,
										     items[i].item_type,
										     items[i].column));
			
			/* Install a control preference if needed */
			if (items[i].control_preference_name != NULL) {
				eel_preferences_item_set_control_preference (item,
									     items[i].control_preference_name);
				eel_preferences_item_set_control_action (item,
									 items[i].control_action);				
				eel_preferences_pane_add_control_preference (pane,
									     items[i].control_preference_name);
			}
			
			/* Install exceptions to enum lists uniqueness rule */
			if (items[i].enumeration_list_unique_exceptions != NULL) {
				g_assert (items[i].item_type == EEL_PREFERENCE_ITEM_ENUMERATION_LIST_VERTICAL
					  || items[i].item_type == EEL_PREFERENCE_ITEM_ENUMERATION_LIST_HORIZONTAL);
				eel_preferences_item_enumeration_list_set_unique_exceptions (item,
											     items[i].enumeration_list_unique_exceptions,
											     STRING_LIST_DEFAULT_TOKENS_DELIMETER);
			}
		}

		if (items[i].populate_function != NULL) {
			(* items[i].populate_function) (group);
		}
	}

	eel_string_list_free (group_names);
}

void
eel_preferences_box_populate (EelPreferencesBox *preferences_box,
			      const EelPreferencesPaneDescription *panes)
{
	guint i;

	g_return_if_fail (EEL_IS_PREFERENCES_BOX (preferences_box));
	g_return_if_fail (panes != NULL);

	for (i = 0; panes[i].pane_name != NULL; i++) {
		preferences_box_populate_pane (preferences_box,
					       gettext (panes[i].pane_name),
					       panes[i].items);
	}

	eel_preferences_box_update (preferences_box);
}

static void
user_level_changed_callback (gpointer callback_data)
{
	g_return_if_fail (EEL_IS_PREFERENCES_BOX (callback_data));

	eel_preferences_box_update (EEL_PREFERENCES_BOX (callback_data));
}

GtkWidget *
eel_preferences_dialog_new (const char *title,
			    const EelPreferencesPaneDescription *panes)
{
	static const char *stock_buttons[] = {
		GNOME_STOCK_BUTTON_OK,
		NULL
	};
	GtkWidget *dialog;
	GtkWidget *preference_box;
	GtkWidget *vbox;

	g_return_val_if_fail (title != NULL, NULL);
	
	dialog = gnome_dialog_newv (title, stock_buttons);

	/* Setup the dialog */
	gtk_window_set_policy (GTK_WINDOW (dialog), 
			       FALSE,	/* allow_shrink */
			       TRUE,	/* allow_grow */
			       FALSE);	/* auto_shrink */

  	gtk_container_set_border_width (GTK_CONTAINER (dialog), 0);
	
	gnome_dialog_set_default (GNOME_DIALOG(dialog), 0);

	eel_gtk_window_set_up_close_accelerator (GTK_WINDOW (dialog));

	preference_box = eel_preferences_box_new ();

	vbox = GNOME_DIALOG (dialog)->vbox;
	
	gtk_box_set_spacing (GTK_BOX (vbox), 10);
	
	gtk_box_pack_start (GTK_BOX (vbox),
			    preference_box,
			    TRUE,	/* expand */
			    TRUE,	/* fill */
			    0);		/* padding */

	gtk_widget_show (preference_box);

	if (panes != NULL) {
		eel_preferences_dialog_populate (GTK_WINDOW (dialog), panes);
	}
	
	return dialog;
}

EelPreferencesBox *
eel_preferences_dialog_get_box (const GtkWindow *dialog)
{
	GtkWidget *vbox;
	const GList *last_node;
	const GtkBoxChild *box_child;

	g_return_val_if_fail (GNOME_IS_DIALOG (dialog), NULL);

	vbox = GNOME_DIALOG (dialog)->vbox;

	g_return_val_if_fail (GTK_IS_VBOX (vbox), NULL);
	last_node = g_list_last (GTK_BOX (vbox)->children);
	g_return_val_if_fail (last_node != NULL, NULL);
	g_return_val_if_fail (last_node->data != NULL, NULL);
	box_child = last_node->data;
	g_return_val_if_fail (EEL_IS_PREFERENCES_BOX (box_child->widget), NULL);
	return EEL_PREFERENCES_BOX (box_child->widget);
}

void
eel_preferences_dialog_populate (GtkWindow *dialog,
				 const EelPreferencesPaneDescription *panes)
{
	EelPreferencesBox *preferences_box;

	g_return_if_fail (GTK_IS_WINDOW (dialog));
	g_return_if_fail (panes != NULL);
	preferences_box = eel_preferences_dialog_get_box (dialog);
	g_return_if_fail (EEL_IS_PREFERENCES_BOX (preferences_box));
	eel_preferences_box_populate (preferences_box, panes);
}

void
eel_preferences_box_for_each_pane (const EelPreferencesBox *preferences_box,
				   EelPreferencesBoxForEachCallback callback,
				   gpointer callback_data)
{
	GList *node;
	PaneInfo *info;
	
	g_return_if_fail (EEL_IS_PREFERENCES_BOX (preferences_box));
	g_return_if_fail (callback != NULL);
	
	for (node = preferences_box->details->panes; node != NULL; node = node->next) {
		g_assert (node->data != NULL);
		info = node->data;
		(* callback) (info->pane_name, info->pane_widget, callback_data);
	}
}

void
eel_preferences_box_rename_pane (EelPreferencesBox *preferences_box,
				 const char *pane_name,
				 const char *new_pane_name)
{
	PaneInfo *pane_info;
	int row;

	g_return_if_fail (EEL_IS_PREFERENCES_BOX (preferences_box));
	g_return_if_fail (eel_strlen (pane_name) > 0);
	g_return_if_fail (eel_strlen (new_pane_name) > 0);

	if (eel_str_is_equal (pane_name, new_pane_name)) {
		return;
	}

	pane_info = preferences_box_find_pane (preferences_box, pane_name);
	if (pane_info == NULL) {
		g_warning ("The box does not have a pane called '%s'", pane_name);
		return;
	}

	row = preferences_box_find_row (GTK_CLIST (preferences_box->details->category_list),
					pane_info->pane_name);
	g_assert (row != -1);

	g_free (pane_info->pane_name);
	pane_info->pane_name = g_strdup (new_pane_name);
	gtk_clist_set_text (GTK_CLIST (preferences_box->details->category_list),
			    row,
			    CATEGORY_COLUMN,
			    pane_info->pane_name);
}

char *
eel_preferences_box_get_pane_name (const EelPreferencesBox *preferences_box,
				   const EelPreferencesPane *pane)
{
	GList *node;
	PaneInfo *info;
	
	g_return_val_if_fail (EEL_IS_PREFERENCES_BOX (preferences_box), NULL);
	g_return_val_if_fail (EEL_IS_PREFERENCES_PANE (pane), NULL);
	
	for (node = preferences_box->details->panes; node != NULL; node = node->next) {
		g_assert (node->data != NULL);
		info = node->data;
		if (info->pane_widget == pane) {
			return g_strdup (info->pane_name);
		}
	}

	return NULL;
}
