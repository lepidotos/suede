/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */

/* eel-preferences-box.c - A preferences box is a widget that manages
                           prefernece panes.  Only one pane can be
                           visible at any given time.

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

#ifndef EEL_PREFERENCES_BOX_H
#define EEL_PREFERENCES_BOX_H

#include <libgnomeui/gnome-dialog.h>
#include <gtk/gtkhbox.h>
#include <gtk/gtkwindow.h>
#include <eel/eel-preferences-pane.h>

BEGIN_GNOME_DECLS

#define EEL_TYPE_PREFERENCES_BOX            (eel_preferences_box_get_type ())
#define EEL_PREFERENCES_BOX(obj)            (GTK_CHECK_CAST ((obj), EEL_TYPE_PREFERENCES_BOX, EelPreferencesBox))
#define EEL_PREFERENCES_BOX_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), EEL_TYPE_PREFERENCES_BOX, EelPreferencesBoxClass))
#define EEL_IS_PREFERENCES_BOX(obj)         (GTK_CHECK_TYPE ((obj), EEL_TYPE_PREFERENCES_BOX))
#define EEL_IS_PREFERENCES_BOX_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), EEL_TYPE_PREFERENCES_BOX))

typedef struct EelPreferencesBox	    EelPreferencesBox;
typedef struct EelPreferencesBoxClass	    EelPreferencesBoxClass;
typedef struct EelPreferencesBoxDetails	    EelPreferencesBoxDetails;

struct EelPreferencesBox
{
	/* Super Class */
	GtkHBox hbox;

	/* Private stuff */
	EelPreferencesBoxDetails *details;
};

struct EelPreferencesBoxClass
{
	GtkHBoxClass parent_class;
};

/*
 * A callback which you can register to to be notified when a particular
 * preference changes.
 */
typedef void (*EelPreferencesGroupPopulateFunction) (EelPreferencesGroup *group);

/* A structure that describes a single preferences dialog ui item. */
typedef struct
{
	const char *group_name;
	const char *preference_name;
	const char *preference_description;
	EelPreferencesItemType item_type;
	const char *control_preference_name;
	EelPreferencesItemControlAction control_action;
	int column;
	EelPreferencesGroupPopulateFunction populate_function;
	const char *enumeration_list_unique_exceptions;
} EelPreferencesItemDescription;

typedef struct
{
	const char *pane_name;
	const EelPreferencesItemDescription *items;
} EelPreferencesPaneDescription;

/* The following tables define preference items for the preferences dialog.
 * Each item corresponds to one preference.
 * 
 * Field definitions:
 *
 * 1. group_name
 *
 *    The group under which the preference is placed.  Each unique group will
 *    be framed and titled with the group_name.
 *
 *    Many items can have the same group_name.  Groups will be created as needed
 *    while populating the items.
 *
 *    This field needs to be non NULL.
 *
 * 2. preference_name
 *
 *    The name of the preference
 *
 *    This field needs to be non NULL.
 *
 * 3. preference_description
 *
 *    A user visible description of the preference.  Not all items use the
 *    description.  In particular, enumeration items use the descriptions from
 *    an enumeration structure.  See field XX below.
 *
 *    This field needs to be non NULL for items other than:
 * 
 *      EEL_PREFERENCE_ITEM_ENUMERATION_VERTICAL_RADIO or 
 *      EEL_PREFERENCE_ITEM_ENUMERATION_HORIZONTAL_RADIO
 * 
 * 4. item_type
 *
 *    The type of the item.  Needs to be one of the valid values of
 *    EelPreferencesItemType.  See eel-preference-item.h.
 *
 *    This field needs to be one of the valid item types.
 * 
 * 5. control_preference_name
 *
 *    A second preference that "controls" this preference.  Can only
 *    be a boolean preference.
 *
 *    This field can be NULL, in which case field 6 is ignored.
 *
 * 6. control_action
 *
 *    The action to take when the control preference in field 5 changes.
 *    There are only 2 possible actions:
 *
 *      EEL_PREFERENCE_ITEM_SHOW - If the control preference is TRUE
 *                                      the show this item.
 *
 *      EEL_PREFERENCE_ITEM_HIDE - If the control preference is FALSE
 *                                      the hide this item.
 *
 * 7. column
 *
 *    A preference pane is composed of groups.  Each group is bounded by
 *    a frame.  Each of these groups can have 0 or 1 columns of preference
 *    item widgets.  This field controls which column the preference item 
 *    widgets appear in.
 *
 * 8. populate_function:
 *
 *    Something.
 *
 * 9. enumeration_list_unique_exceptions
 *    If the item type is one of:
 *
 *      EEL_PREFERENCE_ITEM_ENUMERATION_LIST_HORIZONTAL
 *      EEL_PREFERENCE_ITEM_ENUMERATION_LIST_VERTICAL
 *
 *    The this field can be a string of exceptions to the rule that enumeration
 *    list items must always not allow duplicate choices.  For example, if there
 *    are 3 string pickers in the item, then each one cannot select and item
 *    which is already selected in one of the other two.  The preferences item
 *    widget enforces this rule by making such items insensitive.  
 *
 *    The enumeration_list_unique_exceptions allows a way to bypass this rule
 *    for certain choices.
 */

/* A callback for eel_preferences_box_for_each_pane() */
typedef void (* EelPreferencesBoxForEachCallback) (const char *pane_name,
						   EelPreferencesPane *pane,
						   gpointer callback_data);

GtkType            eel_preferences_box_get_type      (void);
GtkWidget*         eel_preferences_box_new           (void);
void               eel_preferences_box_update        (EelPreferencesBox                   *preferences_box);
void               eel_preferences_box_populate      (EelPreferencesBox                   *preferences_box,
						      const EelPreferencesPaneDescription *panes);
GtkWidget *        eel_preferences_dialog_new        (const char                          *title,
						      const EelPreferencesPaneDescription *panes);
void               eel_preferences_dialog_populate   (GtkWindow                           *dialog,
						      const EelPreferencesPaneDescription *panes);

/* Call the given callback for each pane in the preferences box */
void               eel_preferences_box_for_each_pane (const EelPreferencesBox             *preferences_box,
						      EelPreferencesBoxForEachCallback     callback,
						      gpointer                             callback_data);

/* Return the EelPreferencesBox created for the preferences dialog */
EelPreferencesBox *eel_preferences_dialog_get_box    (const GtkWindow                     *dialog);
void               eel_preferences_box_rename_pane   (EelPreferencesBox                   *preferences_box,
						      const char                          *pane_name,
						      const char                          *new_pane_name);
char *             eel_preferences_box_get_pane_name (const EelPreferencesBox             *preferences_box,
						      const EelPreferencesPane            *pane);

END_GNOME_DECLS

#endif /* EEL_PREFERENCES_BOX_H */


