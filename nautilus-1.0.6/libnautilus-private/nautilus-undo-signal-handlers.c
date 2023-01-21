/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */

/* Signal handlers to enable undo in Gtk Widgets.
 *
 * Copyright (C) 2000 Eazel, Inc.
 *
 * Author: Gene Z. Ragan <gzr@eazel.com>
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


#include <config.h>
#include <gtk/gtk.h>

#include <libgnome/gnome-defs.h>
#include <libgnome/gnome-i18n.h>
#include <libnautilus/nautilus-undo.h>

#include <eel/eel-gtk-macros.h>

#include "nautilus-undo-signal-handlers.h"


typedef struct {
	char *undo_text;
	gint position;
	guint selection_start;
	guint selection_end;
} EditableUndoData;

typedef struct {
	gboolean undo_registered;
} EditableUndoObjectData;


static void restore_editable_from_undo_snapshot_callback (GtkObject 	*target, 
							  gpointer 	callback_data);
static void editable_register_edit_undo 		 (GtkEditable 	*editable);
static void free_editable_object_data 			 (gpointer 	data);

/* nautilus_undo_set_up_nautilus_entry_for_undo
 * 
 * Functions and callback methods to handle undo 
 * in a NautilusEntry
 */

static void 
nautilus_entry_user_changed_callback (NautilusEntry *entry)
{		
	/* Register undo transaction */	
	editable_register_edit_undo (GTK_EDITABLE (entry));
}

void
nautilus_undo_set_up_nautilus_entry_for_undo (NautilusEntry *entry)
{
	EditableUndoObjectData *data;
	
	if (!NAUTILUS_IS_ENTRY (entry) ) {
		return;
	}

	data = g_new(EditableUndoObjectData, 1);
	data->undo_registered = FALSE;
	gtk_object_set_data_full (GTK_OBJECT (entry), "undo_registered", 
				  data, free_editable_object_data);

	/* Connect to entry signals */
	gtk_signal_connect (GTK_OBJECT (entry), 
		    "user_changed",
		    GTK_SIGNAL_FUNC (nautilus_entry_user_changed_callback),
		    NULL);
}

void
nautilus_undo_tear_down_nautilus_entry_for_undo (NautilusEntry *entry)
{
	if (!NAUTILUS_IS_ENTRY (entry) ) {
		return;
	}

	/* Disconnect from entry signals */
	gtk_signal_disconnect_by_func (GTK_OBJECT (entry), 
		    GTK_SIGNAL_FUNC (nautilus_entry_user_changed_callback),		    
		    NULL);

}

/* nautilus_undo_set_up_nautilus_entry_for_undo
 * 
 * Functions and callback methods to handle undo 
 * in a NautilusEntry
 */

static void 
free_editable_undo_data (gpointer data)
{
	EditableUndoData *undo_data;

	undo_data = (EditableUndoData *) data;
	
	g_free (undo_data->undo_text);
	g_free (undo_data);
}

static void 
free_editable_object_data (gpointer data)
{
	g_free (data);
}


static void 
editable_insert_text_callback (GtkEditable *editable)
{
	/* Register undo transaction */	
	editable_register_edit_undo (editable);
}

static void 
editable_delete_text_callback (GtkEditable *editable)
{
	/* Register undo transaction */	
	editable_register_edit_undo (editable);
}

static void
editable_register_edit_undo (GtkEditable *editable)
{	
	EditableUndoData *undo_data;
	EditableUndoObjectData *undo_info;
	gpointer data;

	if (!GTK_IS_EDITABLE (editable) ) {
		return;
	}

	/* Check our undo registered flag */
	data = gtk_object_get_data (GTK_OBJECT (editable), "undo_registered");
	if (data == NULL) {
		g_warning ("Undo data is NULL");
		return;
	}

	undo_info = (EditableUndoObjectData *)data;		
	if (undo_info->undo_registered) {
		return;
	}
	
	undo_data = g_new (EditableUndoData, 1);
	undo_data->undo_text = g_strdup (gtk_editable_get_chars (editable, 0, -1));
	undo_data->position = gtk_editable_get_position (editable);
	undo_data->selection_start = editable->selection_start_pos;
	undo_data->selection_end = editable->selection_end_pos;

	nautilus_undo_register
		(GTK_OBJECT (editable),
		 restore_editable_from_undo_snapshot_callback,
		 undo_data,
		 (GDestroyNotify) free_editable_undo_data,
		 _("Edit"),
		 _("Undo Edit"),
		 _("Undo the edit"),
		 _("Redo Edit"),
		 _("Redo the edit"));

	undo_info->undo_registered = TRUE;
}

void
nautilus_undo_set_up_editable_for_undo (GtkEditable *editable)
{
	EditableUndoObjectData *data;
	
	if (!GTK_IS_EDITABLE (editable) ) {
		return;
	}

	/* Connect to editable signals */
	gtk_signal_connect (GTK_OBJECT (editable), 
		    "insert_text",
		    GTK_SIGNAL_FUNC (editable_insert_text_callback),
		    NULL);
	
	gtk_signal_connect (GTK_OBJECT (editable), 
		    "delete_text",
		    GTK_SIGNAL_FUNC (editable_delete_text_callback),
		    NULL);


	data = g_new (EditableUndoObjectData, 1);
	data->undo_registered = FALSE;
	gtk_object_set_data_full (GTK_OBJECT (editable), "undo_registered", 
				  data, free_editable_object_data);
}

void
nautilus_undo_tear_down_editable_for_undo (GtkEditable *editable)
{
	if (!GTK_IS_EDITABLE (editable) ) {
		return;
	}

	/* Disconnect from entry signals */
	gtk_signal_disconnect_by_func (GTK_OBJECT (editable), 
		    GTK_SIGNAL_FUNC (editable_insert_text_callback),		    
		    NULL);

	gtk_signal_disconnect_by_func (GTK_OBJECT (editable), 
		    GTK_SIGNAL_FUNC (editable_delete_text_callback),		    
		    NULL);
}

/* restore_editable_from_undo_snapshot_callback
 * 
 * Restore edited text.
 */
static void
restore_editable_from_undo_snapshot_callback (GtkObject *target, gpointer callback_data)
{
	GtkEditable *editable;
	GtkWindow *window;
	EditableUndoData *undo_data;
	EditableUndoObjectData *data;
	gint position;
		
	editable = GTK_EDITABLE (target);
	undo_data = (EditableUndoData *) callback_data;

	/* Check our undo registered flag */
	data = gtk_object_get_data (target, "undo_registered");
	if (data == NULL) {
		g_warning ("Undo regisetred flag not found");
		return;
	}
	
	/* Reset the registered flag so we get a new item for future editing. */
	data->undo_registered = FALSE;

	/* Register a new undo transaction for redo. */
	editable_register_edit_undo (editable);
	
	/* Restore the text. */
	position = 0;
	gtk_editable_delete_text (editable, 0, -1);
	gtk_editable_insert_text (editable, undo_data->undo_text,
				  strlen (undo_data->undo_text), &position);

	/* Set focus to widget */
	window = GTK_WINDOW (gtk_widget_get_toplevel ( GTK_WIDGET (target)));
	gtk_window_set_focus (window, GTK_WIDGET (editable));

	/* We have to do this call, because the previous call selects all text */
	gtk_editable_select_region (editable, 0, 0);

	/* Restore selection */
	gtk_editable_select_region (editable, undo_data->selection_start, 
			   	    undo_data->selection_end);
	
	/* Set the i-beam to the saved position */
	gtk_editable_set_position (editable, undo_data->position);

	/* Reset the registered flag so we get a new item for future editing. */
	data->undo_registered = FALSE;
}


/* editable_set_undo_key
 *
 * Allow the use of ctrl-z from within widget.
 */

/* Undo is disabled until we have a better implementation.
 * Both here and in nautilus-shell-ui.xml.
 */

/* FIXME bugzilla.gnome.org 43515: Undo doesn't work */
#ifdef UNDO_ENABLED

static gboolean
editable_key_press_event (GtkEditable *editable, GdkEventKey *event, gpointer user_data)
{	
	switch (event->keyval) {
	/* Undo */
	case 'z':
		if ((event->state & GDK_CONTROL_MASK) != 0) {
			nautilus_undo (GTK_OBJECT (editable));
			gtk_signal_emit_stop_by_name (GTK_OBJECT (editable),
						      "key_press_event");
			return TRUE;
		}
		break;
		
	default:
		break;
	}

	return FALSE;
}

#endif

/* editable_set_undo_key
 *
 * Allow the use of ctrl-z from within widget.  This should only be 
 * set if there is no menu bar to use to undo the widget.
 */
 
void 
nautilus_undo_editable_set_undo_key (GtkEditable *editable, gboolean value)
{
/* FIXME bugzilla.gnome.org 43515: Undo doesn't work */
#ifdef UNDO_ENABLED
	if (value) {
		/* Connect to entry signals */
		gtk_signal_connect (GTK_OBJECT (editable), 
				    "key_press_event",
				    GTK_SIGNAL_FUNC (editable_key_press_event),
				    NULL);
	} else {
		/* FIXME bugzilla.gnome.org 45092: Warns if the handler
		 * is not already connected. We could use object data
		 * to prevent that little problem.
		 */
		gtk_signal_disconnect_by_func (GTK_OBJECT (editable), 
					       GTK_SIGNAL_FUNC (editable_key_press_event),		    
					       NULL);
	}
#endif
}
