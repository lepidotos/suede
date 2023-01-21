/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */

/* eel-password-dialog.c - A use password prompting dialog widget.

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

#include <config.h>
#include "eel-password-dialog.h"

#include "eel-caption-table.h"
#include "eel-gtk-macros.h"
#include <gtk/gtkcheckbutton.h>
#include <gtk/gtklabel.h>
#include <gtk/gtkmain.h>
#include <gtk/gtksignal.h>
#include <libgnome/gnome-i18n.h>
#include <libgnomeui/gnome-stock.h>

struct EelPasswordDialogDetails
{
	/* Attributes */
	char *username;
	char *password;
	gboolean readonly_username;
	gboolean remember;
	char *remember_label_text;

	/* Internal widgetry and flags */
	GtkWidget *table;
	GtkWidget *remember_button;
	GtkLabel *message;
};

static const char * stock_buttons[] =
{
	GNOME_STOCK_BUTTON_OK,
	GNOME_STOCK_BUTTON_CANCEL,
	NULL
};

/* Dialog button indeces */
static const gint  DIALOG_OK_BUTTON = 0;

/* Caption table rows indeces */
static const guint CAPTION_TABLE_USERNAME_ROW = 0;
static const guint CAPTION_TABLE_PASSWORD_ROW = 1;

/* Layout constants */
static const guint DIALOG_BORDER_WIDTH = 0;
static const guint CAPTION_TABLE_BORDER_WIDTH = 4;

/* EelPasswordDialogClass methods */
static void eel_password_dialog_initialize_class (EelPasswordDialogClass *password_dialog_class);
static void eel_password_dialog_initialize       (EelPasswordDialog      *password_dialog);

/* GtkObjectClass methods */
static void eel_password_dialog_destroy          (GtkObject              *object);


/* GtkDialog callbacks */
static void dialog_show_callback                 (GtkWidget              *widget,
						  gpointer                callback_data);
static void dialog_close_callback                (GtkWidget              *widget,
						  gpointer                callback_data);
/* Caption table callbacks */
static void caption_table_activate_callback      (GtkWidget              *widget,
						  guint                   entry,
						  gpointer                callback_data);


EEL_DEFINE_CLASS_BOILERPLATE (EelPasswordDialog,
			      eel_password_dialog,
			      gnome_dialog_get_type ());


static void
eel_password_dialog_initialize_class (EelPasswordDialogClass * klass)
{
	GtkObjectClass * object_class;
	GtkWidgetClass * widget_class;
	
	object_class = GTK_OBJECT_CLASS(klass);
	widget_class = GTK_WIDGET_CLASS(klass);

	/* GtkObjectClass */
	object_class->destroy = eel_password_dialog_destroy;
}

static void
eel_password_dialog_initialize (EelPasswordDialog *password_dialog)
{
	password_dialog->details = g_new (EelPasswordDialogDetails, 1);

	password_dialog->details->username = NULL;
	password_dialog->details->password = NULL;
	password_dialog->details->readonly_username = FALSE;

	password_dialog->details->remember_label_text = NULL;
	password_dialog->details->remember = FALSE;

	password_dialog->details->table = NULL;
	password_dialog->details->remember_button = NULL;
	password_dialog->details->message = NULL;
}

/* GtkObjectClass methods */
static void
eel_password_dialog_destroy (GtkObject* object)
{
	EelPasswordDialog *password_dialog;
	
	g_return_if_fail (object != NULL);
	g_return_if_fail (EEL_IS_PASSWORD_DIALOG (object));
	
	password_dialog = EEL_PASSWORD_DIALOG (object);
	
	if (password_dialog->details->username) {
		g_free (password_dialog->details->username);
	}

	if (password_dialog->details->password) {
		g_free (password_dialog->details->password);
	}

	if (password_dialog->details->remember_label_text) {
		g_free (password_dialog->details->remember_label_text);
	}

	if (password_dialog->details->message) {
		gtk_widget_destroy (GTK_WIDGET (password_dialog->details->message));
	}

	g_free (password_dialog->details);
}

/* GtkDialog callbacks */
static void
dialog_show_callback (GtkWidget *widget, gpointer callback_data)
{
	EelPasswordDialog *password_dialog;

	g_return_if_fail (callback_data != NULL);
	g_return_if_fail (EEL_IS_PASSWORD_DIALOG (callback_data));

	password_dialog = EEL_PASSWORD_DIALOG (callback_data);

	/* Move the focus to the password entry */
	eel_caption_table_entry_grab_focus (EEL_CAPTION_TABLE (password_dialog->details->table), 
						 CAPTION_TABLE_PASSWORD_ROW);
}

static void
dialog_close_callback (GtkWidget *widget, gpointer callback_data)
{
	EelPasswordDialog *password_dialog;

	g_return_if_fail (callback_data != NULL);
	g_return_if_fail (EEL_IS_PASSWORD_DIALOG (callback_data));

	password_dialog = EEL_PASSWORD_DIALOG (callback_data);

	gtk_widget_hide (widget);
}

/* Caption table callbacks */
static void
caption_table_activate_callback (GtkWidget *widget, guint entry, gpointer callback_data)
{
	EelPasswordDialog *password_dialog;

	g_return_if_fail (callback_data != NULL);
	g_return_if_fail (EEL_IS_PASSWORD_DIALOG (callback_data));

	password_dialog = EEL_PASSWORD_DIALOG (callback_data);

	/* If the username entry was activated, simply advance the focus to the password entry */
	if (entry == CAPTION_TABLE_USERNAME_ROW) {
		eel_caption_table_entry_grab_focus (EEL_CAPTION_TABLE (password_dialog->details->table), 
							 CAPTION_TABLE_PASSWORD_ROW);
	}
	/* If the password entry was activated, then simulate and OK button press to continue to hide process */
	else if (entry == CAPTION_TABLE_PASSWORD_ROW) {
		GtkWidget *button;
		
		button = g_list_nth_data (GNOME_DIALOG (password_dialog)->buttons, DIALOG_OK_BUTTON);
		
		g_assert (button != NULL);
		g_assert (GTK_IS_BUTTON (button));

		gtk_button_clicked (GTK_BUTTON (button));
	}
}

/* Public EelPasswordDialog methods */
GtkWidget *
eel_password_dialog_new (const char	*dialog_title,
			      const char	*message,
			      const char	*username,
			      const char	*password,
			      gboolean		readonly_username)
{
	EelPasswordDialog *password_dialog;

	password_dialog = EEL_PASSWORD_DIALOG (gtk_widget_new (eel_password_dialog_get_type (), NULL));

	gnome_dialog_constructv (GNOME_DIALOG (password_dialog), dialog_title, stock_buttons);

	/* Setup the dialog */
	gtk_window_set_policy (GTK_WINDOW (password_dialog), 
			      FALSE,	/* allow_shrink */
			      TRUE,	/* allow_grow */
			      FALSE);	/* auto_shrink */

 	gtk_window_set_position (GTK_WINDOW (password_dialog), GTK_WIN_POS_CENTER);
	gtk_window_set_modal (GTK_WINDOW (password_dialog), TRUE);

 	gtk_container_set_border_width (GTK_CONTAINER (password_dialog), DIALOG_BORDER_WIDTH);

	gnome_dialog_set_default (GNOME_DIALOG (password_dialog), DIALOG_OK_BUTTON);

	/* Dont close the dialog on click.  We'll mange the destruction our selves */
	gnome_dialog_set_close (GNOME_DIALOG (password_dialog), FALSE);

	/* Make the close operation 'just_hide' the dialog - not nuke it */
	gnome_dialog_close_hides (GNOME_DIALOG (password_dialog), TRUE);
	
	gtk_signal_connect_while_alive (GTK_OBJECT (password_dialog),
					"show",
					GTK_SIGNAL_FUNC (dialog_show_callback),
					(gpointer) password_dialog,
					GTK_OBJECT (password_dialog));
	
	gtk_signal_connect_while_alive (GTK_OBJECT (password_dialog),
					"close",
					GTK_SIGNAL_FUNC (dialog_close_callback),
					(gpointer) password_dialog,
					GTK_OBJECT (password_dialog));

	/* The table that holds the captions */
	password_dialog->details->table = eel_caption_table_new (2);
	
	gtk_signal_connect (GTK_OBJECT (password_dialog->details->table),
			    "activate",
			    GTK_SIGNAL_FUNC (caption_table_activate_callback),
			    password_dialog);

	eel_caption_table_set_row_info (EEL_CAPTION_TABLE (password_dialog->details->table),
					     CAPTION_TABLE_USERNAME_ROW,
					     _("Username:"),
					     "",
					     TRUE,
					     TRUE);

	eel_caption_table_set_row_info (EEL_CAPTION_TABLE (password_dialog->details->table),
					     CAPTION_TABLE_PASSWORD_ROW,
					     _("Password:"),
					     "",
					     FALSE,
					     FALSE);
	
	/* Configure the GNOME_DIALOG's vbox */
 	g_assert (GNOME_DIALOG (password_dialog)->vbox != NULL);

	gtk_box_set_spacing (GTK_BOX (GNOME_DIALOG (password_dialog)->vbox), 10);
	
	if (message) {
		password_dialog->details->message =
			GTK_LABEL (gtk_label_new (message));
		gtk_label_set_justify (password_dialog->details->message, GTK_JUSTIFY_LEFT);
		gtk_label_set_line_wrap (password_dialog->details->message, TRUE);

		gtk_box_pack_start (GTK_BOX (GNOME_DIALOG (password_dialog)->vbox),
				    GTK_WIDGET (password_dialog->details->message),
				    TRUE,	/* expand */
				    TRUE,	/* fill */
				    0);		/* padding */
	}

	gtk_box_pack_start (GTK_BOX (GNOME_DIALOG (password_dialog)->vbox),
			    password_dialog->details->table,
			    TRUE,	/* expand */
			    TRUE,	/* fill */
			    0);		/* padding */
#if 0 /* FIXME: disabled for PR2 originally, when will we re-enable */
	password_dialog->details->remember_button = 
		gtk_check_button_new_with_label (_("Remember this password"));

	gtk_box_pack_end (GTK_BOX (GNOME_DIALOG (password_dialog)->vbox),
			  password_dialog->details->remember_button,
			  TRUE,	/* expand */
			  TRUE,	/* fill */
			  4);		/* padding */
#else
	password_dialog->details->remember_button = NULL;
#endif	
	/* Configure the table */
 	gtk_container_set_border_width (GTK_CONTAINER(password_dialog->details->table), CAPTION_TABLE_BORDER_WIDTH);
	
	gtk_widget_show_all (GNOME_DIALOG (password_dialog)->vbox);
	
	eel_password_dialog_set_username (password_dialog, username);
	eel_password_dialog_set_password (password_dialog, password);
	eel_password_dialog_set_readonly_username (password_dialog, readonly_username);
	
	return GTK_WIDGET (password_dialog);
}

gboolean
eel_password_dialog_run_and_block (EelPasswordDialog *password_dialog)
{
	gint button_clicked;

	g_return_val_if_fail (password_dialog != NULL, FALSE);
	g_return_val_if_fail (EEL_IS_PASSWORD_DIALOG (password_dialog), FALSE);
	
	button_clicked = gnome_dialog_run_and_close (GNOME_DIALOG (password_dialog));

	return (button_clicked == DIALOG_OK_BUTTON);
}

void
eel_password_dialog_set_username (EelPasswordDialog	*password_dialog,
				       const char		*username)
{
	g_return_if_fail (password_dialog != NULL);
	g_return_if_fail (EEL_IS_PASSWORD_DIALOG (password_dialog));

	eel_caption_table_set_entry_text (EEL_CAPTION_TABLE (password_dialog->details->table),
					       CAPTION_TABLE_USERNAME_ROW,
					       username);
}

void
eel_password_dialog_set_password (EelPasswordDialog	*password_dialog,
				       const char		*password)
{
	g_return_if_fail (password_dialog != NULL);
	g_return_if_fail (EEL_IS_PASSWORD_DIALOG (password_dialog));
	
	eel_caption_table_set_entry_text (EEL_CAPTION_TABLE (password_dialog->details->table),
					       CAPTION_TABLE_PASSWORD_ROW,
					       password);
}

void
eel_password_dialog_set_readonly_username (EelPasswordDialog	*password_dialog,
						gboolean		readonly)
{
	g_return_if_fail (password_dialog != NULL);
	g_return_if_fail (EEL_IS_PASSWORD_DIALOG (password_dialog));
	
	eel_caption_table_set_entry_readonly (EEL_CAPTION_TABLE (password_dialog->details->table),
						   CAPTION_TABLE_USERNAME_ROW,
						   readonly);
}

char *
eel_password_dialog_get_username (EelPasswordDialog *password_dialog)
{
	g_return_val_if_fail (password_dialog != NULL, NULL);
	g_return_val_if_fail (EEL_IS_PASSWORD_DIALOG (password_dialog), NULL);

	return eel_caption_table_get_entry_text (EEL_CAPTION_TABLE (password_dialog->details->table),
						      CAPTION_TABLE_USERNAME_ROW);
}

char *
eel_password_dialog_get_password (EelPasswordDialog *password_dialog)
{
	g_return_val_if_fail (password_dialog != NULL, NULL);
	g_return_val_if_fail (EEL_IS_PASSWORD_DIALOG (password_dialog), NULL);

	return eel_caption_table_get_entry_text (EEL_CAPTION_TABLE (password_dialog->details->table),
						      CAPTION_TABLE_PASSWORD_ROW);
}

gboolean
eel_password_dialog_get_remember (EelPasswordDialog *password_dialog)
{
	g_return_val_if_fail (password_dialog != NULL, FALSE);
	g_return_val_if_fail (EEL_IS_PASSWORD_DIALOG (password_dialog), FALSE);

#if 0	/* remove for PR2 */
	return gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (password_dialog->details->remember_button));
#else
	return FALSE;
#endif
}

void
eel_password_dialog_set_remember (EelPasswordDialog *password_dialog,
				       gboolean                remember)
{
	g_return_if_fail (password_dialog != NULL);
	g_return_if_fail (EEL_IS_PASSWORD_DIALOG (password_dialog));

#if 0	/* remove for PR2 */
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (password_dialog->details->remember_button),
				      remember);
#endif
}

void
eel_password_dialog_set_remember_label_text (EelPasswordDialog *password_dialog,
						  const char             *remember_label_text)
{
#if 0	/* remove for PR2 */
	GtkWidget *label;

	g_return_if_fail (password_dialog != NULL);
	g_return_if_fail (EEL_IS_PASSWORD_DIALOG (password_dialog));

	label = GTK_BIN (password_dialog->details->remember_button)->child;

	g_assert (label != NULL);
	g_assert (GTK_IS_LABEL (label));

	gtk_label_set_text (GTK_LABEL (label), remember_label_text);
#endif
}
