/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*-

   nautilus-merged-directory.c: Subclass of NautilusDirectory to implement the
   virtual merged directory.
 
   Copyright (C) 1999, 2000 Eazel, Inc.
  
   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.
  
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.
  
   You should have received a copy of the GNU General Public
   License along with this program; if not, write to the
   Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
  
   Author: Darin Adler <darin@bentspoon.com>
*/

#include <config.h>
#include "nautilus-merged-directory.h"

#include "nautilus-directory-private.h"
#include "nautilus-file.h"
#include <eel/eel-glib-extensions.h>
#include <eel/eel-gtk-macros.h>
#include <gtk/gtksignal.h>

struct NautilusMergedDirectoryDetails {
	GList *directories;
	GList *directories_not_done_loading;
	GHashTable *callbacks;
	GHashTable *monitors;
};

typedef struct {
	/* Basic configuration. */
	NautilusMergedDirectory *merged;
	NautilusDirectoryCallback callback;
	gpointer callback_data;

	GList *wait_for_attributes;
	gboolean wait_for_file_list;

	GList *non_ready_directories;
	GList *merged_file_list;
} MergedCallback;

typedef struct {
	NautilusMergedDirectory *merged;

	gboolean monitor_hidden_files;
	gboolean monitor_backup_files;
	GList *monitor_attributes;
} MergedMonitor;

enum {
	ADD_REAL_DIRECTORY,
	REMOVE_REAL_DIRECTORY,
	LAST_SIGNAL
};

static guint signals[LAST_SIGNAL];

static void     nautilus_merged_directory_initialize       (gpointer                 object,
							    gpointer                 klass);
static void     nautilus_merged_directory_initialize_class (gpointer                 klass);
static void     remove_all_real_directories                (NautilusMergedDirectory *merged);
static guint    merged_callback_hash                       (gconstpointer            merged_callback);
static gboolean merged_callback_equal                      (gconstpointer            merged_callback,
							    gconstpointer            merged_callback_2);

EEL_DEFINE_CLASS_BOILERPLATE (NautilusMergedDirectory,
				   nautilus_merged_directory,
				   NAUTILUS_TYPE_DIRECTORY)

static void
nautilus_merged_directory_initialize (gpointer object, gpointer klass)
{
	NautilusMergedDirectory *merged;

	merged = NAUTILUS_MERGED_DIRECTORY (object);

	merged->details = g_new0 (NautilusMergedDirectoryDetails, 1);
	merged->details->callbacks = g_hash_table_new
		(merged_callback_hash, merged_callback_equal);
	merged->details->monitors = g_hash_table_new (NULL, NULL);
}

static void
merged_destroy (GtkObject *object)
{
	NautilusMergedDirectory *merged;

	merged = NAUTILUS_MERGED_DIRECTORY (object);

	remove_all_real_directories (merged);

	if (g_hash_table_size (merged->details->callbacks) != 0) {
		g_warning ("call_when_ready still pending when merged virtual directory is destroyed");
	}
	if (g_hash_table_size (merged->details->monitors) != 0) {
		g_warning ("file monitor still active when merged virtual directory is destroyed");
	}

	g_hash_table_destroy (merged->details->callbacks);
	g_hash_table_destroy (merged->details->monitors);
	g_free (merged->details);

	EEL_CALL_PARENT (GTK_OBJECT_CLASS, destroy, (object));
}

static guint
merged_callback_hash (gconstpointer merged_callback_as_pointer)
{
	const MergedCallback *merged_callback;

	merged_callback = merged_callback_as_pointer;
	return GPOINTER_TO_UINT (merged_callback->callback)
		^ GPOINTER_TO_UINT (merged_callback->callback_data);
}

static gboolean
merged_callback_equal (gconstpointer merged_callback_as_pointer,
		       gconstpointer merged_callback_as_pointer_2)
{
	const MergedCallback *merged_callback, *merged_callback_2;

	merged_callback = merged_callback_as_pointer;
	merged_callback_2 = merged_callback_as_pointer_2;

	return merged_callback->callback == merged_callback_2->callback
		&& merged_callback->callback_data == merged_callback_2->callback_data;
}

static void
merged_callback_destroy (MergedCallback *merged_callback)
{
	g_assert (merged_callback != NULL);
	g_assert (NAUTILUS_IS_MERGED_DIRECTORY (merged_callback->merged));

	eel_g_list_free_deep (merged_callback->wait_for_attributes);
	g_list_free (merged_callback->non_ready_directories);
	nautilus_file_list_free (merged_callback->merged_file_list);
	g_free (merged_callback);
}

static void
merged_callback_check_done (MergedCallback *merged_callback)
{
	/* Check if we are ready. */
	if (merged_callback->non_ready_directories != NULL) {
		return;
	}

	/* Remove from the hash table before sending it. */
	g_hash_table_remove (merged_callback->merged->details->callbacks, merged_callback);

	/* We are ready, so do the real callback. */
	(* merged_callback->callback) (NAUTILUS_DIRECTORY (merged_callback->merged),
				       merged_callback->merged_file_list,
				       merged_callback->callback_data);

	/* And we are done. */
	merged_callback_destroy (merged_callback);
}

static void
merged_callback_remove_directory (MergedCallback *merged_callback,
				  NautilusDirectory *directory)
{
	merged_callback->non_ready_directories = g_list_remove
		(merged_callback->non_ready_directories,
		 directory);
	merged_callback_check_done (merged_callback);
}

static void
directory_ready_callback (NautilusDirectory *directory,
			  GList *files,
			  gpointer callback_data)
{
	MergedCallback *merged_callback;

	g_assert (NAUTILUS_IS_DIRECTORY (directory));
	g_assert (callback_data != NULL);

	merged_callback = callback_data;
	g_assert (g_list_find (merged_callback->non_ready_directories, directory) != NULL);

	/* Update based on this call. */
	merged_callback->merged_file_list = g_list_concat
		(merged_callback->merged_file_list,
		 nautilus_file_list_copy (files));

	/* Check if we are ready. */
	merged_callback_remove_directory (merged_callback, directory);
}

static void
merged_call_when_ready (NautilusDirectory *directory,
			GList *file_attributes,
			gboolean wait_for_file_list,
			NautilusDirectoryCallback callback,
			gpointer callback_data)
{
	NautilusMergedDirectory *merged;
	MergedCallback search_key, *merged_callback;
	GList *node;

	merged = NAUTILUS_MERGED_DIRECTORY (directory);

	/* Check to be sure we aren't overwriting. */
	search_key.callback = callback;
	search_key.callback_data = callback_data;
	if (g_hash_table_lookup (merged->details->callbacks, &search_key) != NULL) {
		g_warning ("tried to add a new callback while an old one was pending");
		return;
	}

	/* Create a merged_callback record. */
	merged_callback = g_new0 (MergedCallback, 1);
	merged_callback->merged = merged;
	merged_callback->callback = callback;
	merged_callback->callback_data = callback_data;
	merged_callback->wait_for_attributes = eel_g_str_list_copy (file_attributes);
	merged_callback->wait_for_file_list = wait_for_file_list;
	for (node = merged->details->directories; node != NULL; node = node->next) {
		merged_callback->non_ready_directories = g_list_prepend
			(merged_callback->non_ready_directories, node->data);
	}

	/* Put it in the hash table. */
	g_hash_table_insert (merged->details->callbacks,
			     merged_callback, merged_callback);

	/* Handle the pathological case where there are no directories. */
	if (merged->details->directories == NULL) {
		merged_callback_check_done (merged_callback);
	}

	/* Now tell all the directories about it. */
	for (node = merged->details->directories; node != NULL; node = node->next) {
		nautilus_directory_call_when_ready
			(node->data,
			 merged_callback->wait_for_attributes,
			 merged_callback->wait_for_file_list,
			 directory_ready_callback, merged_callback);
	}
}

static void
merged_cancel_callback (NautilusDirectory *directory,
			NautilusDirectoryCallback callback,
			gpointer callback_data)
{
	NautilusMergedDirectory *merged;
	MergedCallback search_key, *merged_callback;
	GList *node;

	merged = NAUTILUS_MERGED_DIRECTORY (directory);

	/* Find the entry in the table. */
	search_key.callback = callback;
	search_key.callback_data = callback_data;
	merged_callback = g_hash_table_lookup (merged->details->callbacks, &search_key);
	if (merged_callback == NULL) {
		return;
	}

	/* Remove from the hash table before working with it. */
	g_hash_table_remove (merged_callback->merged->details->callbacks, merged_callback);

	/* Tell all the directories to cancel the call. */
	for (node = merged_callback->non_ready_directories; node != NULL; node = node->next) {
		nautilus_directory_cancel_callback
			(node->data,
			 directory_ready_callback, merged_callback);
	}
	merged_callback_destroy (merged_callback);
}

static void
build_merged_callback_list (NautilusDirectory *directory,
			    GList *file_list,
			    gpointer callback_data)
{
	GList **merged_list;

	merged_list = callback_data;
	*merged_list = g_list_concat (*merged_list,
				      nautilus_file_list_copy (file_list));
}

/* Create a monitor on each of the directories in the list. */
static void
merged_file_monitor_add (NautilusDirectory *directory,
			 gconstpointer client,
			 gboolean monitor_hidden_files,
			 gboolean monitor_backup_files,
			 GList *file_attributes,
			 NautilusDirectoryCallback callback,
			 gpointer callback_data)
{
	NautilusMergedDirectory *merged;
	MergedMonitor *monitor;
	GList *node;
	GList *merged_callback_list;

	merged = NAUTILUS_MERGED_DIRECTORY (directory);

	/* Map the client to a unique value so this doesn't interfere
	 * with direct monitoring of the directory by the same client.
	 */
	monitor = g_hash_table_lookup (merged->details->monitors, client);
	if (monitor != NULL) {
		g_assert (monitor->merged == merged);
		eel_g_list_free_deep (monitor->monitor_attributes);
	} else {
		monitor = g_new0 (MergedMonitor, 1);
		monitor->merged = merged;
		g_hash_table_insert (merged->details->monitors,
				     (gpointer) client, monitor);
	}
	monitor->monitor_hidden_files = monitor_hidden_files;
	monitor->monitor_backup_files = monitor_backup_files;
	monitor->monitor_attributes = eel_g_str_list_copy (file_attributes);
	
	/* Call through to the real directory add calls. */
	merged_callback_list = NULL;
	for (node = merged->details->directories; node != NULL; node = node->next) {
		nautilus_directory_file_monitor_add
			(node->data, monitor,
			 monitor_hidden_files, monitor_backup_files,
			 file_attributes,
			 build_merged_callback_list, &merged_callback_list);
	}
	if (callback != NULL) {
		(* callback) (directory, merged_callback_list, callback_data);
	}
	nautilus_file_list_free (merged_callback_list);
}

/* Remove the monitor from each of the directories in the list. */
static void
merged_file_monitor_remove (NautilusDirectory *directory,
			    gconstpointer client)
{
	NautilusMergedDirectory *merged;
	MergedMonitor *monitor;
	GList *node;
	
	merged = NAUTILUS_MERGED_DIRECTORY (directory);
	
	/* Map the client to the value used by the earlier add call. */
        monitor = g_hash_table_lookup (merged->details->monitors, client);
	if (monitor == NULL) {
		return;
	}
	g_hash_table_remove (merged->details->monitors, client);

	/* Call through to the real directory remove calls. */
	for (node = merged->details->directories; node != NULL; node = node->next) {
		nautilus_directory_file_monitor_remove
			(node->data, monitor);
	}

	eel_g_list_free_deep (monitor->monitor_attributes);
	g_free (monitor);
}

static void
merged_force_reload (NautilusDirectory *directory)
{
	NautilusMergedDirectory *merged;
	GList *node;

	merged = NAUTILUS_MERGED_DIRECTORY (directory);

	/* Call through to the real force_reload calls. */
	for (node = merged->details->directories; node != NULL; node = node->next) {
		nautilus_directory_force_reload (node->data);
	}
}

/* Return true if any directory in the list does. */
static gboolean
merged_contains_file (NautilusDirectory *directory,
		      NautilusFile *file)
{
	NautilusMergedDirectory *merged;
	GList *node;

	merged = NAUTILUS_MERGED_DIRECTORY (directory);

	for (node = merged->details->directories; node != NULL; node = node->next) {
		if (nautilus_directory_contains_file (node->data, file)) {
			return TRUE;
		}
	}
	return FALSE;
}

/* Return true only if all directories in the list do. */
static gboolean
merged_are_all_files_seen (NautilusDirectory *directory)
{
	NautilusMergedDirectory *merged;
	GList *node;

	merged = NAUTILUS_MERGED_DIRECTORY (directory);

	for (node = merged->details->directories; node != NULL; node = node->next) {
		if (!nautilus_directory_are_all_files_seen (node->data)) {
			return FALSE;
		}
	}
	return TRUE;
}

/* Return true if any directory in the list does. */
static gboolean
merged_is_not_empty (NautilusDirectory *directory)
{
	NautilusMergedDirectory *merged;
	GList *node;

	merged = NAUTILUS_MERGED_DIRECTORY (directory);

	for (node = merged->details->directories; node != NULL; node = node->next) {
		if (nautilus_directory_is_not_empty (node->data)) {
			return TRUE;
		}
	}
	return FALSE;
}

static void
forward_files_added_cover (NautilusDirectory *real_directory,
			   GList *files,
			   gpointer callback_data)
{
	nautilus_directory_emit_files_added (NAUTILUS_DIRECTORY (callback_data), files);
}

static void
forward_files_changed_cover (NautilusDirectory *real_directory,
			     GList *files,
			     gpointer callback_data)
{
	nautilus_directory_emit_files_changed (NAUTILUS_DIRECTORY (callback_data), files);
}

static void
done_loading_callback (NautilusDirectory *real_directory,
		       NautilusMergedDirectory *merged)
{
	merged->details->directories_not_done_loading = g_list_remove
		(merged->details->directories_not_done_loading, real_directory);
	if (merged->details->directories_not_done_loading == NULL) {
		nautilus_directory_emit_done_loading (NAUTILUS_DIRECTORY (merged));
	}
}

static void
monitor_add_directory (gpointer key,
		       gpointer value,
		       gpointer callback_data)
{
	MergedMonitor *monitor;
	
	monitor = value;
	nautilus_directory_file_monitor_add
		(NAUTILUS_DIRECTORY (callback_data), monitor,
		 monitor->monitor_hidden_files,
		 monitor->monitor_backup_files,
		 monitor->monitor_attributes,
		 forward_files_added_cover, monitor->merged);
}

static void
merged_add_real_directory (NautilusMergedDirectory *merged,
			   NautilusDirectory *real_directory)
{
	g_return_if_fail (NAUTILUS_IS_MERGED_DIRECTORY (merged));
	g_return_if_fail (NAUTILUS_IS_DIRECTORY (real_directory));
	g_return_if_fail (!NAUTILUS_IS_MERGED_DIRECTORY (real_directory));
	g_return_if_fail (g_list_find (merged->details->directories, real_directory) == NULL);

	/* Add to our list of directories. */
	nautilus_directory_ref (real_directory);
	merged->details->directories = g_list_prepend
		(merged->details->directories, real_directory);
	merged->details->directories_not_done_loading = g_list_prepend
		(merged->details->directories_not_done_loading, real_directory);

	gtk_signal_connect (GTK_OBJECT (real_directory),
			    "done_loading",
			    done_loading_callback,
			    merged);

	/* FIXME bugzilla.gnome.org 45084: The done_loading part won't work for the case where
         * we have no directories in our list.
	 */

	/* Add the directory to any extant monitors. */
	g_hash_table_foreach (merged->details->monitors,
			      monitor_add_directory,
			      real_directory);
	/* FIXME bugzilla.gnome.org 42541: Do we need to add the directory to callbacks too? */

	gtk_signal_connect (GTK_OBJECT (real_directory),
			    "files_added",
			    forward_files_added_cover,
			    merged);
	gtk_signal_connect (GTK_OBJECT (real_directory),
			    "files_changed",
			    forward_files_changed_cover,
			    merged);
}

void
nautilus_merged_directory_add_real_directory (NautilusMergedDirectory *merged,
					      NautilusDirectory *real_directory)
{
	g_return_if_fail (NAUTILUS_IS_MERGED_DIRECTORY (merged));
	g_return_if_fail (NAUTILUS_IS_DIRECTORY (real_directory));
	g_return_if_fail (!NAUTILUS_IS_MERGED_DIRECTORY (real_directory));

	/* Quietly do nothing if asked to add something that's already there. */
	if (g_list_find (merged->details->directories, real_directory) != NULL) {
		return;
	}

	gtk_signal_emit (GTK_OBJECT (merged),
			 signals[ADD_REAL_DIRECTORY],
			 real_directory);
}

GList *
nautilus_merged_directory_get_real_directories (NautilusMergedDirectory *merged)
{
	return g_list_copy (merged->details->directories);
}

static void
merged_callback_remove_directory_cover (gpointer key,
					gpointer value,
					gpointer callback_data)
{
	merged_callback_remove_directory
		(value, NAUTILUS_DIRECTORY (callback_data));
}

static void
monitor_remove_directory (gpointer key,
			  gpointer value,
			  gpointer callback_data)
{
	nautilus_directory_file_monitor_remove
		(NAUTILUS_DIRECTORY (callback_data), value);
}

static void
merged_remove_real_directory (NautilusMergedDirectory *merged,
			      NautilusDirectory *real_directory)
{
	g_return_if_fail (NAUTILUS_IS_MERGED_DIRECTORY (merged));
	g_return_if_fail (NAUTILUS_IS_DIRECTORY (real_directory));
	g_return_if_fail (g_list_find (merged->details->directories, real_directory) != NULL);

	/* Remove this directory from callbacks and monitors. */
	eel_g_hash_table_safe_for_each
		(merged->details->callbacks,
		 merged_callback_remove_directory_cover,
		 real_directory);
	g_hash_table_foreach
		(merged->details->monitors,
		 monitor_remove_directory,
		 real_directory);

	/* Disconnect all the signals. */
	gtk_signal_disconnect_by_data (GTK_OBJECT (real_directory), merged);

	/* Remove from our list of directories. */
	merged->details->directories = g_list_remove
		(merged->details->directories, real_directory);
	merged->details->directories_not_done_loading = g_list_remove
		(merged->details->directories_not_done_loading, real_directory);
	nautilus_directory_unref (real_directory);
}

void
nautilus_merged_directory_remove_real_directory (NautilusMergedDirectory *merged,
						 NautilusDirectory *real_directory)
{
	g_return_if_fail (NAUTILUS_IS_MERGED_DIRECTORY (merged));

	/* Quietly do nothing if asked to remove something that's not there. */
	if (g_list_find (merged->details->directories, real_directory) == NULL) {
		return;
	}

	gtk_signal_emit (GTK_OBJECT (merged),
			 signals[REMOVE_REAL_DIRECTORY],
			 real_directory);
}

static void
remove_all_real_directories (NautilusMergedDirectory *merged)
{
	while (merged->details->directories != NULL) {
		nautilus_merged_directory_remove_real_directory
			(merged, merged->details->directories->data);
	}
}

static void
nautilus_merged_directory_initialize_class (gpointer klass)
{
	GtkObjectClass *object_class;
	NautilusDirectoryClass *directory_class;
	NautilusMergedDirectoryClass *merged_directory_class;

	object_class = GTK_OBJECT_CLASS (klass);
	directory_class = NAUTILUS_DIRECTORY_CLASS (klass);
	merged_directory_class = NAUTILUS_MERGED_DIRECTORY_CLASS (klass);
	
	object_class->destroy = merged_destroy;

	directory_class->contains_file = merged_contains_file;
	directory_class->call_when_ready = merged_call_when_ready;
	directory_class->cancel_callback = merged_cancel_callback;
	directory_class->file_monitor_add = merged_file_monitor_add;
	directory_class->file_monitor_remove = merged_file_monitor_remove;
	directory_class->force_reload = merged_force_reload;
 	directory_class->are_all_files_seen = merged_are_all_files_seen;
	directory_class->is_not_empty = merged_is_not_empty;

	merged_directory_class->add_real_directory = merged_add_real_directory;
	merged_directory_class->remove_real_directory = merged_remove_real_directory;

	signals[ADD_REAL_DIRECTORY] 
		= gtk_signal_new ("add_real_directory",
				  GTK_RUN_LAST,
				  object_class->type,
				  GTK_SIGNAL_OFFSET (NautilusMergedDirectoryClass, 
						     add_real_directory),
				  gtk_marshal_NONE__POINTER,
				  GTK_TYPE_NONE, 1, GTK_TYPE_POINTER);
	signals[REMOVE_REAL_DIRECTORY] 
		= gtk_signal_new ("remove_real_directory",
				  GTK_RUN_LAST,
				  object_class->type,
				  GTK_SIGNAL_OFFSET (NautilusMergedDirectoryClass, 
						     remove_real_directory),
				  gtk_marshal_NONE__POINTER,
				  GTK_TYPE_NONE, 1, GTK_TYPE_POINTER);

	gtk_object_class_add_signals (object_class, signals, LAST_SIGNAL);				  
}
