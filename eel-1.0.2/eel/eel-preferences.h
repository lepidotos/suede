/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */

/* eel-preferences.c - Preference peek/poke/notify interface.

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

#ifndef EEL_PREFERENCES_H
#define EEL_PREFERENCES_H

#include <gtk/gtkobject.h>
#include <libgnome/gnome-defs.h>
#include <eel/eel-string-list.h>

BEGIN_GNOME_DECLS

/*
 * A callback which you can register to to be notified when a particular
 * preference changes.
 */
typedef void (*EelPreferencesCallback) (gpointer callback_data);

/* User level */

/* Note that there's a function to get the number of user levels, but there's
 * a lot of code elsewhere that assumes three levels. Publicizing the numbers
 * of these levels lets that other code be coherent and less error-prone.
 */
#define EEL_USER_LEVEL_NOVICE		0
#define EEL_USER_LEVEL_INTERMEDIATE	1
#define EEL_USER_LEVEL_ADVANCED		2

char *         eel_preferences_get_user_level_name_for_display (int                      user_level);
char *         eel_preferences_get_user_level_name_for_storage (int                      user_level);
int            eel_preferences_get_user_level                  (void);
void           eel_preferences_set_user_level                  (int                      user_level);
int            eel_preferences_user_level_clamp                (int                      user_level);
gboolean       eel_preferences_user_level_is_valid             (int                      user_level);

/* Preferences getters and setters */
gboolean       eel_preferences_get_boolean                     (const char              *name);
void           eel_preferences_set_boolean                     (const char              *name,
								gboolean                 boolean_value);
int            eel_preferences_get_integer                     (const char              *name);
void           eel_preferences_set_integer                     (const char              *name,
								int                      int_value);
char *         eel_preferences_get                             (const char              *name);
void           eel_preferences_set                             (const char              *name,
								const char              *string_value);
EelStringList *eel_preferences_get_string_list                 (const char              *name);
void           eel_preferences_set_string_list                 (const char              *name,
								const EelStringList     *string_list_value);

/* Default values getters and setters */
gboolean       eel_preferences_default_get_boolean             (const char              *name,
								int                      user_level);
void           eel_preferences_default_set_boolean             (const char              *name,
								int                      user_level,
								gboolean                 boolean_value);
int            eel_preferences_default_get_integer             (const char              *name,
								int                      user_level);
void           eel_preferences_default_set_integer             (const char              *name,
								int                      user_level,
								int                      int_value);
char *         eel_preferences_default_get_string              (const char              *name,
								int                      user_level);
void           eel_preferences_default_set_string              (const char              *name,
								int                      user_level,
								const char              *string_value);
EelStringList *eel_preferences_default_get_string_list         (const char              *name,
								int                      user_level);
void           eel_preferences_default_set_string_list         (const char              *name,
								int                      user_level,
								const EelStringList     *string_list_value);
/* Callbacks */
void           eel_preferences_add_callback                    (const char              *name,
								EelPreferencesCallback   callback,
								gpointer                 callback_data);
void           eel_preferences_add_callback_while_alive        (const char              *name,
								EelPreferencesCallback   callback,
								gpointer                 callback_data,
								GtkObject               *alive_object);
void           eel_preferences_remove_callback                 (const char              *name,
								EelPreferencesCallback   callback,
								gpointer                 callback_data);

/* Variables that are automatically updated (lightweight "callbacks") */
void           eel_preferences_add_auto_string                 (const char              *name,
								const char             **storage);
void           eel_preferences_add_auto_string_list            (const char              *name,
								const EelStringList    **storage);
void           eel_preferences_add_auto_integer                (const char              *name,
								int                     *storage);
void           eel_preferences_add_auto_boolean                (const char              *name,
								gboolean                *storage);
void           eel_preferences_remove_auto_string              (const char              *name,
								const char             **storage);
void           eel_preferences_remove_auto_string_list         (const char              *name,
								const EelStringList    **storage);
void           eel_preferences_remove_auto_integer             (const char              *name,
								int                     *storage);
void           eel_preferences_remove_auto_boolean             (const char              *name,
								int                     *storage);

/* Preferences attributes */
int            eel_preferences_get_visible_user_level          (const char              *name);
void           eel_preferences_set_visible_user_level          (const char              *name,
								int                      visible_user_level);
gboolean       eel_preferences_get_is_invisible                (const char              *name);
void           eel_preferences_set_is_invisible                (const char              *name,
								gboolean                 invisible);
char *         eel_preferences_get_description                 (const char              *name);
void           eel_preferences_set_description                 (const char              *name,
								const char              *description);
char *         eel_preferences_get_enumeration_id              (const char              *name);
void           eel_preferences_set_enumeration_id              (const char              *name,
								const char              *enumeration_id);
gboolean       eel_preferences_monitor_directory               (const char              *directory);
gboolean       eel_preferences_visible_in_current_user_level   (const char              *name);
gboolean       eel_preferences_is_visible                      (const char              *name);
void           eel_preferences_initialize                      (const char              *storage_path);

END_GNOME_DECLS

#endif /* EEL_PREFERENCES_H */
