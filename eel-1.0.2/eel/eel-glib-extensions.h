/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */

/* eel-glib-extensions.h - interface for new functions that conceptually
                                belong in glib. Perhaps some of these will be
                                actually rolled into glib someday.

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

#ifndef EEL_GLIB_EXTENSIONS_H
#define EEL_GLIB_EXTENSIONS_H

#include <time.h>
#include <glib.h>

/* A gboolean variant for bit fields. */
typedef guint eel_boolean_bit;

/* Use this until we can switch to G_N_ELEMENTS. */
#define EEL_N_ELEMENTS(array) (sizeof (array) / sizeof ((array)[0]))

/* Callback functions that have user data. */
typedef int      (* EelCompareFunction)   (gconstpointer a,
					   gconstpointer b,
					   gpointer callback_data);
typedef int      (* EelSearchFunction)    (gconstpointer item,
					   gpointer callback_data);

/* Predicate. */
typedef gboolean (* EelPredicateFunction) (gpointer data,
					   gpointer callback_data);

/* Date & time functions. */
GDate *     eel_g_date_new_tm                           (struct tm             *time_pieces);
char *      eel_strdup_strftime                         (const char            *format,
							 struct tm             *time_pieces);

/* environment manipulation functions */
int         eel_setenv                                  (const char            *name,
							 const char            *value,
							 gboolean               overwrite);
void        eel_unsetenv                                (const char            *name);

/* GList functions. */
gboolean    eel_g_list_exactly_one_item                 (GList                 *list);
gboolean    eel_g_list_more_than_one_item               (GList                 *list);
gboolean    eel_g_list_equal                            (GList                 *list_a,
							 GList                 *list_b);
GList *     eel_g_list_copy                             (GList                 *list);
void        eel_g_list_safe_for_each                    (GList                 *list,
							 GFunc                  function,
							 gpointer               user_data);
GList *     eel_g_list_sort_custom                      (GList                 *list,
							 EelCompareFunction     compare,
							 gpointer               user_data);
gboolean    eel_g_lists_sort_and_check_for_intersection (GList                **list_a,
							 GList                **list_b);
GList *     eel_g_list_partition                        (GList                 *list,
							 EelPredicateFunction   predicate,
							 gpointer               user_data,
							 GList                **removed);

/* List functions for lists of g_free'able objects. */
void        eel_g_list_free_deep                        (GList                 *list);
void        eel_g_list_free_deep_custom                 (GList                 *list,
							 GFunc                  element_free_func,
							 gpointer               user_data);

/* GSList functions. */
GList *     eel_g_list_from_g_slist                     (GSList                *list);
GSList *    eel_g_slist_from_g_list                     (GList                 *list);

/* List functions for slists of g_free'able objects. */
void        eel_g_slist_free_deep                       (GSList                *list);
void        eel_g_slist_free_deep_custom                (GSList                *list,
							 GFunc                  element_free_func,
							 gpointer               user_data);

/* List functions for lists of C strings. */
gboolean    eel_g_str_list_equal                        (GList                 *str_list_a,
							 GList                 *str_list_b);
GList *     eel_g_str_list_copy                         (GList                 *str_list);
GList *     eel_g_str_list_alphabetize                  (GList                 *str_list);

/* GString functions */
void        eel_g_string_append_len                     (GString               *string,
							 const char            *characters,
							 int                    length);

/* GHashTable functions */
GHashTable *eel_g_hash_table_new_free_at_exit           (GHashFunc              hash_function,
							 GCompareFunc           key_compare_function,
							 const char            *display_name);
void        eel_g_hash_table_safe_for_each              (GHashTable            *hash_table,
							 GHFunc                 callback,
							 gpointer               callback_data);
gboolean    eel_g_hash_table_remove_deep_custom         (GHashTable            *hash_table,
							 gconstpointer          key,
							 GFunc                  key_free_func,
							 gpointer               key_free_data,
							 GFunc                  value_free_func,
							 gpointer               value_free_data);
gboolean    eel_g_hash_table_remove_deep                (GHashTable            *hash_table,
							 gconstpointer          key);
void        eel_g_hash_table_destroy_deep_custom        (GHashTable            *hash_table,
							 GFunc                  key_free_func,
							 gpointer               key_free_data,
							 GFunc                  value_free_func,
							 gpointer               value_free_data);
void        eel_g_hash_table_destroy_deep               (GHashTable            *hash_table);

/* GPtrArray functions */
GPtrArray * eel_g_ptr_array_new_from_list               (GList                 *list);
void        eel_g_ptr_array_sort                        (GPtrArray             *array,
							 EelCompareFunction     compare_callback,
							 gpointer               callback_data);
int         eel_g_ptr_array_search                      (GPtrArray             *array,
							 EelSearchFunction      search_callback,
							 gpointer               callback_data,
							 gboolean               match_only);

/* NULL terminated string arrays (strv). */
int         eel_g_strv_find                             (char                 **strv,
							 const char            *find_me);

/* return the time in microseconds since the machine was started */
gint64      eel_get_system_time                         (void);

/* shell */
char *      eel_shell_quote                             (const char            *string);

/* math */
int         eel_round                                   (double                 d);

/* Locale */
gboolean    eel_dumb_down_for_multi_byte_locale_hack    (void);

/* A GCompareFunc for integers */
int         eel_compare_integer                         (gconstpointer          a,
							 gconstpointer          b);

/* Return the operating system name: Linux, Solaris, etc. */
char *      eel_get_operating_system_name               (void);

#endif /* EEL_GLIB_EXTENSIONS_H */
