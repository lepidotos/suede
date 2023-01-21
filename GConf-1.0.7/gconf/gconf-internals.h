/* GConf
 * Copyright (C) 1999, 2000 Red Hat Inc.
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

#ifndef GCONF_GCONF_INTERNALS_H
#define GCONF_GCONF_INTERNALS_H

#ifndef GCONF_ENABLE_INTERNALS
#warning "you are trying to use GConf internal functions outside of GConf. This is a Bad Idea"
#endif

#ifdef GCONF_ENABLE_INTERNALS

#include <glib.h>
#include "gconf-error.h"
#include "gconf-value.h"
#include "GConf.h"

gchar*       gconf_key_directory  (const gchar* key);
const gchar* gconf_key_key        (const gchar* key);

/* These file tests are in libgnome, I cut-and-pasted them */
enum {
  GCONF_FILE_EXISTS=(1<<0)|(1<<1)|(1<<2), /*any type of file*/
  GCONF_FILE_ISFILE=1<<0,
  GCONF_FILE_ISLINK=1<<1,
  GCONF_FILE_ISDIR=1<<2
};

gboolean gconf_file_test   (const gchar* filename, int test);
gboolean gconf_file_exists (const gchar* filename);

GConfValue*  gconf_value_from_corba_value      (const ConfigValue *value);
ConfigValue* corba_value_from_gconf_value      (GConfValue        *value);
void         fill_corba_value_from_gconf_value (GConfValue        *value,
                                                ConfigValue       *dest);
ConfigValue* invalid_corba_value               (void);


gchar* gconf_object_to_string (CORBA_Object obj,
                               GError **err);

void          fill_corba_schema_from_gconf_schema (GConfSchema        *sc,
                                                   ConfigSchema       *dest);
ConfigSchema* corba_schema_from_gconf_schema      (GConfSchema        *sc);
GConfSchema*  gconf_schema_from_corba_schema      (const ConfigSchema *cs);

const gchar*   gconf_value_type_to_string   (GConfValueType  type);
GConfValueType gconf_value_type_from_string (const gchar    *str);


gchar**       gconf_load_source_path (const gchar* filename, GError** err);

/* shouldn't be used in applications (although implemented in gconf.c) */

void     gconf_shutdown_daemon (GError **err);
gboolean gconf_ping_daemon     (void);
gboolean gconf_spawn_daemon    (GError **err);


/* Returns 0 on failure (or if the string is "0" of course) */
gulong       gconf_string_to_gulong (const gchar *str);
gboolean     gconf_string_to_double (const gchar *str,
                                     gdouble     *val);
gchar*       gconf_double_to_string (gdouble      val);
const gchar* gconf_current_locale   (void);


/* Log wrapper; we might want to not use syslog someday */
typedef enum {
  GCL_EMERG,
  GCL_ALERT,
  GCL_CRIT,
  GCL_ERR,
  GCL_WARNING,
  GCL_NOTICE,
  GCL_INFO,
  GCL_DEBUG
} GConfLogPriority;

void          gconf_log      (GConfLogPriority pri, const gchar* format, ...) G_GNUC_PRINTF (2, 3);

extern gboolean gconf_log_debug_messages;

/* return FALSE and set error if the key is bad */
gboolean      gconf_key_check(const gchar* key, GError** err);

/*
 * If these were public they'd be in gconf-value.h
 */

/* doesn't work on complicated types (only string, int, bool, float) */
GConfValue* gconf_value_new_from_string      (GConfValueType type,
                                              const gchar* str,
                                              GError** err);
/* for the complicated types */
GConfValue* gconf_value_new_list_from_string (GConfValueType list_type,
                                              const gchar* str,
					      GError** err);
GConfValue* gconf_value_new_pair_from_string (GConfValueType car_type,
                                              GConfValueType cdr_type,
                                              const gchar* str,
					      GError** err);

/* These are a hack to encode values into strings and ship them over CORBA,
 * necessary for obscure reasons (ORBit doesn't like recursive datatypes yet)
 */

/* string quoting is only public for the benefit of the test suite */

gchar* gconf_quote_string           (const gchar  *str);
gchar* gconf_unquote_string         (const gchar  *str,
                                     const gchar **end,
                                     GError      **err);
void   gconf_unquote_string_inplace (gchar        *str,
                                     gchar       **end,
                                     GError      **err);

GConfValue* gconf_value_decode (const gchar *encoded);
gchar*      gconf_value_encode (GConfValue  *val);

/* FIXME is this used? */
gchar* gconf_quote_percents (const gchar* src);

/*
 * List/pair conversion stuff
 */

GConfValue* gconf_value_list_from_primitive_list (GConfValueType  list_type,
                                                  GSList         *list,
                                                  GError        **err);
GConfValue* gconf_value_pair_from_primitive_pair (GConfValueType  car_type,
                                                  GConfValueType  cdr_type,
                                                  gconstpointer   address_of_car,
                                                  gconstpointer   address_of_cdr,
                                                  GError        **err);

GSList*  gconf_value_list_to_primitive_list_destructive (GConfValue      *val,
                                                         GConfValueType   list_type,
                                                         GError         **err);
gboolean gconf_value_pair_to_primitive_pair_destructive (GConfValue      *val,
                                                         GConfValueType   car_type,
                                                         GConfValueType   cdr_type,
                                                         gpointer         car_retloc,
                                                         gpointer         cdr_retloc,
                                                         GError         **err);


void         gconf_set_daemon_mode (gboolean     setting);
gboolean     gconf_in_daemon_mode  (void);
void         gconf_set_daemon_ior  (const gchar *ior);
const gchar* gconf_get_daemon_ior  (void);

/* Returns TRUE if there was an error, frees exception, sets err */
gboolean gconf_handle_oaf_exception (CORBA_Environment* ev, GError** err);

void gconf_nanosleep (gulong useconds);

typedef struct _GConfLock GConfLock;

GConfLock* gconf_get_lock     (const gchar  *lock_directory,
                               GError      **err);
gboolean   gconf_release_lock (GConfLock    *lock,
                               GError      **err);
GConfLock* gconf_get_lock_or_current_holder (const gchar  *lock_directory,
                                             ConfigServer *current_server,
                                             GError      **err);
ConfigServer gconf_get_current_lock_holder  (const gchar *lock_directory);

GError*  gconf_error_new  (GConfError en,
                           const gchar* format, ...) G_GNUC_PRINTF (2, 3);

void     gconf_set_error  (GError** err,
                           GConfError en,
                           const gchar* format, ...) G_GNUC_PRINTF (3, 4);

/* merge two errors into a single message */
GError*  gconf_compose_errors (GError* err1, GError* err2);

CORBA_ORB gconf_orb_get (void);

ConfigServer gconf_activate_server (gboolean  start_if_not_found,
                                    GError  **error);

char*     gconf_get_lock_dir (void);
char*     gconf_get_daemon_dir (void);

gboolean gconf_schema_validate (GConfSchema *sc,
                                GError     **err);
gboolean gconf_value_validate  (GConfValue *value,
                                GError    **err);

#define g_utf8_validate gconf_g_utf8_validate

gboolean gconf_g_utf8_validate (const gchar  *str,
                                gssize        max_len,    
                                const gchar **end);

#ifdef ENABLE_NLS
#    include <libintl.h>
#    include <config.h>
#    undef _
#    define _(String) dgettext (GETTEXT_PACKAGE, String)
#    ifdef gettext_noop
#        define N_(String) gettext_noop (String)
#    else
#        define N_(String) (String)
#    endif
#else
/* Stubs that do something close enough.  */
#    define textdomain(String) (String)
#    define bindtextdomain(Domain,Directory) (Domain)
#    define _(String) (String)
#    define N_(String) (String)
#endif

#endif /* GCONF_ENABLE_INTERNALS */

#endif /* GCONF_GCONF_INTERNALS_H */


