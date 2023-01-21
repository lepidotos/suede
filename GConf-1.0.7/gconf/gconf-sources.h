
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

#ifndef GCONF_GCONF_SOURCES_H
#define GCONF_GCONF_SOURCES_H

#include <glib.h>
#include "gconf-error.h"
#include "gconf-value.h"

/* Sources are not interchangeable; different backend engines will return 
 * GConfSource with different private elements.
 */

typedef struct _GConfBackend GConfBackend;

typedef struct _GConfSource GConfSource;

struct _GConfSource {
  guint flags;
  gchar* address;
  GConfBackend* backend;
};

typedef enum {
  /* These are an optimization to avoid calls to
   * the writable/readable methods in the backend
   * vtable
   */
  GCONF_SOURCE_ALL_WRITEABLE = 1 << 0,
  GCONF_SOURCE_ALL_READABLE = 1 << 1,
  GCONF_SOURCE_NEVER_WRITEABLE = 1 << 2, 
  GCONF_SOURCE_ALL_FLAGS = ((1 << 0) | (1 << 1))
} GConfSourceFlags;

GConfSource*  gconf_resolve_address         (const gchar* address,
                                             GError** err);

void          gconf_source_free          (GConfSource* source);

/* This is the actual thing we want to talk to, the stack of sources */
typedef struct _GConfSources GConfSources;

struct _GConfSources {
  GList* sources;
  
};

/* Even on error, this gives you an empty source list, i.e.  never
   returns NULL but may set the error if some addresses weren't
   resolved and may contain no sources.  */
GConfSources* gconf_sources_new_from_addresses (const gchar **addresses,
                                                GError   **err);
GConfSources* gconf_sources_new_from_source    (GConfSource   *source);
void          gconf_sources_free               (GConfSources  *sources);
void          gconf_sources_clear_cache        (GConfSources  *sources);
GConfValue*   gconf_sources_query_value        (GConfSources  *sources,
                                                const gchar   *key,
                                                const gchar  **locales,
                                                gboolean       use_schema_default,
                                                gboolean      *value_is_default,
                                                gboolean      *value_is_writable,
                                                gchar        **schema_name,
                                                GError   **err);
void          gconf_sources_set_value          (GConfSources  *sources,
                                                const gchar   *key,
                                                GConfValue    *value,
                                                GError   **err);
void          gconf_sources_unset_value        (GConfSources  *sources,
                                                const gchar   *key,
                                                const gchar   *locale,
                                                GError   **err);
GSList*       gconf_sources_all_entries        (GConfSources  *sources,
                                                const gchar   *dir,
                                                const gchar  **locales,
                                                GError   **err);
GSList*       gconf_sources_all_dirs           (GConfSources  *sources,
                                                const gchar   *dir,
                                                GError   **err);
gboolean      gconf_sources_dir_exists         (GConfSources  *sources,
                                                const gchar   *dir,
                                                GError   **err);
void          gconf_sources_remove_dir         (GConfSources  *sources,
                                                const gchar   *dir,
                                                GError   **err);
void          gconf_sources_set_schema         (GConfSources  *sources,
                                                const gchar   *key,
                                                const gchar   *schema_key,
                                                GError   **err);
gboolean      gconf_sources_sync_all           (GConfSources  *sources,
                                                GError   **err);


GConfMetaInfo*gconf_sources_query_metainfo     (GConfSources* sources,
                                                const gchar* key,
                                                GError** err);

GConfValue*   gconf_sources_query_default_value(GConfSources* sources,
                                                const gchar* key,
                                                const gchar** locales,
                                                gboolean* is_writable,
                                                GError** err);

#endif







