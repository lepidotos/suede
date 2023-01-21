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

#include "gconf-schema.h"
#include "gconf-internals.h"

GConfSchema*  
gconf_schema_new(void)
{
  GConfSchema* sc;

  sc = g_new0(GConfSchema, 1);

  sc->type = GCONF_VALUE_INVALID;
  sc->list_type = GCONF_VALUE_INVALID;
  sc->car_type = GCONF_VALUE_INVALID;
  sc->cdr_type = GCONF_VALUE_INVALID;

  return sc;
}

void          
gconf_schema_free(GConfSchema* sc)
{
  if (sc->locale)
    g_free(sc->locale);

  if (sc->short_desc)
    g_free(sc->short_desc);

  if (sc->long_desc)
    g_free(sc->long_desc);

  if (sc->owner)
    g_free(sc->owner);

  if (sc->default_value)
    gconf_value_free(sc->default_value);
  
  g_free(sc);
}

GConfSchema*  
gconf_schema_copy(GConfSchema* sc)
{
  GConfSchema* dest;

  dest = gconf_schema_new();

  dest->type = sc->type;
  dest->list_type = sc->list_type;
  dest->car_type = sc->car_type;
  dest->cdr_type = sc->cdr_type;

  dest->locale = sc->locale ? g_strdup(sc->locale) : NULL;
  
  dest->short_desc = sc->short_desc ? g_strdup(sc->short_desc) : NULL;

  dest->long_desc = sc->long_desc ? g_strdup(sc->long_desc) : NULL;

  dest->owner = sc->owner ? g_strdup(sc->owner) : NULL;

  dest->default_value = sc->default_value ? gconf_value_copy(sc->default_value) : NULL;
  
  return dest;
}

void          
gconf_schema_set_type(GConfSchema* sc, GConfValueType type)
{
  sc->type = type;
}

void          
gconf_schema_set_list_type(GConfSchema* sc, GConfValueType type)
{
  sc->list_type = type;
}

void          
gconf_schema_set_car_type(GConfSchema* sc, GConfValueType type)
{
  sc->car_type = type;
}

void          
gconf_schema_set_cdr_type(GConfSchema* sc, GConfValueType type)
{
  sc->cdr_type = type;
}

void
gconf_schema_set_locale(GConfSchema* sc, const gchar* locale)
{
  g_return_if_fail (locale == NULL || g_utf8_validate (locale, -1, NULL));
  
  if (sc->locale)
    g_free(sc->locale);

  if (locale)
    sc->locale = g_strdup(locale);
  else 
    sc->locale = NULL;
}

void          
gconf_schema_set_short_desc(GConfSchema* sc, const gchar* desc)
{
  g_return_if_fail (desc == NULL || g_utf8_validate (desc, -1, NULL));
  
  if (sc->short_desc)
    g_free(sc->short_desc);

  if (desc)
    sc->short_desc = g_strdup(desc);
  else 
    sc->short_desc = NULL;
}

void          
gconf_schema_set_long_desc(GConfSchema* sc, const gchar* desc)
{
  g_return_if_fail (desc == NULL || g_utf8_validate (desc, -1, NULL));
  
  if (sc->long_desc)
    g_free(sc->long_desc);

  if (desc)
    sc->long_desc = g_strdup(desc);
  else 
    sc->long_desc = NULL;
}

void          
gconf_schema_set_owner(GConfSchema* sc, const gchar* owner)
{
  g_return_if_fail (owner == NULL || g_utf8_validate (owner, -1, NULL));
  
  if (sc->owner)
    g_free(sc->owner);

  if (owner)
    sc->owner = g_strdup(owner);
  else
    sc->owner = NULL;
}

void
gconf_schema_set_default_value(GConfSchema* sc, GConfValue* val)
{
  if (sc->default_value != NULL)
    gconf_value_free(sc->default_value);

  sc->default_value = gconf_value_copy(val);
}

void
gconf_schema_set_default_value_nocopy(GConfSchema* sc, GConfValue* val)
{
  if (sc->default_value != NULL)
    gconf_value_free(sc->default_value);

  sc->default_value = val;
}

gboolean
gconf_schema_validate (GConfSchema *sc,
                       GError     **err)
{
  if (sc->locale && !g_utf8_validate (sc->locale, -1, NULL))
    {
      g_set_error (err, GCONF_ERROR,
                   GCONF_ERROR_FAILED,
                   _("Schema contains invalid UTF-8"));
      return FALSE;
    }

  if (sc->short_desc && !g_utf8_validate (sc->short_desc, -1, NULL))
    {
      g_set_error (err, GCONF_ERROR,
                   GCONF_ERROR_FAILED,
                   _("Schema contains invalid UTF-8"));
      return FALSE;
    }

  if (sc->long_desc && !g_utf8_validate (sc->long_desc, -1, NULL))
    {
      g_set_error (err, GCONF_ERROR,
                   GCONF_ERROR_FAILED,
                   _("Schema contains invalid UTF-8"));
      return FALSE;
    }

  if (sc->owner && !g_utf8_validate (sc->owner, -1, NULL))
    {
      g_set_error (err, GCONF_ERROR,
                   GCONF_ERROR_FAILED,
                   _("Schema contains invalid UTF-8"));
      return FALSE;
    }

  return TRUE;
}
