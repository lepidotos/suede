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

#include "xml-dir.h"
#include "xml-entry.h"

#include <libxml/parser.h>

#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <dirent.h>
#include <limits.h>

#include <gconf/gconf-internals.h>
#include "xml-entry.h"

/* This makes hash table safer when debugging */
#ifndef GCONF_ENABLE_DEBUG
#define safe_g_hash_table_insert g_hash_table_insert
#else
static void
safe_g_hash_table_insert(GHashTable* ht, gpointer key, gpointer value)
{
  gpointer oldkey = NULL, oldval = NULL;

  if (g_hash_table_lookup_extended(ht, key, &oldkey, &oldval))
    {
      gconf_log(GCL_WARNING, "Hash key `%s' is already in the table!",
                (gchar*)key);
      return;
    }
  else
    {
      g_hash_table_insert(ht, key, value);
    }
}
#endif

static gchar* parent_dir(const gchar* dir);

struct _Dir {
  gchar* key;
  gchar* fs_dirname;
  gchar* xml_filename;
  guint root_dir_len;
  GTime last_access; /* so we know when to un-cache */
  xmlDocPtr doc;
  GHashTable* entry_cache; /* store key-value entries */
  GHashTable* subdir_cache; /* store subdirectories */
  guint dir_mode;
  guint file_mode;
  guint dirty : 1;
  guint deleted : 1;
};

static void
dir_load_doc(Dir* d, GError** err);

static Entry* dir_make_new_entry(Dir* d, const gchar* relative_key);

static gboolean dir_forget_entry_if_useless(Dir* d, Entry* e);

static Dir*
dir_blank(const gchar* key)
{
  Dir* d;
  
  d = g_new0(Dir, 1);

#ifdef GCONF_ENABLE_DEBUG
  {
    gchar* why;
    if (!gconf_valid_key(key, &why)) {
      gconf_log(GCL_DEBUG, "key `%s' invalid: %s",
                key, why);
    }
    g_assert(gconf_valid_key(key, NULL));
  }
#endif
  
  d->key = g_strdup(key);
  
  d->last_access = time(NULL);
  d->doc = NULL;

  d->entry_cache = g_hash_table_new(g_str_hash, g_str_equal);
  
  d->dirty = FALSE;
  d->deleted = FALSE;

  d->dir_mode = 0700;
  d->file_mode = 0600;
  
  return d;
}

Dir*
dir_new(const gchar  *keyname,
        const gchar  *xml_root_dir,
        guint dir_mode,
        guint file_mode)
{
  Dir* d;
  
  d = dir_blank(keyname);

  /* sync with dir_load() */
  d->fs_dirname = gconf_concat_dir_and_key(xml_root_dir, keyname);
  d->xml_filename =  g_strconcat(d->fs_dirname, "/%gconf.xml", NULL);
  d->root_dir_len = strlen(xml_root_dir);

  d->dir_mode = dir_mode;
  d->file_mode = file_mode;
  
  return d;
}

Dir*
dir_load        (const gchar* key, const gchar* xml_root_dir, GError** err)
{
  Dir* d;
  gchar* fs_dirname;
  gchar* xml_filename;
  guint dir_mode = 0700;
  guint file_mode = 0600;
  
  g_return_val_if_fail(gconf_valid_key(key, NULL), NULL);
  
  fs_dirname = gconf_concat_dir_and_key(xml_root_dir, key);
  xml_filename = g_strconcat(fs_dirname, "/%gconf.xml", NULL);

  {
    struct stat s;
    gboolean notfound = FALSE;
    
    if (stat(xml_filename, &s) != 0)
      {
        if (errno != ENOENT)
          {
            gconf_set_error(err, GCONF_ERROR_FAILED,
                            _("Could not stat `%s': %s"),
                            xml_filename, strerror(errno));

          }
        
        notfound = TRUE;
      }
    else if (S_ISDIR(s.st_mode))
      {
        gconf_set_error(err, GCONF_ERROR_FAILED,
                         _("XML filename `%s' is a directory"),
                         xml_filename);
        notfound = TRUE;
      }

    if (notfound)
      {
        gconf_log(GCL_DEBUG, "dir file %s not found", xml_filename);
        g_free(fs_dirname);
        g_free(xml_filename);
        return NULL;
      }
    else
      {
        /* Take directory mode from the xml_root_dir, if possible */
        if (stat (xml_root_dir, &s) == 0)
          {
            dir_mode = mode_t_to_mode(s.st_mode);
          }
        
        file_mode = dir_mode & ~0111; /* turn off search bits */
      }
  }

  d = dir_blank(key);

  /* sync with dir_new() */
  d->fs_dirname = fs_dirname;
  d->xml_filename = xml_filename;
  d->root_dir_len = strlen(xml_root_dir);

  d->dir_mode = dir_mode;
  d->file_mode = file_mode;
  
  gconf_log(GCL_DEBUG, "loaded dir %s", fs_dirname);
  
  return d;
}


static void
entry_destroy_foreach(const gchar* name, Entry* e, gpointer data)
{
  entry_destroy(e);
}

void
dir_destroy(Dir* d)
{
  g_free(d->key);
  g_free(d->fs_dirname);
  g_free(d->xml_filename);
  
  g_hash_table_foreach(d->entry_cache, (GHFunc)entry_destroy_foreach,
                       NULL);
  
  g_hash_table_destroy(d->entry_cache);

  if (d->doc != NULL)
    xmlFreeDoc(d->doc);
  
  g_free(d);
}

static gboolean
create_fs_dir(const gchar* dir, const gchar* xml_filename,
              guint root_dir_len,
              guint dir_mode, guint file_mode,
              GError** err);

gboolean
dir_ensure_exists (Dir* d,
                   GError** err)
{
  if (!create_fs_dir(d->fs_dirname, d->xml_filename, d->root_dir_len,
                     d->dir_mode, d->file_mode,
                     err))
    {

      /* check that error is set */
      g_return_val_if_fail( (err == NULL) || (*err != NULL), FALSE );
      
      return FALSE;
    }
  else
    {
      return TRUE;
    }
}

static void
entry_sync_foreach(const gchar* name, Entry* e, gpointer data)
{
  entry_sync_to_node(e);
}

gboolean
dir_sync_pending    (Dir          *d)
{
  return d->dirty;
}

gboolean
dir_sync        (Dir* d, GError** err)
{
  gboolean retval = TRUE;
  
  /* note that if we are deleted but already
     synced, this returns now, making the
     dircache's recursive delete tactic reasonably
     efficient
  */
  if (!d->dirty)
    return TRUE; 

  /* We should have a doc if dirty is TRUE */
  g_assert(d->doc != NULL);

  d->last_access = time(NULL);
  
  if (d->deleted)
    {
      if (unlink(d->xml_filename) != 0)
        {
          gconf_set_error(err, GCONF_ERROR_FAILED, _("Failed to delete `%s': %s"),
                          d->xml_filename, strerror(errno));
          return FALSE;
        }

      if (rmdir(d->fs_dirname) != 0)
        {
          gconf_set_error(err, GCONF_ERROR_FAILED, _("Failed to delete `%s': %s"),
                          d->fs_dirname, strerror(errno));
          return FALSE;
        }
    }
  else
    {
      gboolean old_existed = FALSE;
      gchar* tmp_filename;
      gchar* old_filename;
      
      /* First make sure entry values are synced to their
         XML nodes */
      g_hash_table_foreach(d->entry_cache, (GHFunc)entry_sync_foreach, NULL);
      
      tmp_filename = g_strconcat(d->fs_dirname, "/%gconf.xml.tmp", NULL);
      old_filename = g_strconcat(d->fs_dirname, "/%gconf.xml.old", NULL);

      if (xmlSaveFile(tmp_filename, d->doc) < 0)
        {
          gboolean recovered = FALSE;
          
          /* Try to solve the problem by creating the FS dir */
          if (!gconf_file_exists(d->fs_dirname))
            {
              if (create_fs_dir(d->fs_dirname, d->xml_filename,
                                d->root_dir_len,
                                d->dir_mode, d->file_mode,
                                err))
                {
                  if (xmlSaveFile(tmp_filename, d->doc) >= 0)
                    recovered = TRUE;
                }
            }

          if (!recovered)
            {
              /* I think libxml may mangle errno, but we might as well 
                 try. Don't set error if it's already set by some
                 earlier failure. */
              if (err && *err == NULL)
                gconf_set_error(err, GCONF_ERROR_FAILED, _("Failed to write file `%s': %s"), 
                                tmp_filename, strerror(errno));
              
              retval = FALSE;
              
              goto failed_end_of_sync;
            }
        }

      /* Set permissions on the new file */
      if (chmod (tmp_filename, d->file_mode) != 0)
        {
          gconf_set_error(err, GCONF_ERROR_FAILED, 
                          _("Failed to set mode on `%s': %s"),
                          tmp_filename, strerror(errno));
          
          retval = FALSE;
          goto failed_end_of_sync;
        }
      
      old_existed = gconf_file_exists(d->xml_filename);

      if (old_existed)
        {
          if (rename(d->xml_filename, old_filename) < 0)
            {
              gconf_set_error(err, GCONF_ERROR_FAILED, 
                              _("Failed to rename `%s' to `%s': %s"),
                              d->xml_filename, old_filename, strerror(errno));

              retval = FALSE;
              goto failed_end_of_sync;
            }
        }

      if (rename(tmp_filename, d->xml_filename) < 0)
        {
          gconf_set_error(err, GCONF_ERROR_FAILED, _("Failed to rename `%s' to `%s': %s"),
                          tmp_filename, d->xml_filename, strerror(errno));

          /* Put the original file back, so this isn't a total disaster. */
          if (rename(old_filename, d->xml_filename) < 0)
            {
              gconf_set_error(err, GCONF_ERROR_FAILED, _("Failed to restore `%s' from `%s': %s"),
                              d->xml_filename, old_filename, strerror(errno));
            }

          retval = FALSE;
          goto failed_end_of_sync;
        }

      if (old_existed)
        {
          if (unlink(old_filename) < 0)
            {
              gconf_log(GCL_WARNING, _("Failed to delete old file `%s': %s"),
                         old_filename, strerror(errno));
              /* Not a failure, just leaves cruft around. */
            }
        }

    failed_end_of_sync:
      
      g_free(old_filename);
      g_free(tmp_filename);
    }

  if (retval)
    d->dirty = FALSE;

  return retval;
}

void
dir_set_value   (Dir* d, const gchar* relative_key,
                 GConfValue* value, GError** err)
{
  Entry* e;
  
  if (d->doc == NULL)
    dir_load_doc(d, err);

  if (d->doc == NULL)
    {
      g_return_if_fail( (err == NULL) || (*err != NULL) );
      return;
    }
  
  e = g_hash_table_lookup(d->entry_cache, relative_key);
  
  if (e == NULL)
    e = dir_make_new_entry(d, relative_key);

  entry_set_value(e, value);

  d->last_access = time(NULL);
  entry_set_mod_time(e, d->last_access);

  entry_set_mod_user(e, g_get_user_name());
  
  d->dirty = TRUE;
}

GTime
dir_get_last_access (Dir          *d)
{
  return d->last_access;
}

GConfValue*
dir_get_value   (Dir* d,
                 const gchar* relative_key,
                 const gchar** locales,
                 gchar** schema_name,
                 GError** err)
{
  Entry* e;

  if (d->doc == NULL)
    dir_load_doc(d, err);

  if (d->doc == NULL)
    {
      g_return_val_if_fail( (err == NULL) || (*err != NULL), NULL );
      return NULL;
    }
  
  e = g_hash_table_lookup(d->entry_cache, relative_key);

  d->last_access = time(NULL);

  if (e == NULL)
    {
      /* No entry; return */
      return NULL;
    }
  else
    {
      GConfValue* val;

      g_assert(e != NULL);

      val = entry_get_value (e, locales, err);

      /* Fill schema name if value is NULL because we might be
         interested in the default value of the key in that case. */
      if (schema_name &&
          val == NULL &&
          entry_get_schema_name(e))
        *schema_name = g_strdup(entry_get_schema_name(e));
      
      /* return copy of the value */
      if (val != NULL)
        return gconf_value_copy(val);
      else
        return NULL;
    }
}

const gchar*
dir_get_name        (Dir          *d)
{
  g_return_val_if_fail(d != NULL, NULL);
  return d->key;
}

GConfMetaInfo*
dir_get_metainfo(Dir* d, const gchar* relative_key, GError** err)
{
  Entry* e;
  
  d->last_access = time(NULL);
  
  if (d->doc == NULL)
    dir_load_doc(d, err);

  if (d->doc == NULL)
    {
      g_return_val_if_fail( (err == NULL) || (*err != NULL), NULL );
      return NULL;
    }
  
  e = g_hash_table_lookup(d->entry_cache, relative_key);

  if (e == NULL)
    return NULL;
  else
    return entry_get_metainfo(e);
}

void
dir_unset_value (Dir* d, const gchar* relative_key,
                 const gchar* locale, GError** err)
{
  Entry* e;
  
  d->last_access = time(NULL);
  
  if (d->doc == NULL)
    dir_load_doc(d, err);

  if (d->doc == NULL)
    {
      g_return_if_fail( (err == NULL) || (*err != NULL) );
      return;
    }
  
  e = g_hash_table_lookup(d->entry_cache, relative_key);
  
  if (e == NULL)     /* nothing to change */
    return;

  if (entry_unset_value(e, locale))
    {
      /* If entry_unset() returns TRUE then
         the entry was changed (not already unset) */
      
      d->dirty = TRUE;
      
      if (dir_forget_entry_if_useless(d, e))
        {
          /* entry is destroyed */
          return;
        }
      else
        {
          entry_set_mod_time(e, d->last_access);
          entry_set_mod_user(e, g_get_user_name());
        }
    }
  else
    {
      /* Check uselessness anyway; this ensures that if it was useless
         when the daemon started or we otherwise missed its lack of
         utility, we clean it up if the user does an explicit unset */
      dir_forget_entry_if_useless(d, e);
    }
}

typedef struct _ListifyData ListifyData;

struct _ListifyData {
  GSList* list;
  const gchar* name;
  const gchar** locales;
};

static void
listify_foreach(const gchar* key, Entry* e, ListifyData* ld)
{
  GConfValue* val;
  GConfEntry* entry;
  GError* error = NULL;
  
  val = entry_get_value (e, ld->locales, &error);

  if (error != NULL)
    {
      g_assert (val == NULL);
      g_error_free (error);
      return;
    }
  
  entry = gconf_entry_new_nocopy (g_strdup(key),
                                  val ? gconf_value_copy(val) : NULL);
  
  if (val == NULL &&
      entry_get_schema_name (e))
    {
      gconf_entry_set_schema_name (entry, entry_get_schema_name (e));
    }
  
  ld->list = g_slist_prepend(ld->list, entry);
}

GSList*
dir_all_entries (Dir* d, const gchar** locales, GError** err)
{
  ListifyData ld;
  
  if (d->doc == NULL)
    dir_load_doc(d, err);

  if (d->doc == NULL)
    {
      g_return_val_if_fail( (err == NULL) || (*err != NULL), NULL );
      return NULL;
    }
  
  ld.list = NULL;
  ld.name = d->key;
  ld.locales = locales;

  g_hash_table_foreach(d->entry_cache, (GHFunc)listify_foreach,
                       &ld);
  
  return ld.list;
}

GSList*
dir_all_subdirs (Dir* d, GError** err)
{
  DIR* dp;
  struct dirent* dent;
  struct stat statbuf;
  GSList* retval = NULL;
  gchar* fullpath;
  gchar* fullpath_end;
  guint len;
  guint subdir_len;
  
  if (d->doc == NULL)
    dir_load_doc(d, err);
  
  if (d->doc == NULL)
    {
      g_return_val_if_fail( (err == NULL) || (*err != NULL), NULL );
      return NULL;
    }
  
  dp = opendir(d->fs_dirname);

  if (dp == NULL)
    return NULL;

  len = strlen(d->fs_dirname);
  subdir_len = PATH_MAX - len;
  
  fullpath = g_malloc0(subdir_len + len + 20); /* ensure null termination */
  strcpy(fullpath, d->fs_dirname);
  
  fullpath_end = fullpath + len;
  *fullpath_end = '/';
  ++fullpath_end;
  *fullpath_end = '\0';

  while ((dent = readdir(dp)) != NULL)
    {
      /* ignore ., .., and all dot-files */
      if (dent->d_name[0] == '.')
        continue;

      len = strlen(dent->d_name);

      if (len < subdir_len)
        {
          strcpy(fullpath_end, dent->d_name);
          strncpy(fullpath_end+len, "/%gconf.xml", subdir_len - len);
        }
      else
        continue; /* Shouldn't ever happen since PATH_MAX is available */
      
      if (stat(fullpath, &statbuf) < 0)
        {
          /* This is some kind of cruft, not an XML directory */
          continue;
        }
      
      retval = g_slist_prepend(retval, g_strdup(dent->d_name));
    }

  /* if this fails, we really can't do a thing about it
     and it's not a meaningful error */
  closedir(dp);

  g_free (fullpath);
  
  return retval;
}

void
dir_set_schema  (Dir* d,
                 const gchar* relative_key,
                 const gchar* schema_key,
                 GError** err)
{
  Entry* e;

  if (d->doc == NULL)
    dir_load_doc(d, err);

  if (d->doc == NULL)
    {
      g_return_if_fail( (err == NULL) || (*err != NULL) );
      return;
    }
  
  d->dirty = TRUE;
  d->last_access = time(NULL);
  
  e = g_hash_table_lookup(d->entry_cache, relative_key);

  if (e == NULL)
    e = dir_make_new_entry(d, relative_key);

  entry_set_mod_time(e, d->last_access);

  entry_set_schema_name(e, schema_key);

  if (schema_key == NULL)
    dir_forget_entry_if_useless(d, e);
}

void
dir_mark_deleted(Dir* d)
{
  if (d->deleted)
    return;
  
  d->deleted = TRUE;
  d->dirty = TRUE;
  
  /* go ahead and free the XML document */

  if (d->doc)
    xmlFreeDoc(d->doc);
  d->doc = NULL;
}

gboolean
dir_is_deleted     (Dir* d)
{
  return d->deleted;
}

GTime
dir_last_access (Dir* d)
{
  return d->last_access;
}

/* private Dir functions */

static void
dir_fill_cache_from_doc(Dir* d);

static void
dir_load_doc(Dir* d, GError** err)
{
  gboolean xml_already_exists = TRUE;
  gboolean need_backup = FALSE;
  struct stat statbuf;
  
  g_return_if_fail(d->doc == NULL);

  if (stat(d->xml_filename, &statbuf) < 0)
    {
      switch (errno)
        {
        case ENOENT:
          xml_already_exists = FALSE;
          break;
        case ENOTDIR:
        case ELOOP:
        case EFAULT:
        case EACCES:
        case ENOMEM:
        case ENAMETOOLONG:
        default:
          /* These are all fatal errors */
          gconf_set_error(err, GCONF_ERROR_FAILED, _("Failed to stat `%s': %s"),
                          d->xml_filename, strerror(errno));
          return;
          break;
        }
    }

  if (statbuf.st_size == 0)
    {
      xml_already_exists = FALSE;
    }

  if (xml_already_exists)
    d->doc = xmlParseFile(d->xml_filename);

  /* We recover from these errors instead of passing them up */

  /* This has the potential to just blow away an entire corrupted
     config file; but I think that is better than the alternatives
     (disabling config for a directory because the document is mangled)
  */  

  /* Also we create empty %gconf.xml files when we create a new dir,
     and those return a parse error */
  
  if (d->doc == NULL)
    {
      if (xml_already_exists)
        need_backup = TRUE; /* we want to save whatever broken stuff was in the file */
          
      /* Create a new doc */
      
      d->doc = xmlNewDoc("1.0");
    }
  
  if (d->doc->root == NULL)
    {
      /* fill it in */
      d->doc->root = xmlNewDocNode(d->doc, NULL, "gconf", NULL);
    }
  else if (strcmp(d->doc->root->name, "gconf") != 0)
    {
      xmlFreeDoc(d->doc);
      d->doc = xmlNewDoc("1.0");
      d->doc->root = xmlNewDocNode(d->doc, NULL, "gconf", NULL);
      need_backup = TRUE; /* save broken stuff */
    }
  else
    {
      /* We had an initial doc with a valid root */
      /* Fill child_cache from entries */
      dir_fill_cache_from_doc(d);
    }

  if (need_backup)
    {
      /* Back up the file we failed to parse, if it exists,
         we aren't going to be able to do anything if this call
         fails
      */
      
      gchar* backup = g_strconcat(d->xml_filename, ".bak", NULL);
      int fd;
      
      rename(d->xml_filename, backup);
      
      /* Recreate %gconf.xml to maintain our integrity and be sure
         all_subdirs works */
      /* If we failed to rename, we just give up and truncate the file */
      fd = open(d->xml_filename, O_CREAT | O_WRONLY | O_TRUNC, d->file_mode);
      if (fd >= 0)
        close(fd);
      
      g_free(backup);
    }
  
  g_assert(d->doc != NULL);
  g_assert(d->doc->root != NULL);
}

static Entry*
dir_make_new_entry(Dir* d, const gchar* relative_key)
{
  Entry* e;

  g_return_val_if_fail(d->doc != NULL, NULL);
  g_return_val_if_fail(d->doc->root != NULL, NULL);
  
  e = entry_new(relative_key);

  entry_set_node(e, xmlNewChild(d->doc->root, NULL, "entry", NULL));
  
  safe_g_hash_table_insert(d->entry_cache, (gchar*)entry_get_name(e), e);
  
  return e;
}

static gboolean
dir_forget_entry_if_useless(Dir* d, Entry* e)
{
  GConfValue* val;
  
  if (entry_get_schema_name(e) != NULL)
    return FALSE;
  
  val = entry_get_value(e, NULL, NULL);
  
  if (val != NULL)
    return FALSE; /* not useless */
      
  g_hash_table_remove(d->entry_cache, entry_get_name(e));

  entry_destroy(e);

  return TRUE;
}

static void
dir_fill_cache_from_doc(Dir* d)
{
  xmlNodePtr node;
  
  if (d->doc == NULL ||
      d->doc->root == NULL ||
      d->doc->root->childs == NULL)
    {
      /* Empty document - just return. */
      return;
    }

  node = d->doc->root->childs;

  while (node != NULL)
    {
      if (node->type == XML_ELEMENT_NODE && 
          (strcmp(node->name, "entry") == 0))
        {
          gchar* attr = my_xmlGetProp(node, "name");

          if (attr != NULL)
            {
              if (g_hash_table_lookup(d->entry_cache, attr) != NULL)
                {
                  gconf_log(GCL_WARNING,
                             _("Duplicate entry `%s' in `%s', ignoring"),
                             attr, d->xml_filename);
                }
              else
                {
                  Entry* e;
                  
                  e = entry_new(attr);

                  entry_set_node(e, node);
                  
                  entry_fill_from_node(e);
                  
                  safe_g_hash_table_insert(d->entry_cache,
                                           (gchar*)entry_get_name(e), e);
                }

              free(attr);
            }
          else
            {
              gconf_log(GCL_WARNING,
                         _("Entry with no name in XML file `%s', ignoring"),
                         d->xml_filename);
            }
        }
      else
        {
          if (node->type == XML_ELEMENT_NODE)
            gconf_log(GCL_WARNING,
                      _("A toplevel node in XML file `%s' is <%s> rather than <entry>, ignoring"),
                      d->xml_filename,
                      node->name ? (char*) node->name : "unknown");
        }
      
      node = node->next;
    }
}

/*
 * Misc
 */

static gboolean
create_fs_dir(const gchar* dir, const gchar* xml_filename,
              guint root_dir_len, guint dir_mode, guint file_mode,
              GError** err)
{
  g_return_val_if_fail(xml_filename != NULL, FALSE);
  
  gconf_log(GCL_DEBUG, "Enter create_fs_dir: %s", dir);
  
  if (gconf_file_test(xml_filename, GCONF_FILE_ISFILE))
    {
      gconf_log(GCL_DEBUG, "XML backend file %s already exists", xml_filename);
      return TRUE;
    }
      
  /* Don't create anything above the root directory */
  if (strlen(dir) > root_dir_len)
    {
      gchar* parent;
      
      parent = parent_dir(dir);

      gconf_log(GCL_DEBUG, "Parent dir is %s", parent);
      
      if (parent != NULL)
        {
          gchar* parent_xml = NULL;
          gboolean success = FALSE;
          
          if (xml_filename)
            parent_xml = g_strconcat(parent, "/%gconf.xml", NULL);
          
          success = create_fs_dir(parent, parent_xml, root_dir_len,
                                  dir_mode, file_mode, err);

          if (success)
            gconf_log(GCL_DEBUG, "created parent: %s", parent);
          else
            gconf_log(GCL_DEBUG, "failed parent: %s", parent);
          
          g_free(parent);
          if (parent_xml)
            g_free(parent_xml);
          
          if (!success)
            return FALSE;
        }
      else
        {
          gconf_log(GCL_DEBUG, "%s has no parent", dir);
        }
    }

  gconf_log(GCL_DEBUG, "Making directory %s", dir);
  
  if (mkdir(dir, dir_mode) < 0)
    {
      if (errno != EEXIST)
        {
          gconf_set_error(err, GCONF_ERROR_FAILED,
                          _("Could not make directory `%s': %s"),
                          (gchar*)dir, strerror(errno));
          return FALSE;
        }
    }

  if (xml_filename != NULL)
    {
      int fd;
      /* don't truncate the file, it may well already exist */
      fd = open(xml_filename, O_CREAT | O_WRONLY, file_mode);

      gconf_log(GCL_DEBUG, "Creating XML file %s", xml_filename);
      
      if (fd < 0)
        {
          gconf_set_error(err, GCONF_ERROR_FAILED, _("Failed to create file `%s': %s"),
                          xml_filename, strerror(errno));
          
          return FALSE;
        }
      
      if (close(fd) < 0)
        {
          gconf_set_error(err, GCONF_ERROR_FAILED, _("Failed to close file `%s': %s"),
                          xml_filename, strerror(errno));
          
          return FALSE;
        }
    }
  else
    {
      gconf_log(GCL_DEBUG, "No XML filename passed to create_fs_dir() for %s", dir);
    }
  
  return TRUE;
}

static gchar* 
parent_dir(const gchar* dir)
{
  /* We assume the dir doesn't have a trailing slash, since that's our
     standard canonicalization in GConf */
  gchar* parent;
  gchar* last_slash;

  g_return_val_if_fail(*dir != '\0', NULL);

  if (dir[1] == '\0')
    {
      g_assert(dir[0] == '/');
      return NULL;
    }

  parent = g_strdup(dir);

  last_slash = strrchr(parent, '/');

  /* dir must have had at least the root slash in it */
  g_assert(last_slash != NULL);
  
  if (last_slash != parent)
    *last_slash = '\0';
  else 
    {
      ++last_slash;
      *last_slash = '\0';
    }

  return parent;
}


/* util */
guint
mode_t_to_mode(mode_t orig)
{
  /* I don't think this is portable. */
  guint mode = 0;
  guint fullmask = S_IRWXG | S_IRWXU | S_IRWXO;
  

  mode = orig & fullmask;
  
  g_return_val_if_fail(mode <= 0777, 0700);

  return mode;
}
