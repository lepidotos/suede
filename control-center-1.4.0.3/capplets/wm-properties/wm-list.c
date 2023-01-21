/* -*- Mode: C; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 8 -*- */
/* Copyright (C) 1998 Redhat Software Inc.
 * Code available under the Gnu GPL.
 * Authors: Owen Taylor <otaylor@redhat.com>
 */

#include <sys/types.h>
#include <dirent.h>
#include <unistd.h>
#include <ctype.h>
#include <string.h>
#include <libgnome/libgnome.h>
#include "wm-properties.h"

/* Current list of window managers */
GList *window_managers = NULL;

/* List on startup */
static GList *window_managers_save = NULL;

/* Current window manager */
static WindowManager *current_wm = NULL;

/* Window manager on startup */
static WindowManager *current_wm_save = NULL;

gboolean
is_blank (gchar *str)
{
        while (*str) {
                if (!isspace(*str))
                        return FALSE;
                str++;
        }
        return TRUE;
}

static gint
wm_compare (gconstpointer a, gconstpointer b)
{
        const WindowManager *wm_a = (const WindowManager *)a;
        const WindowManager *wm_b = (const WindowManager *)b;

        return g_strcasecmp (wm_a->dentry->name, wm_b->dentry->name);
}

static void
wm_free (WindowManager *wm)
{
        gnome_desktop_entry_free (wm->dentry);
        g_free (wm->config_exec);
        g_free (wm->config_tryexec);;
        g_free (wm);
}

void
wm_check_present (WindowManager *wm)
{
        gchar *path;

        if (wm->dentry->exec) {
                if (wm->dentry->tryexec) {
                        path = gnome_is_program_in_path (wm->dentry->tryexec);
                        wm->is_present = (path != NULL);
                        if (path)
                                g_free (path);
                } else
                        wm->is_present = TRUE;
        } else
                wm->is_present = FALSE;
        
        if (wm->config_exec) {
                if (wm->config_tryexec) {
                        path = gnome_is_program_in_path (wm->config_tryexec);
                        wm->is_config_present = (path != NULL);
                        if (path)
                                g_free (path);
                } else
                        wm->is_config_present = TRUE;
        } else
                wm->is_config_present = FALSE;
        
}

static WindowManager *
wm_copy (WindowManager *wm)
{
        WindowManager *result = g_new (WindowManager, 1);

        result->dentry = gnome_desktop_entry_copy (wm->dentry);
        result->config_exec = g_strdup (wm->config_exec);
        result->config_tryexec = g_strdup (wm->config_tryexec);

        result->session_managed = wm->session_managed;
        result->is_user = wm->is_user;
        result->is_present = wm->is_present;
        result->is_config_present = wm->is_config_present;

        return result;
}


static WindowManager *
wm_list_find (GList *list, gchar *name)
{
        GList *tmp_list = list;
        while (tmp_list) {
                WindowManager *wm = tmp_list->data;
                if (strcmp (wm->dentry->name, name) == 0)
                        return wm;

                tmp_list = tmp_list->next;
        }
        
        return NULL;
}

static WindowManager *
wm_list_find_exec (GList *list, gchar *name)
{
        GList *tmp_list = list;
        while (tmp_list) {
                WindowManager *wm = tmp_list->data;
                if (!wm->dentry->exec || !wm->dentry->exec[0])
                        continue;
                if (strcmp (wm->dentry->exec[0], name) == 0)
                        return wm;

                tmp_list = tmp_list->next;
        }
        
        return NULL;
}

static GList *
wm_list_find_files (gchar *directory)
{
        DIR *dir;
        struct dirent *child;
        GList *result = NULL;
        gchar *suffix;

        dir = opendir (directory);
        if (dir == NULL)
                return NULL;

        while ((child = readdir (dir)) != NULL) {
                /* Ignore files without .desktop suffix, and ignore
                 * .desktop files with no prefix
                 */
                suffix = child->d_name + strlen (child->d_name) - 8;
                /* strlen(".desktop") == 8 */

                if (suffix <= child->d_name || 
                    strcmp (suffix, ".desktop") != 0)
                        continue;
                
                result = g_list_prepend (result, 
                                         g_concat_dir_and_file (directory,
                                                                child->d_name));
        }
        closedir (dir);

        return result;
}

static void
wm_list_read_dir (gchar *directory, gboolean is_user)
{
        WindowManager *wm;
        GList *tmp_list;
        GList *files;
        gchar *prefix;

        files = wm_list_find_files (directory);

        tmp_list = files;
        while (tmp_list) {
                wm = g_new (WindowManager, 1);

                wm->dentry = gnome_desktop_entry_load_unconditional (tmp_list->data);
                if (!wm->dentry) {
                        g_free (wm);
                        tmp_list = tmp_list->next;
                        continue;
                }

                prefix = g_strconcat ("=", wm->dentry->location, "=/Window Manager/", NULL);
                gnome_config_push_prefix (prefix);
                g_free (prefix);

                wm->config_exec = gnome_config_get_string ("ConfigExec");
                wm->config_tryexec = gnome_config_get_string ("ConfigTryExec");
                wm->session_managed = gnome_config_get_bool ("SessionManaged=0");
                wm->is_user = is_user;

                if (wm->config_exec && is_blank (wm->config_exec)) {
                        g_free (wm->config_exec);
                        wm->config_exec = NULL;
                }

                gnome_config_pop_prefix ();

                wm_check_present (wm);

                if (wm->dentry->name && wm->dentry->exec &&
                    (wm->is_user || wm->is_present)) {
                        window_managers = 
                                g_list_insert_sorted (window_managers, 
                                                      wm,
                                                      wm_compare);
                        window_managers_save = 
                                g_list_insert_sorted (window_managers_save, 
                                                      wm_copy (wm),
                                                      wm_compare);
                } else {
                        wm_free (wm);
                }


                tmp_list = tmp_list->next;
        }
        g_list_free (files);
}

void           
wm_list_init (void)
{
        gchar *tempdir;
        gchar *name;
        
        tempdir = gnome_unconditional_datadir_file ("gnome/wm-properties/");
        wm_list_read_dir (tempdir, FALSE);
        g_free (tempdir);
        
	tempdir = gnome_util_home_file("wm-properties/");
        wm_list_read_dir (tempdir, TRUE);
        g_free (tempdir);

        name = gnome_config_get_string ("wm-properties/Config/Config/Current");
        if (name) {
                current_wm = wm_list_find (window_managers, name);
                g_free (name);
        }

        if (!current_wm) {
                name = gnome_config_get_string ("default.wm/Default/WM");

                if (name) {
                        current_wm = wm_list_find_exec (window_managers, name);
                        g_free (name);
                }
        }

        if (!current_wm) {
                gchar *wmfile, *prefix;

                wmfile = gnome_unconditional_datadir_file ("default.wm");
                prefix = g_strconcat ("=", wmfile, "=/Default/WM", NULL);
                name = gnome_config_get_string (prefix);

                g_free (wmfile);
                g_free (prefix);

                if (name) {
                        current_wm = wm_list_find_exec (window_managers, name);
                        g_free (name);
                }
        }

        if (!current_wm && window_managers) 
                current_wm = window_managers->data;

        if(current_wm)
                current_wm_save = wm_list_find (window_managers_save, current_wm->dentry->name);
}

void
wm_list_save (void)
{
        GList *old_files;
        GList *tmp_list;
        gchar *tempdir;
        gchar *prefix;
        WindowManager *wm;

        /* Clean out the current contents of .gnome/wm-desktops
         */

        tempdir = gnome_util_home_file("wm-properties/");
        old_files = wm_list_find_files (tempdir);
        g_free (tempdir);

        tmp_list = old_files;
        while (tmp_list) {
                prefix = g_strconcat ("=", tmp_list->data, "=", NULL);
                gnome_config_clean_file (prefix);
                gnome_config_sync_file (prefix);
                g_free (prefix);

                tmp_list = tmp_list->next;
        }
        g_list_free (old_files);
        

        /* Save the user's desktops
         */

        tmp_list = window_managers;
        while (tmp_list) {
                wm = tmp_list->data;

                if (wm->is_user) {
                        gnome_desktop_entry_save (wm->dentry);
                        
                        prefix = g_strconcat ("=", wm->dentry->location, "=/Window Manager/", NULL);
                        gnome_config_push_prefix (prefix);
                        g_free (prefix);

                        if (wm->config_exec)
                                gnome_config_set_string ("ConfigExec", wm->config_exec);
                        if (wm->config_tryexec)
                                gnome_config_set_string ("ConfigTryExec", wm->config_tryexec);
                        gnome_config_set_bool ("SessionManaged=0", wm->session_managed);
                        gnome_config_pop_prefix ();
                        
                }
                tmp_list = tmp_list->next;
        }

        /* Save the current window manager
         */
        if(current_wm)
                gnome_config_set_string ("wm-properties/Config/Config/Current",
                                         current_wm->dentry->name);
        gnome_config_sync ();
}

void
wm_list_revert (void)
{
        GList *tmp_list;
        gchar *old_name = NULL;

        if(current_wm)
                old_name = g_strdup (current_wm->dentry->name);
        
        g_list_foreach (window_managers, (GFunc)wm_free, NULL);
        g_list_free (window_managers);
        window_managers = NULL;

        tmp_list = window_managers_save;
        while (tmp_list) {
                window_managers = g_list_prepend (window_managers,
                                                  wm_copy (tmp_list->data));
                tmp_list = tmp_list->next;
        }
        window_managers = g_list_reverse (window_managers);
        current_wm = wm_list_find (window_managers, old_name);
        g_free (old_name);
}

void
wm_list_add (WindowManager *wm)
{
        g_return_if_fail (wm != NULL);
        
        window_managers = g_list_insert_sorted (window_managers, wm,
                                                wm_compare);
}

void
wm_list_delete (WindowManager *wm)
{
        GList *node;

        g_return_if_fail (wm != NULL);
        g_return_if_fail (wm != current_wm);

        node = g_list_find (window_managers, wm);
        g_return_if_fail (node != NULL);
        
        window_managers = g_list_remove_link (window_managers, node);
        g_list_free_1 (node);
        wm_free (wm);
}

void
wm_list_set_current (WindowManager *wm)
{
        current_wm = wm;
}

WindowManager *
wm_list_get_current (void)
{
        return current_wm;
}

WindowManager *
wm_list_get_revert (void)
{
        if(current_wm_save)
                return wm_list_find (window_managers, current_wm_save->dentry->name);
        else
                return NULL;
}

