/* -*- Mode: C; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 8 -*- */
/* Copyright (C) 1998 Redhat Software Inc.
 * Code available under the Gnu GPL.
 * Authors: Owen Taylor <otaylor@redhat.com>
 */

#include <gtk/gtk.h>
#include <gdk/gdkx.h>
#include <libgnome/libgnome.h>
#include "wm-properties.h"

typedef struct _RestartInfo RestartInfo;

struct _RestartInfo {
        GnomeDesktopEntry *dentry;
        gint retries;
        WMResultFunc callback;
        gpointer data;
};

gboolean
wm_is_running (void)
{
        gboolean result;
        gboolean old_warnings = gdk_error_warnings;
        XWindowAttributes attrs;
        
        gdk_error_warnings = FALSE;
        gdk_error_code = 0;

        XGetWindowAttributes (GDK_DISPLAY(), GDK_ROOT_WINDOW(), &attrs);
        
        XSelectInput (GDK_DISPLAY(), GDK_ROOT_WINDOW(),
                      SubstructureRedirectMask);
        XSync (GDK_DISPLAY(), False);
        if (gdk_error_code == 0) {
                result = FALSE;
                XSelectInput (GDK_DISPLAY(), GDK_ROOT_WINDOW(), 
                              attrs.your_event_mask);
        } else
                result = TRUE;

        gdk_error_warnings = old_warnings;
        return result;
}

/* Cut and paste from gnome-libs/gnome_win_hints_wm_exists, except that we
 * return the xid instead of a window
 */
static Window
find_gnome_wm_window(void)
{
  Atom r_type;
  int r_format;
  unsigned long count;
  unsigned long bytes_remain;
  unsigned char *prop, *prop2;
  gint prev_error;
  GdkAtom cardinal_atom = gdk_atom_intern ("CARDINAL", FALSE);
  
  prev_error = gdk_error_warnings;
  gdk_error_warnings = 0;  
  if (XGetWindowProperty(GDK_DISPLAY(), GDK_ROOT_WINDOW(),
                         gdk_atom_intern ("_WIN_SUPPORTING_WM_CHECK", FALSE),
                         0, 1, False, cardinal_atom,
			 &r_type, &r_format,
			 &count, &bytes_remain, &prop) == Success && prop)
    {
      if (r_type == cardinal_atom && r_format == 32 && count == 1)
        {
	  Window n = *(long *)prop;
	  if (XGetWindowProperty(GDK_DISPLAY(), n,
                                 gdk_atom_intern ("_WIN_SUPPORTING_WM_CHECK", FALSE),
                                 0, 1, False, cardinal_atom,
				 &r_type, &r_format, &count, &bytes_remain, 
				 &prop2) == Success && prop)
	    {
	      if (r_type == cardinal_atom && r_format == 32 && count == 1)
		{
		  XFree(prop);
		  XFree(prop2);
		  gdk_error_warnings = prev_error;
		  return n;
		}
	      XFree(prop2);
	    }       
        }
      XFree(prop);
    }       
  gdk_error_warnings = prev_error;
  return None;
}

static Window 
find_wm_window_from_client (GdkWindow *client)
{
        Window window, frame, parent, root;
        Window *children;
        unsigned int nchildren;
        gboolean old_warnings;

        if (!client)
                return None;
        
        frame = None;
        window = GDK_WINDOW_XWINDOW (client);
        
        old_warnings = gdk_error_warnings;
        gdk_error_warnings = FALSE;
        gdk_error_code = 0;

        while (XQueryTree (GDK_DISPLAY(), window,
                           &root, &parent, &children, &nchildren) &&
               (gdk_error_code == 0)) {
                
                if (children)
                        XFree(children);

                if (window == root)
                        break;

                if (root == parent) {
                        frame = window;
                        break;
                }
                window = parent;
        }
        
        gdk_error_warnings = old_warnings;

        return frame;
}

static gboolean
window_has_wm_state (Window window)
{
  Atom r_type;
  int r_format;
  unsigned long count;
  unsigned long bytes_remain;
  unsigned char *prop;
  
  if (XGetWindowProperty(GDK_DISPLAY(), window,
                         gdk_atom_intern ("WM_STATE", FALSE),
                         0, 0, False, AnyPropertyType,
			 &r_type, &r_format,
			 &count, &bytes_remain, &prop) == Success) {
          
          if (r_type != None) {
                  XFree(prop);
                  return TRUE;
          }
  }       
  return FALSE;
}

static gboolean
descendent_has_wm_state (Window window)
{
        gboolean result = FALSE;
        Window parent, root;
        Window *children;
        unsigned int nchildren;
        gint i;

        if (!XQueryTree (GDK_DISPLAY(), window,
                         &root, &parent, &children, &nchildren))
                return FALSE;
                
        for (i=0; i<nchildren; i++) {
                if (window_has_wm_state (children[i]) ||
                    descendent_has_wm_state (children[i])) {
                        result = TRUE;
                        break;
                }
        }

        if (children)
                XFree (children);
        
        return result;
}

/* This function tries to find a window manager frame by
 * hunting all the children of the root window
 */
static Window
find_wm_window_from_hunt (void)
{
        Window parent, root, frame;
        Window *children;
        unsigned int nchildren;
        gboolean old_warnings;
        gint i;

        frame = None;
        
        old_warnings = gdk_error_warnings;
        gdk_error_warnings = FALSE;
        gdk_error_code = 0;

        XQueryTree (GDK_DISPLAY(), gdk_root_window,
                    &root, &parent, &children, &nchildren);

        /* We are looking for a window that doesn't have WIN_STATE
         * set on it, but does have a child with WIN_STATE set
         */
        for (i=0; i<nchildren; i++) {
                if (!window_has_wm_state (children[i]) &&
                    descendent_has_wm_state (children[i])) {
                        frame = children[i];
                        break;
                }
        }

        if (children)
                XFree (children);
        
        gdk_error_warnings = old_warnings;

        return frame;
}

static Window
find_wm_window (GdkWindow *client)
{
        Window wm_window = None;
        
        /* First, try to find a GNOME compliant WM */

        wm_window = find_gnome_wm_window();
        
        if (!wm_window) {
                wm_window = find_wm_window_from_client (client);
        }

        if (!wm_window) {
                wm_window = find_wm_window_from_hunt ();
        }

        return wm_window;
}

static gboolean
start_timeout (gpointer data)
{
        RestartInfo *info = data;
        if (wm_is_running ()) {
                info->callback(WM_SUCCESS, info->data);
                gnome_desktop_entry_free (info->dentry);
                g_free (info);
                return FALSE;
        } else {
                info->retries--;
                if (info->retries > 0)
                        return TRUE;
                else {                         
                        info->callback(WM_CANT_START, info->data);
                        gnome_desktop_entry_free (info->dentry);
                        g_free (info);
                        return FALSE;
                }
        }
}

static void
start_do (RestartInfo *info)
{
        gnome_desktop_entry_launch (info->dentry);

        info->retries = 10;
        gtk_timeout_add (1000, start_timeout, info);
}

static gboolean
kill_timeout (gpointer data)
{
        RestartInfo *info = data;
        if (!wm_is_running ()) {
                start_do (info);
                return FALSE;
        } else {
                info->retries--;
                if (info->retries > 0)
                        return TRUE;
                else {                         
                        info->callback(WM_ALREADY_RUNNING, info->data);
                        gnome_desktop_entry_free (info->dentry);
                        g_free (info);
                        return FALSE;
                }
        }
}

void           
wm_restart (WindowManager *new,
            GdkWindow     *client,
            WMResultFunc   callback,
            gpointer       data)
{
        Window wm_window;
        RestartInfo *info;

        g_return_if_fail (new->is_present);
        
        info = g_new (RestartInfo, 1);
        info->dentry = gnome_desktop_entry_copy (new->dentry);
        info->callback = callback;
        info->data = data;
        info->retries = 10;

        if (wm_is_running ()) {
                wm_window = find_wm_window (client);
                if (!wm_window) {
                        (*callback) (WM_ALREADY_RUNNING, data);
                        gnome_desktop_entry_free (info->dentry);
                        g_free (info);
                } else {
                        XKillClient (GDK_DISPLAY(), wm_window);
                        gtk_timeout_add (1000, kill_timeout, info);
                }
        } else {
                start_do (info);
        }
}

WindowManager *
wm_guess_current (void)
{
        return NULL;
}
