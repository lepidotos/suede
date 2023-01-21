/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */

/* fm-error-reporting.h - interface for file manager functions that report
 			  errors to the user.

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

#ifndef FM_ERROR_REPORTING_H
#define FM_ERROR_REPORTING_H

#include <glib.h>
#include <gtk/gtkwindow.h>
#include <libgnomevfs/gnome-vfs-types.h>
#include <libnautilus-private/nautilus-file.h>

void fm_report_error_loading_directory	 (NautilusFile   *file,
					  GnomeVFSResult  error_code,
					  GtkWindow	 *parent_window);
void fm_report_error_renaming_file       (NautilusFile   *file,
					  const char     *new_name,
					  GnomeVFSResult  error_code,
					  GtkWindow	 *parent_window);
void fm_report_error_setting_permissions (NautilusFile   *file,
					  GnomeVFSResult  error_code,			    
					  GtkWindow	 *parent_window);
void fm_report_error_setting_owner       (NautilusFile   *file,
					  GnomeVFSResult  error_code,  
					  GtkWindow	 *parent_window);
void fm_report_error_setting_group       (NautilusFile   *file,
					  GnomeVFSResult  error_code,
					  GtkWindow	 *parent_window);

/* FIXME bugzilla.gnome.org 42394: Should this file be renamed or should these be moved? */
void fm_rename_file                      (NautilusFile   *file,
					  const char     *new_name);

#endif /* FM_ERROR_REPORTING_H */
