/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gnome-vfs-module-callback.h - registering for callbacks from modules

   Copyright (C) 2001 Eazel, Inc
   Copyright (C) 2001 Free Software Foundation
   Copyright (C) 2001 Maciej Stachowiak

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

   Authors: Maciej Stachowiak <mjs@noisehavoc.org>
            Seth Nickell <snickell@stanford.edu>
	    Michael Fleming <mfleming@eazel.com>
*/

#ifndef GNOME_VFS_MODULE_CALLBACK_H
#define GNOME_VFS_MODULE_CALLBACK_H

#include <glib.h>


typedef void (* GnomeVFSModuleCallback) (gconstpointer in,
					 gsize         in_size,
					 gpointer      out,
					 gsize         out_size,
					 gpointer      callback_data);

typedef void (* GnomeVFSModuleCallbackResponse) (gpointer response_data);

typedef void (* GnomeVFSAsyncModuleCallback) (gconstpointer                  in,
					      gsize                          in_size,
					      gpointer                       out,
					      gsize                          out_size,
					      gpointer                       callback_data,
					      GnomeVFSModuleCallbackResponse response,
					      gpointer                       response_data);


void gnome_vfs_module_callback_set_default       (const char                  *callback_name,
						  GnomeVFSModuleCallback       callback,
						  gpointer                     callback_data,
						  GDestroyNotify               destroy_notify);
void gnome_vfs_module_callback_push              (const char                  *callback_name,
						  GnomeVFSModuleCallback       callback,
						  gpointer                     callback_data,
						  GDestroyNotify               destroy_notify);
void gnome_vfs_module_callback_pop               (const char                  *callback_name);

void gnome_vfs_async_module_callback_set_default (const char                  *callback_name,
						  GnomeVFSAsyncModuleCallback  callback,
						  gpointer                     callback_data,
						  GDestroyNotify               destroy_notify);
void gnome_vfs_async_module_callback_push        (const char                  *callback_name,
						  GnomeVFSAsyncModuleCallback  callback,
						  gpointer                     callback_data,
						  GDestroyNotify               destroy_notify);
void gnome_vfs_async_module_callback_pop         (const char                  *callback_name);

#endif
