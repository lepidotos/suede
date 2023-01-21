/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*-

   nautilus-vfs-directory.h: Subclass of NautilusDirectory to implement the
   the case of a VFS directory.
 
   Copyright (C) 1999, 2000 Eazel, Inc.
  
   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.
  
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.
  
   You should have received a copy of the GNU General Public
   License along with this program; if not, write to the
   Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
  
   Author: Darin Adler <darin@bentspoon.com>
*/

#ifndef NAUTILUS_VFS_DIRECTORY_H
#define NAUTILUS_VFS_DIRECTORY_H

#include "nautilus-directory.h"

#define NAUTILUS_TYPE_VFS_DIRECTORY \
	(nautilus_vfs_directory_get_type ())
#define NAUTILUS_VFS_DIRECTORY(obj) \
	(GTK_CHECK_CAST ((obj), NAUTILUS_TYPE_VFS_DIRECTORY, NautilusVFSDirectory))
#define NAUTILUS_VFS_DIRECTORY_CLASS(klass) \
	(GTK_CHECK_CLASS_CAST ((klass), NAUTILUS_TYPE_VFS_DIRECTORY, NautilusVFSDirectoryClass))
#define NAUTILUS_IS_VFS_DIRECTORY(obj) \
	(GTK_CHECK_TYPE ((obj), NAUTILUS_TYPE_VFS_DIRECTORY))
#define NAUTILUS_IS_VFS_DIRECTORY_CLASS(klass) \
	(GTK_CHECK_CLASS_TYPE ((klass), NAUTILUS_TYPE_VFS_DIRECTORY))

typedef struct NautilusVFSDirectoryDetails NautilusVFSDirectoryDetails;

typedef struct {
	NautilusDirectory parent_slot;
	NautilusVFSDirectoryDetails *details;
} NautilusVFSDirectory;

typedef struct {
	NautilusDirectoryClass parent_slot;
} NautilusVFSDirectoryClass;

GtkType nautilus_vfs_directory_get_type (void);

#endif /* NAUTILUS_VFS_DIRECTORY_H */
