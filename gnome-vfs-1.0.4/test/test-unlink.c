/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* test-unlink.c - Test program for unlink operation in the GNOME
   Virtual File System.

   Copyright (C) 1999 Free Software Foundation
   Copyright (C) 2000 Eazel Inc.

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

   Authors: 
   	Ettore Perazzoli <ettore@gnu.org> 
	Ian McKellar <yakk@yakk.net.au>
 */

#include "gnome-vfs.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

static void
show_result (GnomeVFSResult result, const gchar *what, const gchar *text_uri)
{
	fprintf (stderr, "%s `%s': %s\n",
		 what, text_uri, gnome_vfs_result_to_string (result));
	if (result != GNOME_VFS_OK)
		exit (1);
}

int
main (int argc, char **argv)
{
	GnomeVFSResult    result;
	GnomeVFSURI 	 *uri;
	gchar            *text_uri;

	if (argc != 2) {
		printf ("Usage: %s <uri>\n", argv[0]);
		return 1;
	}

	if (! gnome_vfs_init ()) {
		fprintf (stderr, "Cannot initialize gnome-vfs.\n");
		return 1;
	}

	uri = gnome_vfs_uri_new (argv[1]);
	if (uri == NULL) {
		fprintf (stderr, "URI not valid.\n");
		return 1;
	}

        text_uri = gnome_vfs_uri_to_string (uri, GNOME_VFS_URI_HIDE_NONE);

	result = gnome_vfs_unlink (text_uri);
	show_result (result, "unlink", text_uri);

	g_free (text_uri);

	return 0;
}
