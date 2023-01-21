/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */

/* 
 * Copyright (C) 2000 Eazel, Inc
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * Author: Maciej Stachowiak <mjs@eazel.com>
 */

/* main.c - Main function and object activation function for music
 * view component.
 */

#include <config.h>

#include "nautilus-music-view.h"
#include <libnautilus/nautilus-view-standard-main.h>
#include <libnautilus-private/nautilus-global-preferences.h>
#include <eel/eel-debug.h>

#define FACTORY_IID     "OAFIID:nautilus_music_view_factory:1be0c129-87cd-4daa-9d3a-94397de9bce2"
#define VIEW_IID        "OAFIID:nautilus_music_view:9456b5d2-60a8-407f-a56e-d561e1821391"

int
main (int argc, char *argv[])
{
	/* Make criticals and warnings stop in the debugger if NAUTILUS_DEBUG is set.
	 * Unfortunately, this has to be done explicitly for each domain.
	 */
	if (g_getenv ("NAUTILUS_DEBUG") != NULL) {
		eel_make_warnings_and_criticals_stop_in_debugger (G_LOG_DOMAIN, NULL);
	}

	return nautilus_view_standard_main ("nautilus-music-view",
					    VERSION,
					    PACKAGE,
					    GNOMELOCALEDIR,
					    argc,
					    argv,
					    FACTORY_IID,
					    VIEW_IID,
					    nautilus_view_create_from_get_type_function,
					    nautilus_global_preferences_initialize,
					    nautilus_music_view_get_type);
}
