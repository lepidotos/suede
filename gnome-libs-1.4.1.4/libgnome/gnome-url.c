/* gnome-url.c
 * Copyright (C) 1998, James Henstridge <james@daa.com.au>
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
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc.,  59 Temple Place - Suite 330, Cambridge, MA 02139, USA.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include <string.h>
#include <glib.h>
#include <libgnome/gnome-defs.h>
#include <libgnome/gnome-config.h>
#include <libgnome/gnome-exec.h>
#include <libgnome/gnome-util.h>
#include "gnome-popt.h" /* poptParseArgvString */
#include "gnome-url.h"

#define DEFAULT_HANDLER "gnome-moz-remote --newwin \"%s\""

static gchar *
gnome_url_default_handler (const char *protocol)
{
	static gchar *default_handler = 0;

	if (!default_handler) {
		gchar *str, *app;
		gboolean def;
		str = gnome_config_get_string_with_default ("/Gnome/URL Handlers/default-show",
							    &def);
		if (def) {
			app = gnome_is_program_in_path ("nautilus");
			if (app) {
				g_free (app);
				app = "nautilus \"%s\"";
			} else
				app = "gnome-help-browser \"%s\"";

			/* first time gnome_url_show is run -- set up some useful defaults */
			default_handler = DEFAULT_HANDLER;
			gnome_config_set_string ("/Gnome/URL Handlers/default-show", default_handler);

			g_free (gnome_config_get_string_with_default(
				"/Gnome/URL Handlers/info-show", &def));
			if (def)
				gnome_config_set_string ("/Gnome/URL Handlers/info-show", app);
			g_free (gnome_config_get_string_with_default(
				"/Gnome/URL Handlers/man-show", &def));
			if (def)
				gnome_config_set_string ("/Gnome/URL Handlers/man-show", app);
			g_free (gnome_config_get_string_with_default(
				"/Gnome/URL Handlers/ghelp-show", &def));
			if (def) 
				gnome_config_set_string ("/Gnome/URL Handlers/ghelp-show", app);

			gnome_config_sync_file ("/Gnome/");

			if (protocol &&
			    (strcmp (protocol, "ghelp") == 0 ||
			      strcmp (protocol, "info") == 0 ||
			      strcmp (protocol, "man") == 0)) {
				return app;
			}
			
		} else
			default_handler = str;
	}
	return default_handler;
}

/**
 * gnome_url_show
 * @url: URL to show
 *
 * Description:
 * Loads the given URL in an appropriate viewer.  The viewer is deduced from
 * the protocol part of the URL.  That is all that the caller should know
 * about the function.
 *
 * Internally, the handler for a given URL is deduced by looking in the
 * /Gnome/URL Handlers/<protocol>-show key in the user's configuration
 * files.  The key is a string that will be passed to gnome_execute_shell(),
 * after the %s is replaced with with the URL.  If that key can't be found,
 * it falls back to /Gnome/URL Handlers/default-show, and if that isn't
 * found, uses the contents of the DEFAULT_HANDLER macro in this file.
 *
 * If no /Gnome/URL Handlers keys are set, some sensible defaults are added
 * to the user's configuration files.
 *
 * We really need a capplet so a user can configure the behaviour of this
 * function.
 **/
void
gnome_url_show(const gchar *url)
{
	gint i;
	gchar *pos, *template;
	gboolean free_template = FALSE;
	int argc;
       	char **argv;
	
	g_return_if_fail (url != NULL);
	pos = strchr (url, ':');

	if (pos){
		gchar *protocol, *path;
		gboolean def;
		
		g_return_if_fail (pos >= url);
		protocol = g_new (gchar, pos - url + 1);
		strncpy (protocol, url, pos - url);
		protocol [pos - url] = '\0';
		g_strdown (protocol);
		
		path = g_strconcat ("/Gnome/URL Handlers/", protocol, "-show", NULL);
		template = gnome_config_get_string_with_default (path, &def);
		g_free (path);
		if (def)
			template = gnome_url_default_handler (protocol);
		else
			free_template = TRUE;
		g_free (protocol);
	} else /* no : ? -- this shouldn't happen.  Use default handler */
		template = gnome_url_default_handler (NULL);

	/* we use a popt function as it does exactly what we want to do and
	   gnome already uses popt */
	if(poptParseArgvString(template, &argc, &argv) != 0) {
		/* can't parse */
		g_warning("Parse error of '%s'", template);
		return;
	}

	/* we can just replace the entry in the array since the
	 * array is all in one buffer, we won't leak */
	for(i = 0; i < argc; i++) {
		if(strcmp(argv[i], "%s") == 0)
			/* we can safely cast as it will not get freed nor
			 * modified */
			argv[i] = (char *)url;
	}

	/* use execute async, and not the shell, shell is evil and a
	 * security hole */
	gnome_execute_async (NULL, argc, argv);

	if (free_template)
		g_free (template);

	/* the way the poptParseArgvString works is that the entire thing
	 * is allocated as one buffer, so just free will suffice, also
	 * it must be free and not g_free */
	free(argv);
}
