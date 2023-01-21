/*
 * bonobo-moniker-extender-file.c: 
 *
 * Author:
 *	Dietmar Maurer (dietmar@helixcode.com)
 *
 * Copyright 2000, Helix Code, Inc.
 */
#include <config.h>
#include <bonobo/bonobo-storage.h>
#include <bonobo/bonobo-exception.h>
#include <bonobo/bonobo-moniker.h>
#include <bonobo/bonobo-moniker-extender.h>
#include <bonobo/bonobo-moniker-util.h>
#include <libgnome/gnome-mime.h>
#include <liboaf/liboaf.h>

#include "bonobo-moniker-std.h"

extern gchar * bonobo_internal_get_major_mime_type (const char *mime_type);

Bonobo_Unknown
bonobo_file_extender_resolve (BonoboMonikerExtender *extender,
			      const Bonobo_Moniker   m,
			      const Bonobo_ResolveOptions *options,
			      const CORBA_char      *display_name,
			      const CORBA_char      *requested_interface,
			      CORBA_Environment     *ev)
{
	const char         *mime_type;
	char               *mime_type_major;
	char               *oaf_requirements;
	Bonobo_Unknown      object;
	Bonobo_Persist      persist;
	OAF_ActivationID    ret_id;
	const char         *fname;
	OAF_ServerInfoList *result;
	char               *oafiid;

	if (strchr (display_name, ':'))
		fname = strchr (display_name, ':') + 1;
	else
		fname = display_name;

	g_warning ("Filename : '%s'", fname);

	mime_type = gnome_mime_type_of_file (fname);
	mime_type_major = bonobo_internal_get_major_mime_type (mime_type);
	
	oaf_requirements = g_strdup_printf (
		"bonobo:supported_mime_types.has_one (['%s', '%s/*']) AND "
		"repo_ids.has ('%s') AND "
		"repo_ids.has ('IDL:Bonobo/PersistFile:1.0')",
		mime_type, mime_type_major,
		requested_interface);
	
	result = oaf_query (oaf_requirements, NULL, ev);
	if (BONOBO_EX (ev) || result == NULL || result->_buffer == NULL ||
	    !result->_buffer[0].iid)
		return CORBA_OBJECT_NIL;

	g_free (oaf_requirements);
	g_free (mime_type_major);
	
	oafiid = g_strdup (result->_buffer[0].iid);

	CORBA_free (result);

	object = bonobo_url_lookup (oafiid, (char *) display_name, ev);
	if (!BONOBO_EX (ev) && object != CORBA_OBJECT_NIL) {
		g_free (oafiid);
		Bonobo_Unknown_ref (object, ev);
		if (!BONOBO_EX (ev))
			return bonobo_moniker_util_qi_return (object, 
			        requested_interface, ev);
	}
	
	CORBA_exception_init (ev);

	object = oaf_activate_from_id (oafiid, 0, &ret_id, ev);

	g_free (oafiid);	

	if (BONOBO_EX (ev) || object == CORBA_OBJECT_NIL)
		return CORBA_OBJECT_NIL;

	persist = Bonobo_Unknown_queryInterface (
		object, "IDL:Bonobo/PersistFile:1.0", ev);

	if (BONOBO_EX (ev) || persist == CORBA_OBJECT_NIL) {
		bonobo_object_release_unref (object, ev);
		return CORBA_OBJECT_NIL;
	}

	if (persist != CORBA_OBJECT_NIL) {
		Bonobo_PersistFile_load (persist, fname, ev);

		bonobo_object_release_unref (persist, ev);

		return bonobo_moniker_util_qi_return (
			object, requested_interface, ev);
	}
	
	return CORBA_OBJECT_NIL;
}
