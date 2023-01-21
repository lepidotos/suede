#include <config.h>

#include <bonobo/bonobo-shlib-factory.h>
#include "bonobo-moniker-std.h"

static BonoboObject *
bonobo_std_moniker_factory (BonoboGenericFactory *this,
			    const char           *object_id,
			    void                 *data)
{
	g_return_val_if_fail (object_id != NULL, NULL);

	if (!strcmp (object_id, "OAFIID:Bonobo_Moniker_File"))

		return BONOBO_OBJECT (bonobo_moniker_simple_new (
			"file:", bonobo_moniker_file_resolve));

	else if (!strcmp (object_id, "OAFIID:Bonobo_Moniker_Item"))

		return BONOBO_OBJECT (bonobo_moniker_simple_new (
			"!", bonobo_moniker_item_resolve));
	
	else if (!strcmp (object_id, "OAFIID:Bonobo_Moniker_Oaf"))

		return BONOBO_OBJECT (bonobo_moniker_simple_new (
			"oafiid:", bonobo_moniker_oaf_resolve));

	else if (!strcmp (object_id, "OAFIID:Bonobo_Moniker_Cache"))

		return BONOBO_OBJECT (bonobo_moniker_simple_new (
			"cache:", bonobo_moniker_cache_resolve));

	else if (!strcmp (object_id, "OAFIID:Bonobo_Moniker_New"))

		return BONOBO_OBJECT (bonobo_moniker_simple_new (
			"new:", bonobo_moniker_new_resolve));

/*
 * Deprecated until Miguel likes it.
 *
 *	else if (!strcmp (object_id, "OAFIID:Bonobo_Moniker_Query"))
 *		
 *		return BONOBO_OBJECT (bonobo_moniker_simple_new (
 *			"query:", bonobo_moniker_query_resolve));
 */


	else if (!strcmp (object_id, "OAFIID:Bonobo_MonikerExtender_file"))
		
		return BONOBO_OBJECT (bonobo_moniker_extender_new (
			bonobo_file_extender_resolve, NULL));

	else if (!strcmp (object_id, "OAFIID:Bonobo_MonikerExtender_stream"))
		
		return BONOBO_OBJECT (bonobo_moniker_extender_new (
			bonobo_stream_extender_resolve, NULL));

	else
		g_warning ("Failing to manufacture a '%s'", object_id);

	return NULL;
}


BONOBO_OAF_SHLIB_FACTORY_MULTI ("OAFIID:Bonobo_Moniker_std_Factory",
				"bonobo standard moniker",
				bonobo_std_moniker_factory,
				NULL);


