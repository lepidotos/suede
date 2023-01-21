#include <config.h>
#include <gnome.h>
#include <libgnorba/gnorba.h>
#include <bonobo.h>
#if USING_OAF
#include <liboaf/liboaf.h>
#else
#include <libgnorba/gnorba.h>
#endif
#include <stdio.h>

CORBA_ORB	    orb;
BonoboPropertyBag  *pb;
CORBA_Environment   ev;

enum {
	PROP_BOOLEAN_TEST,
	PROP_INTEGER_TEST,
	PROP_LONG_TEST,
	PROP_FLOAT_TEST,
	PROP_DOUBLE_TEST,
	PROP_STRING_TEST
} PropIdx;

typedef struct {
	gint      i;
	glong     l;
	gboolean  b;
	gfloat    f;
	gdouble   d;
	char     *s;
} PropData;

static void
set_prop (BonoboPropertyBag *bag,
	  const BonoboArg   *arg,
	  guint              arg_id,
	  CORBA_Environment *ev,
	  gpointer           user_data)
{
	PropData *pd = user_data;

	switch (arg_id) {
	case PROP_BOOLEAN_TEST:
		pd->b = BONOBO_ARG_GET_BOOLEAN (arg);
		break;

	case PROP_INTEGER_TEST:
		pd->i = BONOBO_ARG_GET_INT (arg);
		break;

	case PROP_LONG_TEST:
		pd->l = BONOBO_ARG_GET_LONG (arg);
		break;

	case PROP_FLOAT_TEST:
		pd->f = BONOBO_ARG_GET_FLOAT (arg);
		break;

	case PROP_DOUBLE_TEST:
		pd->d = BONOBO_ARG_GET_DOUBLE (arg);
		break;

	case PROP_STRING_TEST:
		g_free (pd->s);
		pd->s = g_strdup (BONOBO_ARG_GET_STRING (arg));
		break;

	default:
		bonobo_exception_set (ev, ex_Bonobo_PropertyBag_NotFound);
	};
}

static void
get_prop (BonoboPropertyBag *bag,
	  BonoboArg         *arg,
	  guint              arg_id,
	  CORBA_Environment *ev,
	  gpointer           user_data)
{
	PropData *pd = user_data;

	switch (arg_id) {
	case PROP_BOOLEAN_TEST:
		BONOBO_ARG_SET_BOOLEAN (arg, pd->b);
		break;

	case PROP_INTEGER_TEST:
		BONOBO_ARG_SET_INT (arg, pd->i);
		break;

	case PROP_LONG_TEST:
		BONOBO_ARG_SET_LONG (arg, pd->l);
		break;

	case PROP_FLOAT_TEST:
		BONOBO_ARG_SET_FLOAT (arg, pd->f);
		break;

	case PROP_DOUBLE_TEST:
		BONOBO_ARG_SET_DOUBLE (arg, pd->d);
		break;

	case PROP_STRING_TEST:
		BONOBO_ARG_SET_STRING (arg, pd->s);
		break;

	default:
		bonobo_exception_set (ev, ex_Bonobo_PropertyBag_NotFound);
	};
}

static char *
simple_prop_to_string (BonoboArg *arg)
{
	static char s [1024];

	if (!arg) {
		g_snprintf (s, sizeof (s), "NULL");
		return g_strdup (s);
	}
	
	g_assert (arg->_type != NULL);
	
	switch (arg->_type->kind) {

	case CORBA_tk_boolean:
		g_snprintf (s, sizeof (s), "boolean: %s",
			    BONOBO_ARG_GET_BOOLEAN (arg) ? "True" : "False");
		break;

	case CORBA_tk_long:
		g_snprintf (s, sizeof (s), "integer: %d",
			    BONOBO_ARG_GET_INT (arg));
		break;

	case CORBA_tk_float:
		g_snprintf (s, sizeof (s), "float: %f",
			    BONOBO_ARG_GET_FLOAT (arg));
		break;

	case CORBA_tk_double:
		g_snprintf (s, sizeof (s), "double: %g",
			    BONOBO_ARG_GET_DOUBLE (arg));
		break;

	case CORBA_tk_string:
	{
		g_snprintf (s, sizeof (s), "string: '%s'",
			    BONOBO_ARG_GET_STRING (arg));
		break;
	}

	default:
		g_error ("Unhandled type: %d", arg->_type->kind);
		break;
	}

	return g_strdup (s);
}

/*
static void
value_changed_cb (BonoboPropertyBag *pb, char *name, char *type,
		  gpointer old_value, gpointer new_value,
		  gpointer user_data)
{
	char *s1, *s2;

	s1 = simple_prop_to_string (old_value);
	s2 = simple_prop_to_string (new_value);
	g_print ("Prop %s [%s]: value changed from %s to %s\n",
		 name, type, s1, s2);
	g_free (s1);
	g_free (s2);
}
*/

static void
create_bag (void)
{
	PropData   *pd = g_new0 (PropData, 1);
	BonoboArg  *def;
	char       *dstr;
	CORBA_char *ior;

	pd->i = 987654321;
	pd->l = 123456789;
	pd->b = TRUE;
	pd->f = 2.71828182845;
	pd->d = 3.14159265358;
	pd->s = g_strdup ("Hello world");

	/* Create the property bag. */
	pb = bonobo_property_bag_new (get_prop, set_prop, pd);

	dstr = "Testing 1 2 3";
	/* Add properties */
	bonobo_property_bag_add (pb, "int-test", PROP_INTEGER_TEST,
				 BONOBO_ARG_INT, NULL, dstr, 0);

	def = bonobo_arg_new (BONOBO_ARG_STRING);
	BONOBO_ARG_SET_STRING (def, "a default string");

	bonobo_property_bag_add (pb, "string-test", PROP_STRING_TEST,
				 BONOBO_ARG_STRING, def, dstr,
				 BONOBO_PROPERTY_READABLE |
				 BONOBO_PROPERTY_WRITEABLE);

	bonobo_property_bag_add (pb, "long-test", PROP_LONG_TEST,
				 BONOBO_ARG_LONG, NULL, dstr, 0);

	bonobo_property_bag_add (pb, "boolean-test", PROP_BOOLEAN_TEST,
				 BONOBO_ARG_BOOLEAN, NULL, dstr, 0);

	def = bonobo_arg_new (BONOBO_ARG_FLOAT);
	BONOBO_ARG_SET_FLOAT (def, 0.13579);
	bonobo_property_bag_add (pb, "float-test", PROP_FLOAT_TEST,
				 BONOBO_ARG_FLOAT, def, dstr, 0);

	bonobo_property_bag_add (pb, "double-test", PROP_DOUBLE_TEST,
				 BONOBO_ARG_DOUBLE, NULL, dstr, 0);

	/* Print out the IOR for this object. */
	ior = CORBA_ORB_object_to_string (orb, BONOBO_OBJREF (pb), &ev);

	/* So we can tee the output to compare */
	fprintf (stderr, "%s\n", ior);
	fflush (stderr);

	CORBA_free (ior);
}

static void
print_props (void)
{
	GList *props;
	GList *l;

	/* This is a private function; we're just using it for testing. */
	props = bonobo_property_bag_get_prop_list (pb);

	for (l = props; l != NULL; l = l->next) {
		BonoboProperty *prop = l->data;
		BonoboArg      *arg;
		char           *s1, *s2;

		arg = bonobo_property_bag_get_value (pb, prop->name, NULL);
		s1  = simple_prop_to_string (arg);
		bonobo_arg_release (arg);

		s2  = simple_prop_to_string (prop->default_value);

		g_print ("Prop %12s [%2d] %s %s %s %s %s %s %s\n",
			 prop->name, prop->type->kind,
			 s1, s2,
			 prop->docstring,
			 prop->flags & BONOBO_PROPERTY_UNSTORED        ? "Unstored"         : "Stored",
			 prop->flags & BONOBO_PROPERTY_READABLE        ? "Readable"         : "NotReadable",
			 prop->flags & BONOBO_PROPERTY_WRITEABLE       ? "Writeable"        : "NotWriteable",
			 prop->flags & BONOBO_PROPERTY_USE_DEFAULT_OPT ? "DefaultOptimized" : "NotDefaultOptimized");

		g_free (s1);
		g_free (s2);

	}

	g_list_free (props);
}

static void
quit_main (GtkObject *object, gpointer dummy)
{
	gtk_main_quit ();
}

int
main (int argc, char **argv)
{
	CORBA_exception_init (&ev);

#if USING_OAF
        gnome_init_with_popt_table(
		"test property server", "0.0", argc, argv,
		oaf_popt_options, 0, NULL);

	orb = oaf_init (argc, argv);
#else
	gnome_CORBA_init_with_popt_table (
		"test property server", "0.0", &argc, argv,
		NULL, 0, NULL, GNORBA_INIT_SERVER_FUNC, &ev);

	orb = gnome_CORBA_ORB ();
#endif

	if (!bonobo_init (orb, NULL, NULL))
		g_error ("Could not initialize Bonobo");

	create_bag ();

	print_props ();

	gtk_signal_connect (GTK_OBJECT (bonobo_context_running_get ()),
			    "last_unref", GTK_SIGNAL_FUNC (quit_main), NULL);

	bonobo_main ();

	return 0;
}
