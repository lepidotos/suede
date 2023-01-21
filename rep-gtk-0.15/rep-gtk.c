/* Copyright (C) 1997, 1998, 1999 Marius Vollmer
 * Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>
 *
 * $Id: rep-gtk.c,v 1.39 2000/09/18 13:05:15 john Exp $
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>
#include <assert.h>
#include <gtk/gtk.h>
#include <gdk/gdkprivate.h>
#include "rep-gtk.h"
#include <string.h>
#include <limits.h>

#ifdef HAVE_LOCALE_H
# include <locale.h>
#endif

/* Define this to enable some output during GC and other interesting
   actions. */
#undef DEBUG_PRINT

static int
list_length (repv list)
{
    repv len = Flength (list);
    return (len && rep_INTP (len)) ? rep_INT (len) : 0;
}



/* Associating SCM values with Gtk pointers.

   We keep a hash table that can store a SCM value for an arbitray
   gpointer.  This is used for the proxies of GtkObjects and the boxed
   types.  */

static GHashTable *proxy_tab;

static void
enter_proxy (gpointer obj, repv proxy)
{
  if (proxy_tab == NULL)
    proxy_tab = g_hash_table_new (NULL, NULL);
  g_hash_table_insert (proxy_tab, obj, (gpointer)proxy);
}

static repv
get_proxy (gpointer obj)
{
  if (proxy_tab)
    {
      gpointer val = g_hash_table_lookup (proxy_tab, obj);
      return val? (repv) val : Qnil;
    }
  return Qnil;
}

static void
forget_proxy (gpointer obj)
{
  g_hash_table_remove (proxy_tab, obj);
}



/* Storing additional info about a GtkType.

   Each GtkType has a unique sequence number.  We use that to simply
   index an array of sgtk_type_info pointers.  The array is grown
   dynamically when necessary. */

#define TYPE_INFO_INCR_MASK 0xFF

static sgtk_type_info **type_info_tab;
static guint n_type_info_tab = 0;

static void
enter_type_info (sgtk_type_info *info)
{
  guint seqno = GTK_TYPE_SEQNO (info->type);

  if (seqno >= n_type_info_tab)
    {
      guint i, new_size = (seqno+TYPE_INFO_INCR_MASK)&(~TYPE_INFO_INCR_MASK);
      type_info_tab = (sgtk_type_info **)
			rep_realloc ((char *)type_info_tab,
				     sizeof(sgtk_type_info*) * new_size);
      for (i = n_type_info_tab; i < new_size; i++)
	type_info_tab[i] = NULL;
      n_type_info_tab = new_size;
    }

  type_info_tab[seqno] = info;
}

sgtk_type_info*
sgtk_get_type_info (guint seqno)
{
  if (seqno >= n_type_info_tab)
    return NULL;
  return type_info_tab[seqno];
}

static sgtk_type_info*
must_get_type_info (guint seqno)
{
  sgtk_type_info *info = sgtk_get_type_info (seqno);
  if (info == NULL)
    abort ();
  return info;
}

typedef struct _type_infos {
  struct _type_infos *next;
  sgtk_type_info **infos;
} type_infos;

static type_infos *all_type_infos;

/* Find types that are mentioned in our *.defs files but are not
   provided by the Gtk run-time system.  This is only used
   occasionally to update the table in sgtk_try_missing_type.  */
#ifdef NEED_UNUSED_CODE
static void
sgtk_find_missing_types (type_infos *infos)
{
  sgtk_type_info **ip;
  for (ip = infos->infos; *ip; ip++)
    {
      if (gtk_type_from_name ((*ip)->name) == GTK_TYPE_INVALID
	  && (*ip)->type != GTK_TYPE_OBJECT)
	printf ("missing: %s, %s\n",
		(*ip)->name, gtk_type_name ((*ip)->type));
    }
}
#endif

void
sgtk_register_type_infos (sgtk_type_info **infos)
{
  type_infos *t;

  sgtk_init ();

  t = (type_infos *) rep_alloc (sizeof(type_infos));
  t->infos = infos;
  t->next = all_type_infos;
  all_type_infos = t;

#if 0
  sgtk_find_missing_types (t);
#endif
}

void
sgtk_register_type_infos_gtk (GtkTypeInfo **infos)
{
  GtkTypeInfo **t;

  for (t = infos; t && *t; t++)
    gtk_type_unique (GTK_TYPE_BOXED, *t);
}

/* When INFO refers to one of the known `missing' types, we initialize
   that type ourselves.  This is used to fix certain discrepancies
   between old Gtk versions and our *.defs files.  It is not OK to do
   this in general because we should not assume that we can safely
   initialize types from other modules.  */

static GtkType
sgtk_try_missing_type (char *name)
{
  static sgtk_type_info missing[] = {
    { "GdkGC", GTK_TYPE_BOXED },
    { "GdkPixbuf", GTK_TYPE_BOXED },	/* XXX okay? */
    { "GtkToolbarStyle", GTK_TYPE_ENUM },
    { "GtkToolbarChildType", GTK_TYPE_ENUM },
    { "GtkTreeViewMode", GTK_TYPE_ENUM },
    { "GtkSpinButtonUpdatePolicy", GTK_TYPE_ENUM },
    { "GtkCellType", GTK_TYPE_ENUM },
    { "GdkOverlapType", GTK_TYPE_ENUM },
    { "GdkWMDecoration", GTK_TYPE_FLAGS },
    { "GdkWMFunction", GTK_TYPE_FLAGS },
    { "GdkVisibilityState", GTK_TYPE_ENUM },
    { "GdkInputSource", GTK_TYPE_ENUM },
    {NULL, GTK_TYPE_NONE}
  };

  sgtk_type_info *m;
  for (m = missing; m->name; m++)
    if (!strcmp (m->name, name))
      {
	GtkTypeInfo info = { NULL };
	info.type_name = name;
	return gtk_type_unique (m->type, &info);
      }

  return GTK_TYPE_INVALID;
}

static int
sgtk_fillin_type_info (sgtk_type_info *info)
{
  if (info->type != GTK_TYPE_OBJECT
      && info->type == GTK_FUNDAMENTAL_TYPE (info->type)
      && info->type != GTK_TYPE_INVALID)
    {
      GtkType parent_type = info->type;
      GtkType this_type = gtk_type_from_name (info->name);
      if (this_type == GTK_TYPE_INVALID)
	this_type = sgtk_try_missing_type (info->name);
      if (this_type == GTK_TYPE_INVALID)
	{
	  if (info->type == GTK_TYPE_BOXED)
	  fprintf (stderr, "unknown type `%s'.\n", info->name);
	  return 0;
	}
      info->type = this_type;
      if (GTK_FUNDAMENTAL_TYPE (info->type) != parent_type)
	{
	  fprintf (stderr, "mismatch for type `%s'.\n", info->name);
	  info->type = GTK_TYPE_INVALID;
	  return 0;
	}
      enter_type_info (info);
    }

  return 1;
}      
     
sgtk_type_info*
sgtk_maybe_find_type_info (GtkType type)
{
  sgtk_type_info *info;
  type_infos *infos;
  char *name;

  info = sgtk_get_type_info (GTK_TYPE_SEQNO(type));
  if (info)
    return info;

  /* XXX - merge this with the GtkObject code.  I don't have the brain
     right now to do it. */

  name = gtk_type_name (type);
  for (infos = all_type_infos; infos; infos = infos->next)
    {
      sgtk_type_info **ip;
      for (ip = infos->infos; *ip; ip++)
	if (!strcmp ((*ip)->name, name))
	  {
	    if (GTK_FUNDAMENTAL_TYPE (type) != (*ip)->type)
	      {
		fprintf (stderr, "mismatch for type `%s'.\n", name);
		info->type = GTK_TYPE_INVALID;
		abort ();
	      }
	    (*ip)->type = type;
	    enter_type_info (*ip);
	    return *ip;
	  }
    }

  /* XXX - should use the Gtk+ type introspection here instead of
     giving up. */

  return NULL;
}

sgtk_type_info *
sgtk_find_type_info (GtkType type)
{
  sgtk_type_info *info = sgtk_maybe_find_type_info (type);

  if (info)
    return info;

  fprintf (stderr, "unknown type `%s'.\n", gtk_type_name (type));
  abort ();
}



/* GtkObjects.

   GtkObjects are wrapped with a smob.  The smob of a GtkObject is
   called its proxy.  The proxy and its GtkObject are strongly
   connected; that is, the GtkObject will stay around as long as the
   proxy is referenced from Scheme, and the proxy will not be
   collected as long as the GtkObject is used from outside of Scheme.

   The lifetime of GtkObjects is controlled by a reference count,
   while Scheme objects are managed by a tracing garbage collector
   (mark/sweep).  These two techniques are made to cooperate like
   this: the pointer from the proxy to the GtkObject is reflected in
   the reference count of the GtkObject.  All proxies are kept in a
   list and those that point to GtkObjects with a reference count
   greater than the number of `internal' references are marked during
   the marking phase of the tracing collector.  An internal reference
   is one that goes from a GtkObject with a proxy to another GtkObject
   with a proxy.  We can only find a subset of the true internal
   references (because Gtk does not yet cooperate), but this should be
   good enough.

   By using this combination of tracing and reference counting it is
   possible to break the cycle that is formed by the proxy pointing to
   the GtkObject and the GtkObject pointing back.  It is
   straightforward to extend this to other kind of cycles that might
   occur.  For example, when connecting a Scheme procedure as a signal
   handler, the procedure is very likely to have the GtkObject that it
   is connected to in its environment.  This cycle can be broken by
   including the procedure in the set of Scheme objects that get
   marked when we are tracing GtkObjects with a reference count
   greater than 1.

   Therefore, each proxy contains a list of `protects' that are marked
   when the proxy itself is marked.  In addition to this, there is
   also a global list of `protects' that is used for Scheme objects
   that are somewhere in Gtk land but not clearly associated with a
   particular GtkObject (like timeout callbacks).

  */

struct sgtk_protshell {
  repv object;
  struct sgtk_protshell *next;
  struct sgtk_protshell **prevp;
};

static GMemChunk *sgtk_protshell_chunk;

/* Analogous to the PROTECTS list of a proxy but for SCM values that
   are not associated with a particular GtkObject. */

static struct sgtk_protshell *global_protects;

void
sgtk_unprotect (sgtk_protshell *prot)
{
  if ((*prot->prevp = prot->next) != 0)
    prot->next->prevp = prot->prevp;
  g_chunk_free (prot, sgtk_protshell_chunk);
}

static void
sgtk_mark_protects (sgtk_protshell *prots)
{
  while (prots)
    {
      rep_MARKVAL (prots->object);
      prots = prots->next;
    }
}

/* The CDR of a GtkObject smob points to one of these.  PROTECTS is a
   Scheme list of all SCM values that need to be protected from the GC
   because they are in use by OBJ.  PROTECTS includes the smob cell
   itself.  NEXT and PREVP are used to chain all proxies together for
   the marking mentioned above.  NEXT simply points to the next proxy
   struct and PREVP points to the pointer that points to us.  */

typedef struct _sgtk_object_proxy {
  repv car;
  GtkObject *obj;
  struct sgtk_protshell *protects;
  int traced_refs;
  struct _sgtk_object_proxy *next;
} sgtk_object_proxy;

/* The list of all existing proxies. */

static sgtk_object_proxy *all_proxies = NULL;

/* Insert the list of protshells starting at PROTS into the global
   protects list.  This is used when a proxy is freed so that we don't
   forget about its protects. */

static void
sgtk_move_prots_to_global (sgtk_protshell *prots)
{
  if (prots)
    {
      sgtk_protshell *g = global_protects;
      global_protects = prots;
      global_protects->prevp = &global_protects;
      if (g)
	{
	  sgtk_protshell *p;
	  for (p = prots; p->next; p = p->next)
	    ;
	  p->next = g;
	  g->prevp = &p->next;
	}
    }
}

/* The smob for GtkObjects.  */

static long tc16_gtkobj;

#define GTKOBJP(x)       (rep_CELL16_TYPEP(x, tc16_gtkobj))
#define GTKOBJ_PROXY(x)  ((sgtk_object_proxy *)rep_PTR(x))

void
sgtk_set_protect (repv protector, sgtk_protshell *prot)
{
  sgtk_protshell **prevp;

  if (GTKOBJP (protector))
    prevp = &(GTKOBJ_PROXY(protector)->protects);
  else
    prevp = &global_protects;
  
  if ((prot->next = *prevp) != 0)
	prot->next->prevp = &prot->next;
  *prevp = prot;
  prot->prevp = prevp;
}

repv
sgtk_get_protect (sgtk_protshell *prot)
{
  return prot->object;
}

sgtk_protshell *
sgtk_new_protect (repv obj)
{
  sgtk_protshell *prot = g_chunk_new (sgtk_protshell, sgtk_protshell_chunk);
  prot->object = obj;
  return prot;
}

sgtk_protshell *
sgtk_protect (repv protector, repv obj)
{
  sgtk_protshell *prot = sgtk_new_protect (obj);
  sgtk_set_protect (protector, prot);
  return prot;
}

static void
mark_traced_ref (GtkWidget *obj, void *data)
{
  repv p = (repv)get_proxy (obj);
  if (p != Qnil)
    {
      sgtk_object_proxy *proxy = GTKOBJ_PROXY (p);
#ifdef DEBUG_PRINT
      fprintf (stderr, "marking trace %p %s\n",
	       proxy->obj, gtk_type_name (GTK_OBJECT_TYPE (proxy->obj)));
#endif
      sgtk_mark_protects (proxy->protects);
    }
}

static void
gtkobj_mark (repv obj)
{
  sgtk_object_proxy *proxy = GTKOBJ_PROXY(obj);

#ifdef DEBUG_PRINT
  fprintf (stderr, "marking %p %s\n",
	   proxy->obj, gtk_type_name (GTK_OBJECT_TYPE (proxy->obj)));
#endif

  if (GTK_IS_CONTAINER (proxy->obj))
    gtk_container_foreach (GTK_CONTAINER(proxy->obj), mark_traced_ref, NULL);
  sgtk_mark_protects (proxy->protects);
}

static void
gtkobj_print (repv stream, repv obj)
{
    char buf[32];
  sgtk_object_proxy *proxy = GTKOBJ_PROXY (obj);
  GtkType tid = GTK_OBJECT_TYPE (proxy->obj);
  char *type = gtk_type_name (tid);
  rep_stream_puts (stream, "#<", -1, rep_FALSE);
  rep_stream_puts (stream, type ? type : "Gtk???", -1, rep_FALSE);
  rep_stream_puts (stream, " ", -1, rep_FALSE);
  if (GTK_OBJECT_DESTROYED (proxy->obj))
      rep_stream_puts (stream, "destroyed", -1, rep_FALSE);
  else
  {
      sprintf (buf, "%lx", (long)proxy->obj);
      rep_stream_puts (stream, buf, -1, rep_FALSE);
  }
  rep_stream_putc (stream, '>');
}

static void
gtkobj_free (repv obj)
{
  sgtk_object_proxy *proxy = GTKOBJ_PROXY (obj);

#ifdef DEBUG_PRINT
  fprintf (stderr, "freeing %p %s\n",
	   proxy->obj, gtk_type_name (GTK_OBJECT_TYPE (proxy->obj)));
#endif

  forget_proxy (proxy->obj);
  gtk_object_unref (proxy->obj);
  sgtk_move_prots_to_global (proxy->protects);
  rep_FREE_CELL ((char *)proxy);
}

static void
gtkobj_sweep (void)
{
  sgtk_object_proxy *proxy = all_proxies;
  all_proxies = 0;
  while (proxy != 0)
  {
      sgtk_object_proxy *next = proxy->next;
      if (! rep_GC_CELL_MARKEDP(rep_VAL(proxy)))
	  gtkobj_free (rep_VAL(proxy));
      else
      {
	  rep_GC_CLR_CELL (rep_VAL(proxy));
	  proxy->next = all_proxies;
	  all_proxies = proxy;
      }
      proxy = next;
  }
}

/* Treating GtkObject proxies right during GC.  We need to run custom
   code during the mark phase of the Scheme GC.  We do this by
   creating a new smob type and allocating one actual smob of it.
   This smob is made permanent and thus its marking function is
   invoked for every GC.  We hijack this function to do the tracing of
   all existing proxies as well. */

static void
count_traced_ref (GtkWidget *obj, void *data)
{
  repv p = (repv)get_proxy (obj);
  if (p != Qnil)
    {
      sgtk_object_proxy *proxy = GTKOBJ_PROXY (p);
#ifdef DEBUG_PRINT
      fprintf (stderr, "counting %p %s\n",
	       proxy->obj, gtk_type_name (GTK_OBJECT_TYPE (proxy->obj)));
#endif
      proxy->traced_refs++;
    }
}

static void
gtkobj_marker_hook (void)
{
  sgtk_object_proxy *proxy;

  /* We do two passes here.  The first pass counts how many references
     an object has from other objects that have a proxy.  The second
     pass marks all objects that have more than this number of
     references.  For the first pass to work, we need to enumerate all
     references that an object has to other objects.  We can't do that
     precisely without help from Gtk+ itself.  But luckily, *not*
     knowing about an `internal' reference is the conservative thing.
     Missing a reference will make it appear to us that an object has
     more `external' references to it than it really has, thus making
     us keep the proxy alive.  Only when these `external' references
     form a cycle over some Scheme values, we loose.  As a first
     approximation to the true set of references of a GtkObject, we
     just traverse its children with gtk_container_foreach.  */

  /* First pass. */
  for (proxy = all_proxies; proxy; proxy = proxy->next)
    {
      GtkObject *obj = proxy->obj;
#ifdef DEBUG_PRINT
      fprintf (stderr, "on %p %p\n", proxy, obj);
#endif
      if (GTK_IS_CONTAINER (obj))
	gtk_container_foreach (GTK_CONTAINER(obj), count_traced_ref, NULL);
    }
#ifdef DEBUG_PRINT
  fprintf (stderr, "done with pass 1.\n");
#endif

  /* Second pass. */
  for (proxy = all_proxies; proxy; proxy = proxy->next)
    {
      if (proxy->obj->ref_count > proxy->traced_refs + 1)
	{
#ifdef DEBUG_PRINT
	  fprintf (stderr, "hooking %p %s\n",
		   proxy->obj, gtk_type_name (GTK_OBJECT_TYPE (proxy->obj)));
#endif
	  /* mark the proxy itself */
	  rep_MARKVAL (rep_VAL (proxy));
	}
      /* always mark the protected objects, since they're moved to
         the global_protects list if the object is freed */
      sgtk_mark_protects (proxy->protects);
      proxy->traced_refs = 0;
    }
  sgtk_mark_protects (global_protects);
}

/* Create a proxy for OBJ. */

static repv
make_gtkobj (GtkObject *obj)
{
  sgtk_object_proxy *proxy;

  g_assert (obj->ref_count > 0);

  proxy = (sgtk_object_proxy *)rep_ALLOC_CELL (sizeof(sgtk_object_proxy));
  gtk_object_ref (obj);
  gtk_object_sink (obj);
#ifdef DEBUG_PRINT
  fprintf (stderr, "New proxy %p for %p %s\n", proxy, obj,
	   gtk_type_name (GTK_OBJECT_TYPE (obj)));
#endif
  proxy->obj = obj;
  proxy->protects = NULL;
  proxy->traced_refs = 0;
  proxy->next = all_proxies;
  all_proxies = proxy;

  proxy->car = tc16_gtkobj;
  enter_proxy (obj, rep_VAL(proxy));

  return rep_VAL(proxy);
}

/* Return the proxy for OBJ if it already has one, else create a new
   one.  When OBJ is NULL, return `#f'. */

repv
sgtk_wrap_gtkobj (GtkObject *obj)
{
  repv handle;

  if (obj == NULL)
    return Qnil;

  handle = get_proxy (obj);
  if (handle == Qnil)
    handle = make_gtkobj (obj);
  return handle;
}

int
sgtk_is_a_gtkobj (guint type, repv obj)
{
  if (!GTKOBJP (obj)
      || !GTK_IS_OBJECT (GTKOBJ_PROXY(obj)->obj)
      || GTK_OBJECT_DESTROYED (GTKOBJ_PROXY(obj)->obj))
    {
      return 0;
    }

  return gtk_type_is_a (GTK_OBJECT_TYPE(GTKOBJ_PROXY(obj)->obj), type);
}

GtkObject*
sgtk_get_gtkobj (repv obj)
{
  if (obj == Qnil)
    return NULL;
  else
    return GTKOBJ_PROXY(obj)->obj;
}


/* Enums.

   Enumerations are described by a `sgtk_enum_info' structure.  That
   structure contains a list of all literals and their respective
   values.  In Scheme, an enum element is represented by a symbol
   whose name is the literal. */

int
sgtk_valid_enum (repv obj, sgtk_enum_info *info)
{
  int i;
  char *obj_name;

  if (!rep_SYMBOLP (obj))
    return 0;

  obj_name = rep_STR(rep_SYM(obj)->name);
  for (i = 0; i < info->n_literals; i++)
    if (!strcmp (info->literals[i].name, obj_name))
      return 1;
  return 0;
}

repv
sgtk_enum_to_rep (gint val, sgtk_enum_info *info)
{
  int i;
  for (i = 0; i < info->n_literals; i++)
    if (info->literals[i].value == val)
      return Fintern (rep_string_dup(info->literals[i].name), Qnil);
#if 0
  /* XXX */
  SCM_ASSERT (0, SCM_MAKINUM (val), SCM_ARG1, "enum->symbol");
#endif
  return Qnil;
}

gint
sgtk_rep_to_enum (repv obj, sgtk_enum_info *info)
{
  int i;
  char *obj_name = rep_STR(rep_SYM(obj)->name);
  for (i = 0; i < info->n_literals; i++)
    if (!strcmp (info->literals[i].name, obj_name))
      return info->literals[i].value;
  return -1;
}


/* Flags.

   Like enums, flags are described by a `sgtk_enum_info' structure.
   In Scheme, flags are represented by a list of symbols, one for each
   bit that is set in the flags value. */

int
sgtk_valid_flags (repv obj, sgtk_enum_info *info)
{
  while (obj != Qnil)
    {
      int i, valid;
      repv sym;
      char *sym_name;
      
      if (!rep_CONSP (obj))
	return 0;
      sym = rep_CAR (obj);
      if (!rep_SYMBOLP (sym))
	return 0;
      
      sym_name = rep_STR(rep_SYM(sym)->name);
      for (i = 0, valid = 0; i < info->n_literals; i++)
	if (!strcmp (info->literals[i].name, sym_name))
	  {
	    valid = 1;
	    break;
	  }
      if (!valid)
	return 0;

      obj = rep_CDR (obj);
    }
  
  return 1;
}

repv
sgtk_flags_to_rep (gint val, sgtk_enum_info *info)
{
  repv ans = Qnil;
  int i;
  for (i = 0; i < info->n_literals; i++)
    if (val & info->literals[i].value)
      {
	ans = Fcons (Fintern (rep_string_dup(info->literals[i].name), Qnil),
		     ans);
	val &= ~info->literals[i].value;
      }
  return ans;
}

gint
sgtk_rep_to_flags (repv obj, sgtk_enum_info *info)
{
  int ans = 0;

  while (rep_CONSP(obj) && !rep_INTERRUPTP)
    {
      int i;
      repv sym = rep_CAR (obj);
      char *sym_name = rep_STR(rep_SYM(sym)->name);

      for (i = 0; i < info->n_literals; i++)
	if (!strcmp (info->literals[i].name, sym_name))
	  {
	    ans |= info->literals[i].value;
	    break;
	  }
      obj = rep_CDR (obj);
      rep_TEST_INT;
    }
  
  return ans;
}


/* String enums.

   A string enum is like an enum, but the values are strings.  The
   range of values can be extended, so anywhere a "string enum" value
   is accepted, we also accept a string (but not a symbol).  */

int
sgtk_valid_senum (repv obj, sgtk_senum_info *info)
{
  int i;
  char *obj_name;

  if (rep_STRINGP (obj))
    return 1;
  if (! rep_SYMBOLP (obj))
    return 0;

  obj_name = rep_STR(rep_SYM(obj)->name);
  for (i = 0; i < info->n_literals; i++)
    if (! strcmp (info->literals[i].name, obj_name))
      return 1;
  return 0;
}

repv
sgtk_senum_to_rep (char *val, sgtk_senum_info *info)
{
  int i;
  for (i = 0; i < info->n_literals; i++)
    if (! strcmp (info->literals[i].value, val))
      return Fintern (rep_string_dup(info->literals[i].name), Qnil);
  return rep_string_dup (val);
}

char *
sgtk_rep_to_senum (repv obj, sgtk_senum_info *info)
{
  int i;
  char *obj_name;

  if (rep_STRINGP (obj))
    return rep_STR (obj);

  obj_name = rep_STR (rep_SYM (obj)->name);
  for (i = 0; i < info->n_literals; i++)
    if (! strcmp (info->literals[i].name, obj_name))
      return info->literals[i].value;
  return NULL;
}



/* Boxed Values.

   I'm trying to use the same hash table approach as with the gtkobj's,
   but without such complex gc tracing. I'm hoping that the `opaqueness'
   of the boxed types preclude any internal pointers..  --jsh

 */

typedef struct _sgtk_boxed_proxy {
  repv car;
  struct _sgtk_boxed_proxy *next;
  guint seqno;
  gpointer ptr;
} sgtk_boxed_proxy;

static sgtk_boxed_proxy *all_boxed;

static long tc16_boxed;

#define BOXED_P(x)     (rep_CELL16_TYPEP(x, tc16_boxed))
#define BOXED_PROXY(x) ((sgtk_boxed_proxy *)rep_PTR(x))
#define BOXED_SEQNO(x) (BOXED_PROXY(x)->seqno)
#define BOXED_PTR(x)   (BOXED_PROXY(x)->ptr)
#define BOXED_INFO(x)  ((sgtk_boxed_info*)must_get_type_info(BOXED_SEQNO(x)))

static void
boxed_free (repv obj)
{
  sgtk_boxed_info *info = BOXED_INFO (obj);
  info->destroy (BOXED_PTR (obj));
  forget_proxy (BOXED_PTR (obj));
  rep_FREE_CELL (rep_PTR(obj));
}

static void
boxed_print (repv stream, repv exp)
{
  char buf[32];
  sgtk_boxed_info *info = BOXED_INFO (exp);
  rep_stream_puts (stream, "#<", -1, rep_FALSE);
  rep_stream_puts (stream, info->header.name, -1, rep_FALSE);
  rep_stream_putc (stream, ' ');
  sprintf (buf, "%lx", (long)BOXED_PTR (exp));
  rep_stream_puts (stream, buf, -1, rep_FALSE);
  rep_stream_putc (stream, '>');
}

static void
boxed_sweep (void)
{
  sgtk_boxed_proxy *proxy = all_boxed;
  all_boxed = 0;
  while (proxy != 0)
  {
      sgtk_boxed_proxy *next = proxy->next;
      if (! rep_GC_CELL_MARKEDP(rep_VAL(proxy)))
	  boxed_free (rep_VAL(proxy));
      else
      {
	  rep_GC_CLR_CELL (rep_VAL(proxy));
	  proxy->next = all_boxed;
	  all_boxed = proxy;
      }
      proxy = next;
  }
}

repv
sgtk_boxed_to_rep (gpointer ptr, sgtk_boxed_info *info, int copyp)
{
  repv handle;

  if (ptr == NULL)
    return Qnil;

  if (!sgtk_fillin_type_info (&info->header))
    return Qnil;

  handle = get_proxy (ptr);
  if (handle == Qnil) {
      /* Allocate a new proxy */
      sgtk_boxed_proxy *p = rep_ALLOC_CELL (sizeof (sgtk_boxed_proxy));
      if (copyp)
	  ptr = info->copy (ptr);
      if (GTK_TYPE_SEQNO(info->header.type) > 0xFFFF)
	  abort ();
      p->car = tc16_boxed;
      p->next = all_boxed;
      all_boxed = p;
      p->seqno = GTK_TYPE_SEQNO(info->header.type);
      p->ptr = ptr;
      handle = rep_VAL(p);
  }
  return handle;
}

void *
sgtk_rep_to_boxed (repv obj)
{
  if (obj == Qnil)
    return NULL;
  return BOXED_PTR (obj);
}

int
sgtk_valid_boxed (repv obj, sgtk_boxed_info *info)
{
  return (BOXED_P (obj) && BOXED_INFO (obj) == info);
}

int
sgtk_valid_point (repv obj)
{
  return (rep_CONSP (obj)
	  && rep_INTP (rep_CAR (obj))    /* too permissive */
	  && rep_INTP (rep_CDR (obj)));  /* too permissive */
}

GdkPoint
sgtk_rep_to_point (repv obj)
{
  GdkPoint res;
  res.x = rep_INT (rep_CAR (obj));
  res.y = rep_INT (rep_CDR (obj));
  return res;
}

repv
sgtk_point_to_rep (GdkPoint p)
{
  return Fcons (sgtk_int_to_rep (p.x),
		sgtk_int_to_rep (p.y));
}

int
sgtk_valid_rect (repv obj)
{
  return rep_CONSP (obj)
    && sgtk_valid_point (rep_CAR (obj))
    && sgtk_valid_point (rep_CDR (obj));
}

GdkRectangle
sgtk_rep_to_rect (repv obj)
{
  GdkRectangle res;
  res.x = rep_INT (rep_CAAR (obj));
  res.y = rep_INT (rep_CDAR (obj));
  res.width = rep_INT (rep_CADR (obj));
  res.height = rep_INT (rep_CDDR (obj));
  return res;
}

repv
sgtk_rect_to_rep (GdkRectangle r)
{
  return Fcons (Fcons (rep_MAKE_INT (r.x),
		       rep_MAKE_INT (r.y)),
		Fcons (rep_MAKE_INT (r.width),
		       rep_MAKE_INT (r.height)));
}



/* GtkType objects

   I'm going to be lazy and try to store these in rep's 30-bit
   signed integers, let's see if it works...  --jsh */

#define GTKTYPEP(x)     (rep_INTP(x))
#define GTKTYPE(x)      ((GtkType)rep_INT(x))

GtkType
sgtk_type_from_name (char *name)
{
  GtkType type = gtk_type_from_name (name);
  if (type == GTK_TYPE_INVALID)
    {
      sgtk_object_info *info = sgtk_find_object_info (name);
      if (info)
	type = info->header.type;
    }
  return type;
}

int
sgtk_valid_type (repv obj)
{
  return (obj == Qnil || GTKTYPEP (obj)
	  || (rep_SYMBOLP (obj)
	      && sgtk_type_from_name (rep_STR(rep_SYM(obj)->name))));
}

GtkType
sgtk_rep_to_type (repv obj)
{
  if (obj == Qnil)
    return GTK_TYPE_INVALID;
  else if (GTKTYPEP (obj))
    return GTKTYPE (obj);
  else {
      if (rep_SYMBOLP(obj))
	  obj = rep_SYM(obj)->name;

      if (rep_STRINGP(obj))
	  return sgtk_type_from_name (rep_STR (obj));
      else
	  return GTK_TYPE_INVALID;
  }
}

repv
sgtk_type_to_rep (GtkType t)
{
  if (t == GTK_TYPE_INVALID)
    return Qnil;

  assert (t <= rep_LISP_MAX_INT);

  return sgtk_uint_to_rep (t);
}


/* converting between SCM and GtkArg */

repv
sgtk_arg_to_rep (GtkArg *a, int free_mem)
{
  switch (GTK_FUNDAMENTAL_TYPE (a->type))
    {
    case GTK_TYPE_NONE:
      return Qnil;
    case GTK_TYPE_CHAR:
      return rep_MAKE_INT (GTK_VALUE_CHAR(*a));
    case GTK_TYPE_BOOL:
      return GTK_VALUE_BOOL(*a)? Qt : Qnil;
    case GTK_TYPE_INT:
      return sgtk_int_to_rep (GTK_VALUE_INT(*a));
    case GTK_TYPE_UINT:
      return sgtk_uint_to_rep (GTK_VALUE_UINT(*a));
    case GTK_TYPE_LONG:
      return sgtk_int_to_rep (GTK_VALUE_LONG(*a));
    case GTK_TYPE_ULONG:
      return sgtk_uint_to_rep (GTK_VALUE_ULONG(*a));
    case GTK_TYPE_FLOAT:
      return sgtk_float_to_rep (GTK_VALUE_FLOAT(*a));
    case GTK_TYPE_DOUBLE:
      return sgtk_double_to_rep (GTK_VALUE_DOUBLE(*a));
    case GTK_TYPE_STRING:
      {
	repv ret = rep_string_dup (GTK_VALUE_STRING(*a));
	if (free_mem)
	  g_free GTK_VALUE_STRING(*a);
	return ret;
      }
    case GTK_TYPE_ENUM:
      return sgtk_enum_to_rep (GTK_VALUE_FLAGS(*a),
			       (sgtk_enum_info *)sgtk_find_type_info (a->type));
    case GTK_TYPE_FLAGS:
      return sgtk_flags_to_rep (GTK_VALUE_FLAGS(*a),
				(sgtk_enum_info *)sgtk_find_type_info (a->type));
    case GTK_TYPE_BOXED:
      return sgtk_boxed_to_rep (GTK_VALUE_BOXED(*a),
				(sgtk_boxed_info *)sgtk_find_type_info (a->type),
				TRUE);
    case GTK_TYPE_POINTER:
      return sgtk_pointer_to_rep (GTK_VALUE_POINTER(*a));
    case GTK_TYPE_OBJECT:
      return sgtk_wrap_gtkobj (GTK_VALUE_OBJECT(*a));
    default:
      fprintf (stderr, "illegal type %s in arg\n", 
	       gtk_type_name (a->type));
      return Qnil;
    }
}

int
sgtk_valid_arg (GtkArg *a, repv obj)
{
  switch (GTK_FUNDAMENTAL_TYPE (a->type))
    {
    case GTK_TYPE_NONE:
      return TRUE;
    case GTK_TYPE_CHAR:
      return sgtk_valid_char(obj);
    case GTK_TYPE_BOOL:
      return TRUE;
    case GTK_TYPE_INT:
    case GTK_TYPE_UINT:
    case GTK_TYPE_LONG:
    case GTK_TYPE_ULONG:
      return sgtk_valid_int (obj);
    case GTK_TYPE_FLOAT:
    case GTK_TYPE_DOUBLE:
      return sgtk_valid_float (obj);
    case GTK_TYPE_STRING:
      return rep_STRINGP (obj);
    case GTK_TYPE_ENUM:
      return sgtk_valid_enum (obj, ((sgtk_enum_info *)
				    sgtk_find_type_info (a->type)));
    case GTK_TYPE_FLAGS:
      return sgtk_valid_flags (obj, ((sgtk_enum_info *)
				     sgtk_find_type_info (a->type)));
    case GTK_TYPE_BOXED:
      return sgtk_valid_boxed (obj, ((sgtk_boxed_info *)
				     sgtk_find_type_info (a->type)));
      break;
    case GTK_TYPE_POINTER:
      return BOXED_P (obj) || GTKOBJP (obj) || sgtk_valid_pointer (obj);
      break;
    case GTK_TYPE_CALLBACK:
      return sgtk_valid_function (obj);
    case GTK_TYPE_OBJECT:
      return sgtk_is_a_gtkobj (a->type, obj);
    default:
      fprintf (stderr, "unhandled arg type %s\n", gtk_type_name (a->type));
      return FALSE;
    }
}

void
sgtk_rep_to_arg (GtkArg *a, repv obj, repv protector)
{
  switch (GTK_FUNDAMENTAL_TYPE (a->type))
    {
    case GTK_TYPE_NONE:
      return;
    case GTK_TYPE_CHAR:
      GTK_VALUE_CHAR(*a) = rep_INT (obj);
      break;
    case GTK_TYPE_BOOL:
      GTK_VALUE_BOOL(*a) = obj != Qnil;
      break;
    case GTK_TYPE_INT:
      GTK_VALUE_INT(*a) = sgtk_rep_to_int (obj);
      break;
    case GTK_TYPE_UINT:
      GTK_VALUE_UINT(*a) = sgtk_rep_to_uint (obj);
      break;
    case GTK_TYPE_LONG:
      GTK_VALUE_LONG(*a) = sgtk_rep_to_long (obj);
      break;
    case GTK_TYPE_ULONG:
      GTK_VALUE_ULONG(*a) = sgtk_rep_to_ulong (obj);
      break;
    case GTK_TYPE_FLOAT:
      GTK_VALUE_FLOAT(*a) = sgtk_rep_to_float (obj);
      break;
    case GTK_TYPE_DOUBLE:
      GTK_VALUE_DOUBLE(*a) = sgtk_rep_to_double (obj);
      break;
    case GTK_TYPE_STRING:
      GTK_VALUE_STRING(*a) = sgtk_rep_to_string (obj);
      break;
    case GTK_TYPE_ENUM:
      GTK_VALUE_ENUM(*a) =
	sgtk_rep_to_enum (obj, (sgtk_enum_info *)sgtk_find_type_info (a->type));
      break;
    case GTK_TYPE_FLAGS:
      GTK_VALUE_ENUM(*a) =
	sgtk_rep_to_flags (obj, (sgtk_enum_info *)sgtk_find_type_info (a->type));
      break;
    case GTK_TYPE_BOXED:
      GTK_VALUE_BOXED(*a) = sgtk_rep_to_boxed (obj);
      break;
    case GTK_TYPE_POINTER:
      if (BOXED_P (obj))
	  GTK_VALUE_POINTER(*a) = BOXED_PTR (obj);
      else if (GTKOBJP (obj))
	  GTK_VALUE_POINTER(*a) = GTKOBJ_PROXY (obj)->obj;
      else
	  GTK_VALUE_POINTER(*a) = sgtk_rep_to_pointer (obj);
      break;
    case GTK_TYPE_CALLBACK:
      sgtk_protect (protector, obj);
      GTK_VALUE_CALLBACK(*a).marshal = sgtk_callback_marshal;
      GTK_VALUE_CALLBACK(*a).data = (gpointer)obj;
      GTK_VALUE_CALLBACK(*a).notify = sgtk_callback_destroy;
      break;
    case GTK_TYPE_OBJECT:
      GTK_VALUE_OBJECT(*a) = sgtk_get_gtkobj (obj);
      break;
    default:
      fprintf (stderr, "unhandled arg type %s\n", gtk_type_name (a->type));
      break;
    }
}

void
sgtk_rep_to_ret (GtkArg *a, repv obj)
{
  switch (GTK_FUNDAMENTAL_TYPE (a->type))
    {
    case GTK_TYPE_NONE:
      return;
    case GTK_TYPE_CHAR:
      *GTK_RETLOC_CHAR(*a) = rep_INT (obj);
      break;
    case GTK_TYPE_BOOL:
      *GTK_RETLOC_BOOL(*a) = (obj != Qnil);
      break;
    case GTK_TYPE_INT:
      *GTK_RETLOC_INT(*a) = sgtk_rep_to_int (obj);
      break;
    case GTK_TYPE_UINT:
      *GTK_RETLOC_UINT(*a) = sgtk_rep_to_uint (obj);
      break;
    case GTK_TYPE_LONG:
      *GTK_RETLOC_LONG(*a) = sgtk_rep_to_long (obj);
      break;
    case GTK_TYPE_ULONG:
      *GTK_RETLOC_ULONG(*a) = sgtk_rep_to_ulong (obj);
      break;
    case GTK_TYPE_FLOAT:
      *GTK_RETLOC_FLOAT(*a) = sgtk_rep_to_float (obj);
      break;
    case GTK_TYPE_DOUBLE:
      *GTK_RETLOC_DOUBLE(*a) = sgtk_rep_to_double (obj);
      break;
    case GTK_TYPE_STRING:
      GTK_VALUE_STRING(*a) = g_strdup (rep_STR(obj));
      break;
    case GTK_TYPE_ENUM:
      *GTK_RETLOC_ENUM(*a) =
	sgtk_rep_to_enum (obj, (sgtk_enum_info *)sgtk_find_type_info (a->type));
      break;
    case GTK_TYPE_FLAGS:
      *GTK_RETLOC_ENUM(*a) =
	sgtk_rep_to_flags (obj, (sgtk_enum_info *)sgtk_find_type_info (a->type));
      break;
    case GTK_TYPE_BOXED:
      *GTK_RETLOC_BOXED(*a) = sgtk_rep_to_boxed (obj);
      break;
    case GTK_TYPE_OBJECT:
      if (sgtk_is_a_gtkobj (a->type, obj))
	  *GTK_RETLOC_OBJECT(*a) = sgtk_get_gtkobj (obj);
      else
	  *GTK_RETLOC_OBJECT(*a) = NULL;
      break;
    default:
      fprintf (stderr, "unhandled return type %s\n", gtk_type_name (a->type));
      break;
    }
}



/* Callbacks.

   Callbacks are executed within a new dynamic root.  That means that
   the flow of control can't leave them without Gtk noticing.  Throws
   are catched and briefly reported.  Calls to continuations that have
   been made outside the dynamic root can not be activated.

   Callbacks are invoked with whatever arguments that are specified by
   the Gtk documentation.  They do not, however, receive the GtkObject
   that has initiated the callback.

   [ actually, they do receive the GtkObject. For rep, there are no
     closures, so without the invoking object it's usually necessary
     to build ad hoc closures through backquoting..  --jsh ]

   When callback_trampoline is non-#f, we treat it as a procedure and
   call it as

      (trampoline proc args)

   PROC is the real callback procedure and ARGS is the list of
   arguments that should be passed to it.  */

static repv callback_trampoline;

DEFUN ("gtk-callback-trampoline", Fgtk_callback_trampoline,
       Sgtk_callback_trampoline, (repv new), rep_Subr1)
{
  repv old = rep_CAR (callback_trampoline);
  if (new != Qnil)
    rep_CAR (callback_trampoline) = new;
  return old;
}

/* Be carefull when this macro is true.
   scm_gc_heap_lock is set during gc.  */
#define rep_GC_P (rep_in_gc)

struct callback_info {
  GtkObject *obj;
  repv proc;
  gint n_args;
  GtkArg *args;
};

static repv
inner_callback_marshal (repv data)
{
  struct callback_info *info = (struct callback_info *) rep_PTR (data);
  int i;
  repv args = Qnil, ans;

  for (i = info->n_args-1; i >= 0; i--)
    args = Fcons (sgtk_arg_to_rep (info->args+i, 0), args);
  args = Fcons (sgtk_wrap_gtkobj (info->obj), args);

  if (rep_CAR(callback_trampoline) == Qnil)
    ans = rep_funcall (info->proc, args, rep_FALSE);
  else
    ans = rep_funcall (rep_CAR(callback_trampoline),
		       Fcons (info->proc, Fcons (args, Qnil)), rep_FALSE);

  if (info->args[info->n_args].type != GTK_TYPE_NONE)
    sgtk_rep_to_ret (info->args+info->n_args, ans);

  return Qnil;
}

void
sgtk_callback_marshal (GtkObject *obj,
		       gpointer data,
		       guint n_args,
		       GtkArg *args)
{
  struct callback_info info;

  if (rep_GC_P)
    {
      /* This should only happen for the "destroy" signal and is then
         harmless. */
      fprintf (stderr, "callback ignored during GC!\n");
      return;
    }
  
  info.obj = obj;
  info.proc = ((sgtk_protshell *)data)->object;
  info.n_args = n_args;
  info.args = args;

  rep_call_with_barrier (inner_callback_marshal,
			 rep_VAL(&info), rep_TRUE, 0, 0, 0);

  sgtk_callback_postfix ();
}

void
sgtk_callback_destroy (gpointer data)
{
  sgtk_unprotect ((sgtk_protshell *)data);
}



/* Type conversions */

extern sgtk_boxed_info sgtk_gdk_color_info;

repv
sgtk_color_conversion (repv color)
{
  repv orig_color = color;

  if (rep_STRINGP (color))
    {
      GdkColor colstruct;
      GdkColormap *colmap;

      if (!gdk_color_parse (rep_STR (color), &colstruct))
	{
	  Fsignal (Qerror, rep_list_2 (rep_string_dup ("no such color"),
				       orig_color));
	  return Qnil;
	}
      colmap = gtk_widget_peek_colormap ();
      if (!gdk_color_alloc (colmap, &colstruct))
	{
	  Fsignal (Qerror, rep_list_2 (rep_string_dup ("can't allocate color"),
				       orig_color));
	  return Qnil;
	}
      return sgtk_boxed_to_rep (&colstruct, &sgtk_gdk_color_info, 1);
    }
  return color;
}

extern repv Fgdk_fontset_load (repv font);

repv
sgtk_font_conversion (repv font)
{
  repv orig_font = font;

  if (rep_STRINGP (font))
    {
      font = Fgdk_fontset_load (font);
      if (font == Qnil)
	Fsignal (Qerror, rep_list_2 (rep_string_dup ("no such font: "),
				     orig_font));
    }
  return font;
}



/* Support for gtk_object_new, gtk_object_set, ... */

/* The SCM_PROC for the exported functions is in gtk-support.c to have
   it be snarfed for sgtk_init_gtk_support. */

sgtk_object_info *sgtk_find_object_info (char *name);

sgtk_object_info *
sgtk_find_object_info_from_type (GtkType type)
{
  sgtk_object_info *info;
  if (type == GTK_TYPE_INVALID)
    return NULL;
  info = (sgtk_object_info *)sgtk_get_type_info (GTK_TYPE_SEQNO(type));
  if (info)
    return info;
  
  return sgtk_find_object_info (gtk_type_name (type));
}

sgtk_object_info *
sgtk_find_object_info (char *name)
{
  GtkType type, parent;
  sgtk_object_info *info;
  type_infos *infos;
  int i;

  type = gtk_type_from_name (name);
  if (type != GTK_TYPE_INVALID)
    {
      info = (sgtk_object_info *)sgtk_get_type_info (GTK_TYPE_SEQNO(type));
      if (info)
	return info;
    }

  for (infos = all_type_infos; infos; infos = infos->next)
    {
      sgtk_type_info **ip;
      for (ip = infos->infos; *ip; ip++)
	if (!strcmp ((*ip)->name, name))
	  {
	    if (GTK_FUNDAMENTAL_TYPE((*ip)->type) != GTK_TYPE_OBJECT)
	      return NULL;

	    info = (sgtk_object_info *)*ip;
	    info->header.type = info->init_func ();
	    enter_type_info ((sgtk_type_info*)info);
	    goto query_args;
	  }
    }

  /* Not found among our precompiled types.  Construct a fresh
     sgtk_object_info, if it's known to Gtk+. */

  if (type != GTK_TYPE_INVALID)
    {
      fprintf (stderr, "Fresh info for %s, %d\n", name, type);

      info = (sgtk_object_info *)rep_alloc (sizeof(sgtk_object_info));
      info->header.type = type;
      info->header.name = name;
      info->init_func = NULL;
      enter_type_info ((sgtk_type_info*)info);
    }
  else
    return NULL;

 query_args:
  gtk_type_class (info->header.type);
  info->args = gtk_object_query_args (info->header.type,
				      &info->args_flags,
				      &info->n_args);
  info->args_short_names = (char **)rep_alloc (info->n_args*(sizeof(char*)));
  for (i = 0; i < info->n_args; i++)
    {
      char *l = info->args[i].name;
      char *d = strchr (l, ':');
      if (d == NULL || d[1] != ':')
	{
	  fprintf (stderr, "`%s' has no class part.\n", l);
	  info->args_short_names[i] = l;
	}
      else
	info->args_short_names[i] = d+2;
    }
  
  parent = gtk_type_parent (info->header.type);
  if (parent != GTK_TYPE_INVALID)
    info->parent = sgtk_find_object_info_from_type (parent);
  else
    info->parent = NULL;
  
  return info;
}

#ifdef NEED_UNUSED_CODE
static char*
xstrndup (char *str, int n)
{
  char *dup;

  if (str == NULL)
    return NULL;
  dup = rep_alloc (n+1, "xstrndup");
  strncpy (dup, str, n);
  dup[n] = '\0';
  return dup;
}
#endif

static void
sgtk_find_arg_info (GtkArg *arg, sgtk_object_info *info, char *name)
{
  /* XXX - handle signal handlers.  Do not use '::', use '.' instead. */

  char *d = strchr (name, ':');
  if (d && d[1] == ':')
    {
      /* A long name.  Find the object_info for the class part. */
      int len = d-name;

      while (info)
	{
	  if (info->header.name[len] == '\0'
	      && !strncmp (info->header.name, name, len))
	    break;
	  info = info->parent;
	}
      name = d+2;
    }
  
#ifdef DEBUG_PRINT
  fprintf (stderr, "searching short `%s'\n", name);
#endif
  while (info)
    {
      int i;
      for (i = 0; i < info->n_args; i++)
	{
#ifdef DEBUG_PRINT
	  fprintf (stderr, " on %s\n", info->args[i].name);
#endif
	  if (!strcmp (info->args_short_names[i], name))
	    {
	      *arg = info->args[i];
	      return;
	    }
	}
      info = info->parent;
    }
  
  arg->type = GTK_TYPE_INVALID;
  return;
}
      
GtkArg*
sgtk_build_args (sgtk_object_info *info, int *n_argsp, repv scm_args,
		 repv protector, char *subr)
{
  int i, n_args = *n_argsp;
  GtkArg *args;
  char *name;
  repv kw, val;
  sgtk_type_info *type_info;

  args = g_new0 (GtkArg, n_args);

  for (i = 0; i < n_args; i++)
    {
      kw = rep_CAR (scm_args);
      val = rep_CADR (scm_args);
      scm_args = rep_CDDR (scm_args);

      if (rep_SYMBOLP (kw))
	name = rep_STR(rep_SYM(kw)->name);
      else
	{
	  fprintf (stderr, "bad keyword\n");
	  n_args -= 1;
	  i -= 1;
	  continue;
	}

      sgtk_find_arg_info (&args[i], info, name);
      if (args[i].type == GTK_TYPE_INVALID)
	{
	  fprintf (stderr, "no such arg for type `%s': %s\n",
		   info->header.name, name);
	  n_args -= 1;
	  i -= 1;
	  continue;
	}

      /* XXX - rethink type info business.  Avoid double lookups. */

      type_info = sgtk_maybe_find_type_info (args[i].type);
      if (type_info && type_info->conversion)
	val = type_info->conversion (val);

      if (!sgtk_valid_arg (&args[i], val))
	{
	  repv throw_args = 
	    rep_LIST_3 (rep_string_dup ("wrong type for"),
			rep_string_dup (gtk_type_name (args[i].type)), val);
	  g_free (args);
	  Fsignal (Qerror, throw_args);
	}
	  
      sgtk_rep_to_arg (&args[i], val, protector);
    }

  *n_argsp = n_args;
  return args;
}

DEFUN("gtk-object-new", Fgtk_object_new, Sgtk_object_new,
      (repv type_obj, repv scm_args), rep_Subr2)
{
  int n_args;
  sgtk_object_info *info;
  GtkArg *args;
  GtkObject *obj;
  repv scm_obj;

  rep_DECLARE (1, type_obj, type_obj != Qnil && sgtk_valid_type (type_obj));
  n_args = list_length (scm_args);
  rep_DECLARE (2, scm_args, n_args >= 0 && (n_args%2) == 0);
  n_args = n_args/2;

  info = sgtk_find_object_info_from_type (sgtk_rep_to_type (type_obj));
  if (info == 0)
      return Qnil;

  obj = gtk_object_new (info->header.type, NULL);
  scm_obj = sgtk_wrap_gtkobj (obj);
  args = sgtk_build_args (info, &n_args, scm_args, scm_obj, "gtk-object-new");
  gtk_object_setv (obj, n_args, args);
  g_free (args);

  return scm_obj;
}

DEFUN("gtk-object-set", Fgtk_object_set, Sgtk_object_set,
      (repv scm_obj, repv scm_args), rep_Subr2)
{
  int n_args;
  sgtk_object_info *info;
  GtkArg *args;
  GtkObject *obj;

  rep_DECLARE (1, scm_obj, GTKOBJP(scm_obj));
  n_args = list_length (scm_args);
  rep_DECLARE (2, scm_args, n_args >= 0 && (n_args%2) == 0);
  n_args = n_args/2;

  obj = GTKOBJ_PROXY(scm_obj)->obj;
  info = sgtk_find_object_info_from_type (GTK_OBJECT_TYPE(obj));
  if (info == 0)
      return Qnil;
  
  args = sgtk_build_args (info, &n_args, scm_args, scm_obj, "gtk-object-set");
  gtk_object_setv (obj, n_args, args);
  g_free (args);

  return Qnil;
}

DEFUN ("gtk-object-get", Fgtk_object_get, Sgtk_object_get,
       (repv scm_obj, repv argsym), rep_Subr2)
{
  GtkObject *obj;
  sgtk_object_info *info;
  char *name;
  GtkArg arg;

  rep_DECLARE (1, scm_obj, GTKOBJP(scm_obj));
  rep_DECLARE (2, argsym, rep_SYMBOLP(argsym));

  obj = GTKOBJ_PROXY(scm_obj)->obj;
  info = sgtk_find_object_info_from_type (GTK_OBJECT_TYPE(obj));
  if (info == 0)
      return Qnil;

  name = rep_STR(rep_SYM(argsym)->name);
  sgtk_find_arg_info (&arg, info, name);

  if (arg.type != GTK_TYPE_INVALID)
    gtk_object_getv (obj, 1, &arg);

  if (arg.type == GTK_TYPE_INVALID)
    return Qnil;
  else
    return sgtk_arg_to_rep (&arg, 1);
}



/* Creating new object classes */

GtkType
gtk_class_new (GtkType parent_type, gchar *name)
{
  GtkTypeInfo info = { 0 };
  GtkTypeInfo parent_info;

  if (!gtk_type_get_info (parent_type, &parent_info))
    return GTK_TYPE_INVALID;

  info.type_name = name;
  info.object_size = parent_info.object_size;
  info.class_size = parent_info.class_size;
  info.class_init_func = NULL;
  info.object_init_func = NULL;
#if GTK_MAJOR_VERSION > 1 || GTK_MINOR_VERSION > 0
  info.base_class_init_func = NULL;
#endif

  return gtk_type_unique (parent_type, &info);
}

guint
gtk_signal_new_generic (const gchar     *name,
			GtkSignalRunType signal_flags,
			GtkType          type,
			GtkType          return_type,
			guint            nparams,
			GtkType         *params)
{
  guint signal_id;

  if (GTK_FUNDAMENTAL_TYPE (type) != GTK_TYPE_OBJECT)
    return 0;

  signal_id = gtk_signal_newv (name, signal_flags, type,
			       0, NULL,
			       return_type, nparams, params);
  if (signal_id > 0)
    gtk_object_class_add_signals (gtk_type_class (type),
				  &signal_id, 1);

  return signal_id;
}

void
sgtk_signal_emit (GtkObject *obj, char *name, repv scm_args)
{
  GtkSignalQuery *info;
  guint signal_id, i;
  GtkArg *args;

  signal_id = gtk_signal_lookup (name, GTK_OBJECT_TYPE (obj));
  if (signal_id == 0)
    {
      Fsignal (Qerror, rep_list_2 (rep_string_dup ("no such signal"),
				   rep_string_dup (name)));
      return;
    }

  info = gtk_signal_query (signal_id);
  if (!rep_CONSP(scm_args) || list_length (scm_args) != info->nparams)
    {
      g_free (info);
      Fsignal (Qerror, Fcons (rep_string_dup ("wrong number of signal arguments"), Qnil));
      return;
    }

  args = g_new (GtkArg, info->nparams+1);
  i = 0;
  while (rep_CONSP (scm_args))
    {
      args[i].name = NULL;
      args[i].type = info->params[i];

      if (!sgtk_valid_arg (&args[i], rep_CAR (scm_args)))
	{
	  repv throw_args =
	    rep_LIST_3 (rep_string_dup ("wrong type for"),
			rep_string_dup (gtk_type_name (args[i].type)),
			rep_CAR (scm_args));
	  g_free (args);
	  Fsignal (Qerror, throw_args);
	  return;
	}

      sgtk_rep_to_arg (&args[i], rep_CAR(scm_args), Qt);
      i++;
      scm_args = rep_CDR (scm_args);
    }
  args[i].type = GTK_TYPE_NONE;

  gtk_signal_emitv (obj, signal_id, args);

  g_free (args);
  g_free (info);
}


/* Support rep input handling through gtk_main */

/* The input_tags table hashes fds to gdk tags; the input_callbacks
   table hashes fds to rep callback function. These should be a single
   table really.. */
static GHashTable *input_tags, *input_callbacks;

struct input_callback_data {
    void (*func)(int);
    int fd;
};

struct timeout_data {
    struct timeout_data *next;
    int timed_out;
    int idle_counter;
    u_long this_timeout_msecs;
    u_long actual_timeout_msecs;
    int gtk_tag;
};

static struct timeout_data *context;

static repv
inner_input_callback (repv data_)
{
    struct input_callback_data *data
	= (struct input_callback_data *) rep_PTR (data_);
    (*data->func) (data->fd);
    return Qnil;
}

static void
sgtk_input_callback (gpointer data, gint fd, GdkInputCondition cond)
{
    struct input_callback_data d;
    d.func = g_hash_table_lookup (input_callbacks, GINT_TO_POINTER (fd));
    d.fd = fd;
    if (d.func != 0)
    {
	rep_call_with_barrier (inner_input_callback, rep_VAL(&d),
			       rep_TRUE, 0, 0, 0);
    }
    sgtk_callback_postfix ();
}

static void
sgtk_register_input_fd (int fd, void (*callback)(int fd))
{
    if (callback != 0)
    {
	int tag;
	if (input_tags == 0)
	{
	    input_tags = g_hash_table_new (NULL, NULL);
	    input_callbacks = g_hash_table_new (NULL, NULL);
	}
	tag = gdk_input_add (fd, GDK_INPUT_READ,
			     (GdkInputFunction) sgtk_input_callback, 0);
	g_hash_table_insert (input_tags, GINT_TO_POINTER (fd), GINT_TO_POINTER (tag));
	g_hash_table_insert (input_callbacks,
			     GINT_TO_POINTER (fd), (gpointer) callback);
    }
}

static void
sgtk_deregister_input_fd (int fd)
{
    if (input_tags != 0)
    {
	int tag = GPOINTER_TO_INT (g_hash_table_lookup (input_tags, GINT_TO_POINTER (fd)));
	gdk_input_remove (tag);
	g_hash_table_remove (input_tags, GINT_TO_POINTER (fd));
	g_hash_table_remove (input_callbacks, GINT_TO_POINTER (fd));
    }
}

static gboolean
timeout_callback (gpointer data)
{
    struct timeout_data *d = data;

    d->gtk_tag = 0;
    d->timed_out = 1;

    /* Only quit if we'd return to the correct event loop */
    if (context == d)
	gtk_main_quit ();

    return FALSE;
}

static void
unset_timeout (void)
{
    if (context != 0)
    {
	if (context->gtk_tag != 0)
	    gtk_timeout_remove (context->gtk_tag);
	context->gtk_tag = 0;
    }
}

static void
set_timeout (void)
{
    if (context != 0 && !context->timed_out && !context->gtk_tag)
    {
	u_long max_sleep = rep_max_sleep_for ();
	context->this_timeout_msecs = rep_input_timeout_secs * 1000;
	context->actual_timeout_msecs = MIN (context->this_timeout_msecs,
					     max_sleep);
	context->gtk_tag = gtk_timeout_add (context->actual_timeout_msecs,
					    timeout_callback,
					    (gpointer) context);
    }
}

/* Call this after executing any callbacks that could invoke Lisp code */
void
sgtk_callback_postfix (void)
{
    unset_timeout ();
    if (rep_INTERRUPTP && gtk_main_level () > 0)
	gtk_main_quit ();
    else if (rep_redisplay_fun != 0)
	(*rep_redisplay_fun)();
    if (context != 0)
    {
	context->timed_out = 0;
	set_timeout ();
	context->idle_counter = 0;
    }
}

/* This function replaces the standard rep event loop. */
static repv
sgtk_event_loop (void)
{
    struct timeout_data data;

    data.idle_counter = 0;
    data.gtk_tag = 0;
    data.next = context;
    context = &data;

    while (1)
    {
	u_long max_sleep = rep_max_sleep_for ();

	if (rep_redisplay_fun != 0)
	    (*rep_redisplay_fun)();

	if (max_sleep == 0)
	{
	    while (gtk_events_pending ())
		gtk_main_iteration_do (FALSE);
	    Fthread_yield ();
	}
	else
	{
	    data.timed_out = 0;
	    set_timeout ();
	    gtk_main ();
	    unset_timeout ();
	    if (data.timed_out)
	    {
		if (data.actual_timeout_msecs < data.this_timeout_msecs)
		{
		    Fthread_suspend (Qnil, rep_MAKE_INT (data.this_timeout_msecs
							 - data.actual_timeout_msecs));
		}
		else
		    rep_on_idle (data.idle_counter++);
	    }
	}

	rep_proc_periodically ();

	/* Check for exceptional conditions. */
	if(rep_throw_value != rep_NULL)
	{
	    repv result;
	    if(rep_handle_input_exception (&result))
	    {
		context = data.next;
		/* reset the timeout for any containing event loop */
		set_timeout ();
		return result;
	    }
	}

#ifdef C_ALLOCA
	/* Using the C implementation of alloca. So garbage collect
	   anything below the current stack depth. */
	alloca(0);
#endif
    }
}

/* Called by librep/src/unix_processes.c whenever SIGCHLD is received
   (from the signal handler) */
static void
sgtk_sigchld_callback (void)
{
    /* XXX I'm hoping that this is safe to call from a signal handler... */

    if (gtk_main_level () > 0)
	gtk_main_quit ();
}



/* Initialization */

static int standalone_p = 1;

void
sgtk_set_standalone (int flag)
{
  standalone_p = flag;
}

int
sgtk_is_standalone (void)
{
  return standalone_p;
}

DEFUN ("gtk-standalone-p", Fgtk_standalone_p,
       Sgtk_standalone_p, (void), rep_Subr0)
{
  return standalone_p ? Qt : Qnil;
}

DEFSYM (gtk_major_version, "gtk-major-version");
DEFSYM (gtk_minor_version, "gtk-minor-version");
DEFSYM (gtk_micro_version, "gtk-micro-version");
DEFSYM (rep_gtk_version, "rep-gtk-version");

static void
sgtk_init_substrate (void)
{
  DEFSTRING (ver, REP_GTK_VERSION);

  tc16_gtkobj = rep_register_new_type ("gtk-object", 0,
				       gtkobj_print, gtkobj_print,
				       gtkobj_sweep, gtkobj_mark,
				       gtkobj_marker_hook,
				       0, 0, 0, 0, 0, 0);

  tc16_boxed = rep_register_new_type ("gtk-boxed", 0,
				      boxed_print, boxed_print,
				      boxed_sweep, 0, 0,
				      0, 0, 0, 0, 0, 0);

  global_protects = NULL;
  sgtk_protshell_chunk = g_mem_chunk_create (sgtk_protshell, 128,
					     G_ALLOC_AND_FREE);
  
  callback_trampoline = Fcons (Qnil, Qnil);
  rep_mark_static (&callback_trampoline);

  rep_register_input_fd_fun = sgtk_register_input_fd;
  rep_deregister_input_fd_fun = sgtk_deregister_input_fd;
  rep_map_inputs (sgtk_register_input_fd);
  rep_event_loop_fun = sgtk_event_loop;
  rep_sigchld_fun = sgtk_sigchld_callback;

  /* Need this in case sit-for is called. */
  if (gdk_display != 0)
      rep_register_input_fd (ConnectionNumber (gdk_display), 0);

  rep_ADD_SUBR (Sgtk_callback_trampoline);
  rep_ADD_SUBR (Sgtk_standalone_p);
  rep_ADD_SUBR (Sgtk_object_new);
  rep_ADD_SUBR (Sgtk_object_set);
  rep_ADD_SUBR (Sgtk_object_get);
  rep_INTERN (gtk_major_version);
  rep_INTERN (gtk_minor_version);
  rep_INTERN (gtk_micro_version);
  rep_INTERN (rep_gtk_version);
  Fset (Qgtk_major_version, rep_MAKE_INT (GTK_MAJOR_VERSION));
  Fset (Qgtk_minor_version, rep_MAKE_INT (GTK_MINOR_VERSION));
  Fset (Qgtk_micro_version, rep_MAKE_INT (GTK_MICRO_VERSION));
  Fset (Qrep_gtk_version, rep_VAL (&ver));
  Fexport_bindings (rep_list_4 (Qgtk_major_version,
				Qgtk_minor_version,
				Qgtk_micro_version,
				Qrep_gtk_version));
}

static int sgtk_inited = 0;

void
sgtk_init_with_args (int *argcp, char ***argvp)
{
  if (sgtk_inited)
    return;

  /* XXX - Initialize Gtk only once.  We assume that Gtk has already
     been initialized when Gdk has.  That is not completely correct,
     but the best I can do.

     Actually it shouldn't matter, gtk_init () won't initialise more
     than once.. --jsh */

  if (gdk_display == NULL)
    {
      char *tem = getenv ("REP_GTK_DONT_INITIALIZE");
      if (tem == 0 || atoi (tem) == 0)
        {
	  gtk_set_locale ();
	  gtk_init (argcp, argvp);

#ifdef HAVE_SETLOCALE
	  /* XXX remove when no longer needed.. */
	  setlocale (LC_NUMERIC, "C");
#endif
        }
    }

  if (rep_recurse_depth >= 0)
    standalone_p = 0;			/* a reasonable assumption? --jsh */

  sgtk_init_substrate ();
  sgtk_inited = 1;
}

static char*
xstrdup (char *str)
{
  if (str)
    {
      char *newstr = rep_alloc (strlen(str)+1);
      strcpy (newstr, str);
      return newstr;
    }
  else
    return NULL;
}

static void
make_argv (repv list, int *argc, char ***argv)
{
  static char *argv_storage[1] = { "rep-gtk" };

  int c = list_length (list), i;
  char **v;

  *argv = argv_storage;
  *argc = 1;

  if (c < 0)
    return;

  v = (char **)rep_alloc ((c+1) * sizeof(char**));
  for (i = 0; i < c; i++, list = rep_CDR (list))
    {
      if (!rep_STRINGP (rep_CAR (list)))
	{
	  rep_free ((char *)v);
	  return;
	}
      v[i] = xstrdup (rep_STR (rep_CAR (list)));
    }
  v[c] = NULL;
  
  *argv = v;
  *argc = c;
}

void
sgtk_init (void)
{
  int argc;
  char **argv;
  repv head, *last;

  if (sgtk_inited)
    return;

  make_argv (Fcons (Fsymbol_value (Qprogram_name, Qt),
		    Fsymbol_value (Qcommand_line_args, Qt)), &argc, &argv);
  sgtk_init_with_args (&argc, &argv);

  argc--; argv++;
  head = Qnil;
  last = &head;
  while(argc > 0)
  {
      *last = Fcons(rep_string_dup(*argv), Qnil);
      last = &rep_CDR(*last);
      argc--;
      argv++;
  }
  Fset (Qcommand_line_args, head);
}



/* DL hooks */

extern void sgtk_init_gtk_gtk_glue (void);

repv
rep_dl_init (void)
{
  repv tem = rep_push_structure ("gui.gtk.gtk");
  /* ::alias:gui.gtk gui.gtk.gtk::
     ::alias:gtk gui.gtk.gtk:: */
  rep_alias_structure ("gui.gtk");
  rep_alias_structure ("gtk");
  sgtk_init_gtk_gtk_glue ();
  return rep_pop_structure (tem);
}

/* This is required mainly since other dls may try to unregister
   inputs as they're being deleted. */
void
rep_dl_kill (void)
{
    if (rep_register_input_fd_fun == sgtk_register_input_fd)
	rep_register_input_fd_fun = 0;
    if (rep_deregister_input_fd_fun == sgtk_deregister_input_fd)
	rep_deregister_input_fd_fun = 0;
    if (rep_event_loop_fun == sgtk_event_loop)
	rep_event_loop_fun = 0;
    if (rep_sigchld_fun == sgtk_sigchld_callback)
	rep_sigchld_fun = 0;
    if (gdk_display != 0)
	rep_deregister_input_fd (ConnectionNumber (gdk_display));
}
