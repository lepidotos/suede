/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- *//* 
 * Copyright (C) 1998-2000 Free Software Foundation
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
 * Authors: Eskil Heyn Olsen
 *
 */

/* $Id: calendar-conduit.h,v 1.9 2000/12/03 03:26:46 eskil Exp $ */

#ifndef __CALENDAR_CONDUIT_H__
#define __CALENDAR_CONDUIT_H__

#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <pi-datebook.h>
#include <gnome.h>

#include "GnomeCal.h"
#include "calobj.h"
#include "calendar.h"
#include "timeutil.h"

#include <gpilotd/gnome-pilot-conduit.h>
#include <gpilotd/gnome-pilot-conduit-standard-abs.h>

/* This is the local record structure for the GnomeCal conduit. */
typedef struct _GCalLocalRecord GCalLocalRecord;
struct _GCalLocalRecord {
	/* The stuff from gnome-pilot-conduit-standard-abs.h
	   Must be first in the structure, or instances of this
	   structure cannot be used by gnome-pilot-conduit-standard-abs.
	*/
	LocalRecord local;
	/* The corresponding iCal object, as found by GnomeCal. */
	iCalObject *ical;
        /* pilot-link appointment structure, used for implementing Transmit. */	
	struct Appointment *a;
};
#define GCAL_LOCALRECORD(s) ((GCalLocalRecord*)(s))

/* This is the configuration of the GnomeCal conduit. */
typedef struct _GCalConduitCfg GCalConduitCfg;
struct _GCalConduitCfg {
	gboolean open_secret;
	guint32 pilotId;
	GnomePilotConduitSyncType  sync_type;   /* only used by capplet */
};
#define GET_GCALCONFIG(c) ((GCalConduitCfg*)gtk_object_get_data(GTK_OBJECT(c),"gcalconduit_cfg"))

/* This is the context for all the GnomeCal conduit methods. */
typedef struct _GCalConduitContext GCalConduitContext;
struct _GCalConduitContext {
	struct AppointmentAppInfo ai;
	GCalConduitCfg *cfg;
	GNOME_Calendar_Repository calendar;
	CORBA_Environment ev;
	CORBA_ORB orb;
};
#define GET_GCALCONTEXT(c) ((GCalConduitContext*)gtk_object_get_data(GTK_OBJECT(c),"gcalconduit_context"))

/* Given a GCalConduitCfg*, allocates the structure and 
   loads the configuration data for the given pilot. */
static void 
gcalconduit_load_configuration(GCalConduitCfg **c,
			       guint32 pilotId) 
{
	gchar prefix[256];
	g_snprintf(prefix,255,"/gnome-pilot.d/calendard-conduit/Pilot_%u/",pilotId);
	
	*c = g_new0(GCalConduitCfg,1);
	g_assert(*c != NULL);
	gnome_config_push_prefix(prefix);
	(*c)->open_secret = gnome_config_get_bool("open_secret=FALSE");
	(*c)->sync_type = GnomePilotConduitSyncTypeCustom; /* set in capplets main */
	gnome_config_pop_prefix();
	
	(*c)->pilotId = pilotId;
}

/* Saves the configuration data. */
static void 
gcalconduit_save_configuration(GCalConduitCfg *c) 
{
	gchar prefix[256];

	g_snprintf(prefix,255,"/gnome-pilot.d/calendar-conduit/Pilot_%u/",c->pilotId);

	gnome_config_push_prefix(prefix);
	gnome_config_set_bool("open_secret",c->open_secret);
	gnome_config_pop_prefix();

	gnome_config_sync();
	gnome_config_drop_all();
}

/* Creates a duplicate of the configuration data */
static GCalConduitCfg*
gcalconduit_dupe_configuration(GCalConduitCfg *c) {
	GCalConduitCfg *retval;
	g_return_val_if_fail(c!=NULL,NULL);
	retval = g_new0(GCalConduitCfg,1);
	retval->sync_type = c->sync_type;
	retval->open_secret = c->open_secret;
	retval->pilotId = c->pilotId;
	return retval;
}

/* Destroys any data allocated by gcalconduit_load_configuration
   and deallocates the given configuration. */
static void 
gcalconduit_destroy_configuration(GCalConduitCfg **c) 
{
	g_return_if_fail(c!=NULL);
	g_return_if_fail(*c!=NULL);
	g_free(*c);
	*c = NULL;
}

/* Given a GCalConduitContxt*, allocates the structure */
static void
gcalconduit_new_context(GCalConduitContext **ctxt,
			GCalConduitCfg *c) 
{
	*ctxt = g_new0(GCalConduitContext,1);
	g_assert(ctxt!=NULL);
	(*ctxt)->cfg = c;
	CORBA_exception_init (&((*ctxt)->ev));
}

/* Destroys any data allocated by gcalconduit_new_context
   and deallocates its data. */
static void
gcalconduit_destroy_context(GCalConduitContext **ctxt)
{
	g_return_if_fail(ctxt!=NULL);
	g_return_if_fail(*ctxt!=NULL);
/*
	if ((*ctxt)->cfg!=NULL)
		gcalconduit_destroy_configuration(&((*ctxt)->cfg));
*/
	g_free(*ctxt);
	*ctxt = NULL;
}
#endif __CALENDAR_CONDUIT_H__ 
