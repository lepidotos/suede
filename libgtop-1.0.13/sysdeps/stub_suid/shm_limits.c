/* $Id: shm_limits.c,v 1.3 1999/02/23 11:47:05 martin Exp $ */

/* Copyright (C) 1998-99 Martin Baulig
   This file is part of LibGTop 1.0.

   Contributed by Martin Baulig <martin@home-of-linux.org>, April 1998.

   LibGTop is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License,
   or (at your option) any later version.

   LibGTop is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with LibGTop; see the file COPYING. If not, write to the
   Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
*/

#include <glibtop.h>
#include <glibtop/error.h>
#include <glibtop/shm_limits.h>

#include <glibtop_suid.h>

static const unsigned long _glibtop_sysdeps_shm_limits = 0;

/* Init function. */

void
glibtop_init_shm_limits_p (glibtop *server)
{
	server->sysdeps.shm_limits = _glibtop_sysdeps_shm_limits;
}

/* Provides information about sysv ipc limits. */

void
glibtop_get_shm_limits_p (glibtop *server, glibtop_shm_limits *buf)
{
	glibtop_init_p (server, GLIBTOP_SYSDEPS_SHM_LIMITS, 0);
	
	memset (buf, 0, sizeof (glibtop_shm_limits));
}
