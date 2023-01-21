/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*-

   eel-region.h: A simple wrapper on GdkRegion with rectangle operations.
 
   Copyright (C) 2000 Eazel, Inc.
  
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
  
   Author: Ramiro Estrugo <ramiro@eazel.com>
*/

#ifndef EEL_REGION_H
#define EEL_REGION_H

#include <libgnome/gnome-defs.h>
#include <libart_lgpl/art_rect.h>
#include <glib.h>
#include <gdk/gdk.h>

/* Opaque EelRegion declaration. */
typedef struct EelRegion EelRegion;

EelRegion *eel_region_new                (void);
void       eel_region_free               (EelRegion       *region);
void       eel_region_add_rectangle      (EelRegion       *region,
					  ArtIRect         rectangle);
void       eel_region_subtract_rectangle (EelRegion       *region,
					  ArtIRect         rectangle);
void       eel_region_set_gc_clip_region (const EelRegion *region,
					  GdkGC           *gc);

#endif /* EEL_REGION_H */

