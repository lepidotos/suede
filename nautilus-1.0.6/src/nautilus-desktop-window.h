/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */

/*
 * Nautilus
 *
 * Copyright (C) 2000 Eazel, Inc.
 *
 * Nautilus is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * Nautilus is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Authors: Darin Adler <darin@bentspoon.com>
 */

/* nautilus-desktop-window.h
 */

#ifndef NAUTILUS_DESKTOP_WINDOW_H
#define NAUTILUS_DESKTOP_WINDOW_H

#include "nautilus-window.h"
#include "nautilus-application.h"

#define NAUTILUS_TYPE_DESKTOP_WINDOW            (nautilus_desktop_window_get_type())
#define NAUTILUS_DESKTOP_WINDOW(object)         (GTK_CHECK_CAST ((object), NAUTILUS_TYPE_DESKTOP_WINDOW, NautilusDesktopWindow))
#define NAUTILUS_DESKTOP_WINDOW_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), NAUTILUS_TYPE_DESKTOP_WINDOW, NautilusDesktopWindowClass))
#define NAUTILUS_IS_DESKTOP_WINDOW(object)      (GTK_CHECK_TYPE ((object), NAUTILUS_TYPE_DESKTOP_WINDOW))
#define NAUTILUS_IS_DESKTOP_WINDOW_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), NAUTILUS_TYPE_DESKTOP_WINDOW))

typedef struct NautilusDesktopWindowDetails NautilusDesktopWindowDetails;

typedef struct {
	NautilusWindow parent_spot;
	NautilusDesktopWindowDetails *details;
        gboolean affect_desktop_on_next_location_change;
} NautilusDesktopWindow;

typedef struct {
	NautilusWindowClass parent_spot;
} NautilusDesktopWindowClass;

GtkType                nautilus_desktop_window_get_type            (void);
NautilusDesktopWindow *nautilus_desktop_window_new                 (NautilusApplication *application);
void                   nautilus_desktop_window_update_directory    (NautilusDesktopWindow *window);

#endif /* NAUTILUS_DESKTOP_WINDOW_H */
