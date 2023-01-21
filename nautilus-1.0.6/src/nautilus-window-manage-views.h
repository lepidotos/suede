/* -*- Mode: C; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 2 -*- */

/*
 *  Nautilus
 *
 *  Copyright (C) 1999, 2000 Red Hat, Inc.
 *  Copyright (C) 1999, 2000, 2001 Eazel, Inc.
 *
 *  Nautilus is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public
 *  License as published by the Free Software Foundation; either
 *  version 2 of the License, or (at your option) any later version.
 *
 *  Nautilus is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License along with this program; if not, write to the Free
 *  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  Author: Darin Adler <darin@bentspoon.com>
 *
 */

#ifndef NAUTILUS_WINDOW_MANAGE_VIEWS_H
#define NAUTILUS_WINDOW_MANAGE_VIEWS_H

#include "nautilus-window.h"

void                    nautilus_window_manage_views_destroy         (NautilusWindow         *window);
void                    nautilus_window_open_location                (NautilusWindow         *window,
                                                                      const char             *location);
void                    nautilus_window_open_location_with_selection (NautilusWindow         *window,
                                                                      const char             *location,
                                                                      GList                  *selection);
void                    nautilus_window_stop_loading                 (NautilusWindow         *window);
void                    nautilus_window_set_content_view             (NautilusWindow         *window,
                                                                      NautilusViewIdentifier *id);
void                    nautilus_window_set_sidebar_panels           (NautilusWindow         *window,
                                                                      GList                  *view_identifier_list);
void                    nautilus_window_back_or_forward              (NautilusWindow         *window,
                                                                      gboolean                back,
                                                                      guint                   distance);
gboolean                nautilus_window_content_view_matches_iid     (NautilusWindow         *window,
                                                                      const char             *iid);
NautilusViewIdentifier *nautilus_window_get_content_view_id          (NautilusWindow         *window);

#endif /* NAUTILUS_WINDOW_MANAGE_VIEWS_H */
