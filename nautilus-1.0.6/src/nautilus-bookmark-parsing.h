/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */

/*
 * Nautilus
 *
 * Copyright (C) 1999, 2000 Eazel, Inc.
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
 * Authors: John Sullivan <sullivan@eazel.com>
 */

/* nautilus-bookmark-parsing.h - routines to parse bookmarks from XML data.
 */

#ifndef NAUTILUS_BOOKMARK_PARSING_H
#define NAUTILUS_BOOKMARK_PARSING_H

#include <libnautilus-private/nautilus-bookmark.h>
#include <libxml/tree.h>

NautilusBookmark *nautilus_bookmark_new_from_node (xmlNodePtr node);

#endif /* NAUTILUS_BOOKMARK_PARSING_H */
