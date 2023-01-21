/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */

/*
 *  Medusa
 *
 *  Copyright (C) 2000 Eazel, Inc.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public
 *  License as published by the Free Software Foundation; either
 *  version 2 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License along with this library; if not, write to the Free
 *  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  Author: Rebecca Schulman <rebecka@eazel.com>
 */

/* medusa-rdb-table-private.h:  Functions used only by the
   table objects */

#ifndef MEDUSA_RDB_TABLE_PRIVATE_H
#define MEDUSA_RDB_TABLE_PRIVATE_H

#include <medusa-rdb-table.h>


void        
medusa_rdb_table_virtual_insert_record  (MedusaRDBTable *table,
					 MedusaRDBRecordNumber number);

#endif /* MEDUSA_RDB_TABLE_PRIVATE_H */
