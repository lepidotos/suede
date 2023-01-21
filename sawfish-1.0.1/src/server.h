/* server.h -- Definitions for client/server
   $Id: server.h,v 1.7 2000/08/13 20:31:01 john Exp $

   Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

   This file is part of sawmill.

   sawmill is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   sawmill is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with sawmill; see the file COPYING.   If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */

#ifndef SAWMILL_SERVER_H
#define SAWMILL_SERVER_H

/* Directory containing the unix domain sockets, each socket is
   simply the canonical display name */
#define SAWMILL_SOCK_DIR "/tmp/.sawfish-%s"

/* Types of request packet. A byte with one of these values is sent to
   initiate a command. */
enum server_request {
    req_eval,
    req_eval_async,
    req_end_of_session = 255
};

#endif /* SAWMILL_SERVER_H */
