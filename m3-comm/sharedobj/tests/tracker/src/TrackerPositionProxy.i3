(*
 * TrackerPositionProxy.i3 -- a netobj proxy for getting the trackerposition
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * For more information on this program, contact Blair MacIntyre at
 * bm@cs.columbia.edu or Computer Science Dept., Columbia University,
 * 500 W 120th St, Room 450, New York, NY, 10027.
 *
 * Copyright (C) Blair MacIntyre 1995
 * Copyright (C) Columbia University 1995
 *
 * Author          : Blair MacIntyre
 * Created On      : Sat Jul 15 22:22:21 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Sun Jul 16 22:40:40 1995
 * Update Count    : 9
 * Status          : Unknown, Use with caution!
 *
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 *
 * $Log$
 * Revision 1.1.1.1  1996/03/03 19:20:26  bm
 * Imported Sources
 *
 *
 * HISTORY
 *)

INTERFACE TrackerPositionProxy;

IMPORT TrackerPosition, NetObj, Thread;

CONST Brand = "TrackerPositionProxy";

TYPE
  T = NetObj.T OBJECT
    METHODS
      get  (): TrackerPosition.T RAISES {NetObj.Error, Thread.Alerted};
      init (t: TrackerPosition.T): T;
    END;

END TrackerPositionProxy.
