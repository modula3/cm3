(* 
 * TrackerPositionF.i3 -- defines the internal representation of a trackerpos
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
 * Created On      : Sun Jul 30 23:55:43 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Jul 31 00:03:44 1995
 * Update Count    : 4
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

INTERFACE TrackerPositionF;

FROM TrackerPosition IMPORT S, Public, Brand, Data;
 
REVEAL
   S = Public BRANDED Brand OBJECT
        data: Data;
      OVERRIDES
        set := Set;
        get := Get;
      END;

PROCEDURE Set (self: S; READONLY val: Data);

PROCEDURE Get (self: S; VAR val: Data); 

END TrackerPositionF.
