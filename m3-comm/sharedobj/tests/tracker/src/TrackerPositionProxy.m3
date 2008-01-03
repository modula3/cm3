(*
 * TrackerPositionProxy.m3 -- a netobj proxy for getting the trackerposition
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
 * Created On      : Sat Jul 15 22:23:46 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Sun Jul 16 11:00:54 1995
 * Update Count    : 4
 * Status          : Unknown, Use with caution!
 * 
 * $Source: /opt/cvs/cm3/m3-comm/sharedobj/tests/tracker/src/TrackerPositionProxy.m3,v $
 * $Date: 2001-12-02 13:14:14 $
 * $Author: wagner $
 * $Revision: 1.1.1.1 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.1.1.1  1996/03/03 19:20:26  bm
 * Imported Sources
 *
 * 
 * HISTORY
 *)

MODULE TrackerPositionProxy;

IMPORT TrackerPosition;

REVEAL 
  T = Public BRANDED Brand OBJECT 
    pos: TrackerPosition.T;
  OVERRIDES
    get := Get;
    init := Init;
  END;

PROCEDURE Get(self: T): TrackerPosition.T =
  BEGIN
    RETURN self.pos;
  END Get;

PROCEDURE Init(self: T; t: TrackerPosition.T): T =
  BEGIN
    self.pos := t;
    RETURN self;
  END Init;

BEGIN
END TrackerPositionProxy.
