(* 
 * This library is free software; you can redistribute it and/or          
 * modify it under the terms of the GNU Library General Public            
 * License as published by the Free Software Foundation.                  
 * This library is distributed in the hope that it will be useful,        
 * but WITHOUT ANY WARRANTY; without even the implied warranty of         
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      
 * Library General Public License for more details.                       
 * If you do not have a copy of the GNU Library General Public            
 * License, write to The Free Software Foundation, Inc.,                  
 * 675 Mass Ave, Cambridge, MA 02139, USA.                                
 *                                                                        
 * For more information on this program, contact Blair MacIntyre          
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 500 W 120th St, Room 450, New York, NY, 10027.                         
 *                                                                        
 * Copyright (C) Blair MacIntyre 1995, Columbia University 1995           
 * 
 * Author          : Blair MacIntyre
 * Created On      : Thu Jan 19 17:38:38 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Tue Dec  5 13:21:04 1995
 * Update Count    : 99
 * 
 * SCCS Status     : @(#)TrackerPosition.m3	1.2	30 Jan 1996
 * 
 * HISTORY
 *)

MODULE TrackerPosition EXPORTS TrackerPosition, TrackerPositionF,
                               TrackerPositionProxy;

PROCEDURE Init (self: S) : T =
  BEGIN
    IF self.data = NIL THEN
      self.data := NEW(Data);
    END;

    RETURN self;
  END Init;

PROCEDURE Set (self: S; READONLY val: Data) =
  BEGIN
    self.data := val;
  END Set;

PROCEDURE Get (self: S): Data =
  BEGIN
    RETURN self.data;
  END Get;

BEGIN
END TrackerPosition.
