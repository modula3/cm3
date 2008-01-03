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
 * Created On      : Sun Jul 30 23:55:43 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Wed May 15 11:18:46 1996
 * Update Count    : 18
 * 
 * SCCS Status     : @(#)TrackerPositionF.i3	1.2	30 Jan 1996
 * 
 * HISTORY
 *)

INTERFACE TrackerPositionF;

FROM TrackerPosition IMPORT T, S, Public, Brand, Data;
 
REVEAL
   S = Public BRANDED Brand & ".S" OBJECT
        data: Data := NIL;
      OVERRIDES
        init := Init;
        set := Set;
        get := Get;
      END;

PROCEDURE Init (self: S): T;

PROCEDURE Set (self: S; READONLY val: Data);

PROCEDURE Get (self: S): Data; 

END TrackerPositionF.
