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
 * Created On      : Mon Jun 19 21:08:25 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Wed Aug 28 10:15:24 1996
 * Update Count    : 72
 * 
 * SCCS Status     : %W%	%G%
 * 
 * HISTORY
 *)

INTERFACE ObjectSpace;

IMPORT NetObj, SharedObj, SharedObjRep;

CONST Brand = "ObjectSpace";

TYPE
  T = NetObj.T OBJECT
      METHODS
        get ():  SharedObj.T;
      END;

END ObjectSpace.
