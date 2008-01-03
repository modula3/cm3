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
 * Created On      : Fri Jul 14 17:54:34 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Wed Sep 13 10:48:12 1995
 * Update Count    : 7
 * 
 * SCCS Status     : @(#)WeakerRef.i3	1.2	13 Sep 1995
 * 
 * HISTORY
 *)

INTERFACE WeakerRef;

IMPORT WeakRef;

CONST Brand = "WeakRef";
      
TYPE
  T = REF RECORD 
    weakRef := WeakRef.T{ARRAY [0..7] OF BITS 8 FOR [0..255] {0, ..}}; 
    ready := FALSE; 
  END;

PROCEDURE Equal(k1, k2: T): BOOLEAN;

END WeakerRef.
