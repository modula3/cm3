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
 * Created On      : Wed Sep 13 11:53:29 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Sep 25 22:36:21 1995
 * Update Count    : 7
 * 
 * SCCS Status     : %W%	%G%
 * 
 * HISTORY
 *)

MODULE SharedObjRep;

IMPORT SharedObj, EventWireRep, Fmt, WeakRefList;

PROCEDURE ToText(o: SharedObj.T): TEXT =
  VAR ret : TEXT;
  BEGIN
    ret := "SharedObj(" & EventWireRep.ToText(o.wrep) &")seq(" &
               o.seqNoCnt.value().fmt(10) & ")timeliness(" &
               Fmt.Int(o.timeliness) & ")callbacks(";
    IF o.callbacks # NIL THEN
      ret := ret & Fmt.Int(WeakRefList.Length(o.callbacks));
    ELSE
      ret := ret & "0";
    END;
    RETURN ret & ")";
  END ToText;

BEGIN
END SharedObjRep.
