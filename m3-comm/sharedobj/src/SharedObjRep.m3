(*                            -*- Mode: Modula-3 -*- 
 * 
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 * See file COPYRIGHT-COLUMBIA for details.
 * 
 * Author          : Blair MacIntyre
 * Created On      : Wed Sep 13 11:53:29 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Tue Dec  3 13:12:45 1996
 * Update Count    : 10
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.1.1.1  2001/12/02 13:14:14  wagner
 * Blair MacIntyre's sharedobj package
 *
 * Revision 1.3  1997/01/23 15:27:19  bm
 * Lot's of little bug fixes.
 *
 * Revision 1.2  1996/11/22 19:03:05  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

MODULE SharedObjRep;

IMPORT SharedObj, EventWireRep, Fmt, WeakRefList;

PROCEDURE ToText(o: SharedObj.T): TEXT =
  VAR ret : TEXT;
  BEGIN
    ret := "{" & EventWireRep.ToText(o.wrep) &")seq(" &
               o.seqNoCnt.value().fmt(10) & ")timeliness(" &
               Fmt.Int(o.timeliness) & ")callbacks(";
    IF o.callbacks # NIL THEN
      ret := ret & Fmt.Int(WeakRefList.Length(o.callbacks));
    ELSE
      ret := ret & "0";
    END;
    RETURN ret & "}";
  END ToText;

BEGIN
END SharedObjRep.
