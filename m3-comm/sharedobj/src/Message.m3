(*                            -*- Mode: Modula-3 -*- 
 * 
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 * 
 * Author          : Blair MacIntyre
 * Created On      : Wed Sep 13 10:33:49 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Tue Dec  3 13:13:18 1996
 * Update Count    : 3
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.3  1997/01/23 15:27:13  bm
 * Lot's of little bug fixes.
 *
 * Revision 1.2  1996/11/22 18:59:51  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

MODULE Message;

IMPORT Event, Fmt;

PROCEDURE Equal(k1, k2: T): BOOLEAN =
  BEGIN
    RETURN k1 = k2;
  END Equal;

PROCEDURE ToText(m: T): TEXT =
  BEGIN
    RETURN "{ev(" & Event.ToText(m.ev) & ")+thread(" &
           Fmt.Int(m.thread) & ")+seqNo(" & m.ev.num.fmt(10) & ")}";
  END ToText;

BEGIN
END Message.
