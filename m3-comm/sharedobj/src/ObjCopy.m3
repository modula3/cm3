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
 * Created On      : Tue Apr 25 13:57:19 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Fri Nov 22 14:00:21 1996
 * Update Count    : 16
 * 
 * $Source: /opt/cvs/cm3/m3-comm/sharedobj/src/ObjCopy.m3,v $
 * $Date: 2001-12-02 13:41:16 $
 * $Author: wagner $
 * $Revision: 1.2 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.1.1.1  2001/12/02 13:14:14  wagner
 * Blair MacIntyre's sharedobj package
 *
 * Revision 1.2  1996/11/22 19:00:25  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

MODULE ObjCopy;

IMPORT EventConn, SpaceConn;

(* Only allowed to have one copy per host, so if the hosts are equal,
   the objects are equal. *)
PROCEDURE Equal(k1, k2: T): BOOLEAN =
  BEGIN
    RETURN EventConn.Equal(k1.conn, k2.conn);
  END Equal;

PROCEDURE ToText(c: T): TEXT =
  BEGIN
    RETURN "ObjCopy for " & SpaceConn.ToText(c.conn);
  END ToText;

BEGIN
END ObjCopy.
