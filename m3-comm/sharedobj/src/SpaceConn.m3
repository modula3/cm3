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
 * Created On      : Sat Jul 15 12:26:20 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Fri Nov 22 14:03:55 1996
 * Update Count    : 6
 * 
 * $Source: /opt/cvs/cm3/m3-comm/sharedobj/src/SpaceConn.m3,v $
 * $Date: 2001-12-02 13:14:14 $
 * $Author: wagner $
 * $Revision: 1.1.1.1 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.2  1996/11/22 19:03:59  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

MODULE SpaceConn;

IMPORT EventSpaceID;

PROCEDURE ToText(c: T): TEXT =
  BEGIN
    IF c.connected THEN
      RETURN EventSpaceID.ToText(c.space);
    ELSE
      RETURN "*unconnected space*";
    END;
  END ToText;

BEGIN
END SpaceConn.
