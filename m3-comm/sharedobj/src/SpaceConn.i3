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
 * Created On      : Wed Jun 28 13:42:37 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Fri Nov 22 14:03:43 1996
 * Update Count    : 10
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.2  1996/11/22 19:03:47  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

INTERFACE SpaceConn;

IMPORT EventConn, ObjectSpace;

CONST Brand = "SpaceConn";

(* All the infomation about a connection goes in here.  Eventually, we will
   be able to maintain infomation about retrying connections, etc. *)

TYPE T = EventConn.T OBJECT
                       objSpace: ObjectSpace.T;  
                       connected: BOOLEAN := TRUE;
                     END;

(* "objSpace" is the space network object on the remote space.
   "connected" indicates if the space is actually connected.  This is
   used to do lazy cleanup when the spaces are disconnected. *)

(* generate a text representation of the SpaceConn for debugging. *)
PROCEDURE ToText(c: T): TEXT;

END SpaceConn.
