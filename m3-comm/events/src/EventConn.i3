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
 * Created On      : Wed May 24 16:57:28 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Nov 21 17:32:50 1996
 * Update Count    : 25
 *
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.2  1996/11/21 22:33:00  bm
 * fixed the headers
 *
 * 
 * HISTORY
 *)

(* An "EventConn.T" represents a connection to another process through
   which events are to be distributed.\ttindex{EventConn.T}  *)

INTERFACE EventConn;

IMPORT MsgRd, MsgWr, EventSpaceID, AtomList;

CONST Brand = "EventConn";

TYPE 
  T = OBJECT 
    space: EventSpaceID.T;
    wr: MsgWr.T := NIL;
    rd: MsgRd.T := NIL;
  METHODS
    problem(al: AtomList.T);
  END;

(* The "problem" method must be provided and is called when there is
   an exception raised on either the "rd" or "wr" objects.  The user
   can then take some action, such as dropping the connection.  The
   "al" argument is an atomList describing the problem, and will
   include the parameter to any exception that triggered the call to
   "problem".  *)

PROCEDURE Equal(k1, k2: T): BOOLEAN;

END EventConn.
