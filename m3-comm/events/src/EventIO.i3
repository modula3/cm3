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
 * Created On      : Tue Jun 13 00:49:09 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Nov 21 17:36:33 1996
 * Update Count    : 9
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.3  1996/11/21 22:36:37  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

INTERFACE EventIO;

IMPORT Event, Rd, Wr, Thread;

PROCEDURE Read (rd: Rd.T): Event.T RAISES {Event.Error, Rd.Failure, 
                                              Thread.Alerted};

(* "Read" is called by the runtime when a new event arrives on a reader.
   The header is read and a new "Event.T" is created to hold this incoming
   event.\ttindex{EventStubLib.Handle} There should be no need for user's
   of the event package to call this routine directly.  However, they
   can if they wish to construct events and read/write them to files.  *)

PROCEDURE Write (wr: Wr.T; ev: Event.T)
  RAISES {Rd.Failure, Wr.Failure, Thread.Alerted};

END EventIO.
