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
 * Last Modified On: Thu Nov 21 17:35:29 1996
 * Update Count    : 15
 * 
 * $Source: /opt/cvs/cm3/m3-comm/events/src/EventHandle.i3,v $
 * $Date: 2001-12-02 00:06:45 $
 * $Author: wagner $
 * $Revision: 1.1.1.1 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.3  1996/11/21 22:35:41  bm
 * fixed header
 *
 * 
 * HISTORY
 *  Based on StubConn.i3, header here:
 * Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* StubConn.i3 *)
(* Last modified on Mon Nov  7 12:34:49 PST 1994 by wobber  *)
(*      modified on Wed Dec  2 11:40:50 PST 1992 by gnelson *)
(*      modified on Wed Jun 24 11:12:25 PDT 1992 by owicki *)


(* An "EventStubLib.Handle" represents a handle used to either
   marshal and unmarshal events into and out of EventRd.T structures.
   Here we reveal that a handle "h" consists of an event writer
   "h.wr" used for marshalling an event and an event structure "c.event"
   used for unmarshalling an event.\ttindex{EventStubLib.Handle}  *)

INTERFACE EventHandle;

IMPORT Event, EventWr, EventStubLib, Thread;
   
REVEAL EventStubLib.Handle <: Public;  

TYPE 
  Public = Thread.Mutex OBJECT 
    cur: CARDINAL;
    event: Event.T; 
    wr: EventWr.T 
  END;

END EventHandle.

(* Clients can use this interface to bypass the procedures in the
   "EventStubLib" interface and marshal and unmarshal arguments using
   inline code, for example to write directly to the underlying
   writer.  To do this, import the "RdClass" and "WrClass"
   interfaces\cite{Modula3} to reveal the internal structure of
   readers and writers.  You will have to be careful about locks.  All
   readers and writers contain an internal lock used to serialize
   operations.  It is a requirement of the "EventStubLib" interface
   that all parameters of type "Handle" be passed with both streams
   unlocked.  

   The two parts of the "Public" object are disjoint.  "wr" is used
   when an event is being created.  "cur" and "event" are used when an
   event is being read.  "cur" is the offset of the
   "EventStubLib.Handle" in "event.rd".  This must be maintained here
   because there can be multiple simultaneous readers of an "Event.T".

   There are two final clauses in the specification of the message
   writer "wr" and the message reader "event.rd" in an
   "EventHandle.Public".  First, their buffers must be word-aligned in
   memory. More precisely, if byte "i" in the data stream is stored in
   the buffer at memory address "j", then "i" and "j" must be equal
   modulo the machine word size. This requirement allows optimized
   stubs to read and write scalar values from the buffer
   efficiently.\index{buffered streams}. *)
