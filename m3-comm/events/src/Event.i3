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
 * Created On      : Wed Jun  7 15:56:08 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Nov 21 17:31:54 1996
 * Update Count    : 33
 * 
 * $Source: /opt/cvs/cm3/m3-comm/events/src/Event.i3,v $
 * $Date: 2001-12-02 00:20:37 $
 * $Author: wagner $
 * $Revision: 1.2 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.1.1.1  2001/12/02 00:06:45  wagner
 * Blair MacIntyre's events library
 *
 * Revision 1.2  1997/01/23 15:26:33  bm
 * Lots of little bug fixes.
 *
 * 
 * HISTORY
 *)

INTERFACE Event;

IMPORT EventRd, AtomList, EventSpaceID, EventNumber, EventConn, TimeStamp;
FROM EventProtocol IMPORT MsgHeader, ID, StubProtocol;

CONST Brand = "Event";

TYPE
  T <: Public;
  Public = OBJECT
    (* for creating a list of them *)
    next: T := NIL;

    (* Event data *)
    hdr: MsgHeader;
    from: EventSpaceID.T;
    ts: TimeStamp.T;
    prot: StubProtocol;
    num: EventNumber.T;

    (* who sent this to us.  NIL means we created it! *)
    sender: EventConn.T := NIL;

    (* the reader for an event *)
    rd: EventRd.T;
  METHODS
    (* Initialize an event. *)
    init (id: ID; eventProt: StubProtocol; num: EventNumber.T);

    (* Keep track of the number of references to an event.  The Event
       module may cache a certain number of event structures for reuse
       by "New". A new, or freshly initialized event, has a reference
       count of 1.  When the reference count drops to 0 the event may
       be reused. *)
    addRef ();
    dropRef ();
  END;

(* "hdr" contains the header that is sent out at the start of
   the event.  "rep" contains the "DataRep" part of "hdr".  "prot"
   contains the "prot" part of "rep", converted to local byte
   ordering. "rd" contains the bulk of the event.  "ts" is the
   timestamp of event creation. *)

(* Get an event structure (possibly from the cache list) *)
PROCEDURE New (): T;

PROCEDURE ToText(ev: T): TEXT;
(* "ToText" creates a text version of ev, suitable for use during
   debugging. *)

EXCEPTION Error(AtomList.T);

END Event.
