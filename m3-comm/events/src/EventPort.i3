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
 * Created On      : Thu Jun  1 16:49:22 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Fri Oct 24 11:50:35 1997
 * Update Count    : 55
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.3  1997/10/24 19:31:30  bm
 * Added the ability to flush the readers and worker pool.
 *
 * Revision 1.2  1997/01/23 15:26:38  bm
 * Lots of little bug fixes.
 *
 * 
 * HISTORY
 *)

(* An "EventPort.T" is a place events are sent from and to.  Each event
   port is associated with the machine it is created on, but you may create
   several, independent "EventPort.T"s in a given process.  However, each
   "EventPort.T" can talk to at most one "EventPort.T" on another machine.
   In other words, you can create several independent networks of
   "EventPort.T"s.

   It is intended that a given user of the event package will only create
   one port per machine.

   The MsgRd.T "rd" and MsgWr.T "wr" passed in will be used as
   follows.  When an event is sent, data will be written to "wr",
   followed by a call to "wr.nextMsg()".  Conversely, "rd.nextMsg()"
   will be called to wait for new data, after which data will be read
   from "rd".  Note that this implies that the initial state of "rd"
   is at the end of a message, whereas "wr" is at the beginning of a
   message. 

   "stealWorker()" should be called if a thread delivering an event
   is about to block,  since each port has a limited number of threads
   to handle message delivery.  The event port may then create an
   extra thread if it needs to.  It is ok to call stealWorker from any
   thread:  the return value indicates if the thread was a thread
   created by the EventPort for message delivery.

   "flushReader()" should be called if/when it is a good time to pause
   while all pending events on the input pipe are read.  It will
   return when all the events are read and added to the work queue.
   
   "flushWork()" should be called if/when it is a good time to pause
   while all pending work in the work queue is performed.  It will
   return when the work queue is empty.
   *)

INTERFACE EventPort;

IMPORT EventProtocol, Event, Thread, EventConn, EventConnList, EventSeq;
FROM EventProtocol IMPORT ID, StubProtocol;
FROM Event IMPORT Error;

TYPE
  T <: Public;
  Public = OBJECT METHODS
             init (debug: BOOLEAN := FALSE): T;
             connect (c: EventConn.T) RAISES {Error};
             disconnect (c: EventConn.T): EventSeq.T RAISES {Error};
             send (c: EventConn.T; ev: Event.T) RAISES {Error};
             mcast (cs: EventConnList.T; ev: Event.T) RAISES {Error};
             register (id: ID; prot: StubProtocol; 
                       disp: Dispatcher := DefaultDispatcher;
                       data: REFANY) RAISES {Error};
             stealWorker(): BOOLEAN;
             flushReader() RAISES {Thread.Alerted};
             flushWork() RAISES {Thread.Alerted};
           END;

TYPE
  Dispatcher =
    PROCEDURE (ev: Event.T; data: REFANY) RAISES {Thread.Alerted}; 

PROCEDURE DefaultDispatcher(ev: Event.T; data: REFANY);

(* A procedure of type "Dispatcher" is registered for each event for which
   stubs exist.  The dispatcher is called by the event runtime when it
   receives an event.  

   *)

END EventPort.

