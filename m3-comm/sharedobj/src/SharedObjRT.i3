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
 * Created On      : Wed May 24 10:28:43 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Fri Oct 24 12:01:59 1997
 * Update Count    : 68
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.3  1997/10/24 19:32:56  bm
 * Added the ability to flush the incoming updates.
 *
 * Revision 1.2  1996/11/22 19:02:04  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

(* This interface contains procedures to control the Shared Object
   RunTime system.   Each process must call these routines to set up
   the shared object system.  In particular, the runtime system needs
   to know which process is its default sequencer.

   A sequencer coordinates sequencing and distribution of object
   updates.  To sequence the updates, it takes the update requests it
   receives from copies of the shared objects, serializes them,
   assigned each a sequence number and sends them back to all copies.
   This is how the system ensures that all copies of the objects have
   all updates applied to them in the same order.

   Each object has one sequencer that handles its updates.  By
   default, each machine has a sequencer that handles all the objects
   created on it.  If the sequencer for the object is not the default
   sequencer for that machine, the sequencer will take care of
   fowarding messages to the appropriate sequencer. The default
   sequencer is set using the "SetDfltSequencer()" procedure.  If the
   return value of "LocalSpace()" is passed to "SetDfltSequencer()",
   this process becomes a sequencer.

*)

INTERFACE SharedObjRT;

IMPORT ObjectSpace, Text, SharedObj, Thread; 

PROCEDURE LocalSpace(): ObjectSpace.T;
(* Each "space" (running Modula-3 process) has its own local "space
   object".  This object is used to communicate with other spaces to
   control the shared object system.  No routines in the
   "ObjectSpace.T" object that should be called by the user. *)

PROCEDURE SetDfltSequencer(seq: ObjectSpace.T) RAISES {SharedObj.Error};

(* "SetDfltSequencer()" must be called to tell the local space which
   space is its default sequencer. *)

PROCEDURE ExportSpace(name: Text.T) RAISES {SharedObj.Error, Thread.Alerted};

(* Export our local space under name "name". *)

PROCEDURE ImportSpace (host: Text.T; name: Text.T): ObjectSpace.T
  RAISES {SharedObj.Error, Thread.Alerted};

(* Set the level of debugging output. *)
PROCEDURE DebugLevel(p: INTEGER);

(* Flush all the read connections into the message event port. *)
PROCEDURE FlushIncomingUpdates() RAISES {Thread.Alerted};

(* Flush all the queued input events from the message event port. *)
PROCEDURE FlushQueuedUpdates() RAISES {Thread.Alerted};

END SharedObjRT.
