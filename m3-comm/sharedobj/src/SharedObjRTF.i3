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
 * Created On      : Wed Sep 13 11:19:30 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Tue Dec  3 13:16:24 1996
 * Update Count    : 46
 * 
 * $Source: /opt/cvs/cm3/m3-comm/sharedobj/src/SharedObjRTF.i3,v $
 * $Date: 2001-12-02 13:14:14 $
 * $Author: wagner $
 * $Revision: 1.1.1.1 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.3  1997/01/23 15:27:18  bm
 * Lot's of little bug fixes.
 *
 * Revision 1.2  1996/11/22 19:02:26  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

INTERFACE SharedObjRTF;

IMPORT AtomList, Thread, ObjectInfo, SharedObjRep, Debug,
       Message, SpaceConn, ObjectSpace, Event, EventSeq, WeakRef,
       SharedObj; 

TYPE
  Op = { MethodCall };

(* "Op" indicates the message type and is used the event type in the
   event package, as follows:

   "MethodCall" indicates a method invocation.  A header containing
   (in order) the wirerep of the object, the sequence number of the
   update call and an identifier for the calling thread is at the
   start of the event.  These are followed by the method number and
   the arguments of the call, which are read by the appropriate
   applyUpdate() method. *)

PROCEDURE GetObjInfo(wrep: SharedObjRep.WireRep; 
                     VAR objInfo: ObjectInfo.T): BOOLEAN; 
PROCEDURE GetObjRef(objInfo: ObjectInfo.T): SharedObj.T;
PROCEDURE SequenceMsg(m: Message.T): BOOLEAN 
  RAISES {SharedObj.Error, Thread.Alerted};
PROCEDURE CheckForLastClient(objInfo: ObjectInfo.T);
PROCEDURE RegisterObject(obj: SharedObj.T; wrep: SharedObjRep.WireRep; 
                         sequencer: SpaceConn.T; standAlone: BOOLEAN; 
                         seqNo: SharedObj.SequenceNumber): ObjectInfo.T;
PROCEDURE AddClient(objInfo: ObjectInfo.T; conn: SpaceConn.T) 
  RAISES {SharedObj.Error};
PROCEDURE DeleteClient(objInfo: ObjectInfo.T; conn: SpaceConn.T) 
  RAISES {SharedObj.Error};
PROCEDURE WaitForSequencer() RAISES {Thread.Alerted};
PROCEDURE GetDfltSequencer(): ObjectSpace.T RAISES {Thread.Alerted};
PROCEDURE GetSequencer(wrep: SharedObjRep.WireRep): ObjectSpace.T
  RAISES {SharedObj.Error}; 
PROCEDURE SequencerFailed(seq: SpaceConn.T; ec: AtomList.T);

PROCEDURE EventPortConnect(conn: SpaceConn.T) RAISES {SharedObj.Error};
PROCEDURE EventPortDisconnect (conn: SpaceConn.T): EventSeq.T 
  RAISES {SharedObj.Error};

PROCEDURE SequenceCall(obj: SharedObj.T; ev: Event.T; thread: INTEGER; 
                       objInfo: ObjectInfo.T; en: SharedObj.SequenceNumber;
                       VAR alerted: BOOLEAN) 
  RAISES {SharedObj.Error, Thread.Alerted};

PROCEDURE CleanupSharedObj(READONLY w: WeakRef.T; r: REFANY);

PROCEDURE ObjTblToText(): TEXT;

VAR
  objTblMu: MUTEX; (* A mutex for the object table. *)

  (* For debugging.  Messages are turned off by default. *)
  debug: Debug.T;
  debug_level := 0;

  zeroSeqNo: SharedObj.SequenceNumber;
  oneSeqNo: SharedObj.SequenceNumber;

  localSpace: ObjectSpace.Local := NIL;

END SharedObjRTF.
