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
 * Created On      : Mon Jun 19 21:08:25 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Dec  2 21:30:18 1996
 * Update Count    : 80
 * 
 * $Source: /opt/cvs/cm3/m3-comm/sharedobj/src/ObjectSpace.i3,v $
 * $Date: 2001-12-02 13:14:14 $
 * $Author: wagner $
 * $Revision: 1.1.1.1 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.5  1997/01/23 15:27:14  bm
 * Lot's of little bug fixes.
 *
 * Revision 1.4  1996/11/22 21:03:28  bm
 * fixed header
 *
 * Revision 1.3  1996/11/22 19:01:07  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

INTERFACE ObjectSpace;

IMPORT NetObj, IP, Thread, EventSpaceID, EventConn;
IMPORT SharedObj, SharedObjRep;

CONST Brand = "ObjectSpace";

TYPE
  (* Methods that are only to be called locally. *)
  Local = T OBJECT METHODS
        (* Connect to a space. *)
        newSpace (space: T): EventConn.T RAISES {SharedObj.Error};
        getSpace (id: EventSpaceID.T): EventConn.T RAISES {SharedObj.Error};
    END;

  T = NetObj.T OBJECT
      METHODS
        (* So we can change the default sequencer. *)
        setDfltSequencer(seq: T) RAISES {SharedObj.Error, NetObj.Error,
                                         Thread.Alerted};
        getDfltSequencer(): T RAISES {SharedObj.Error, NetObj.Error,
                                         Thread.Alerted};

        (* Get a sequencer for an object. *)
        getSequencer(wrep: SharedObjRep.WireRep): T 
            RAISES {SharedObj.Error, NetObj.Error, Thread.Alerted};

        (* So other spaces can get our space identifier. *)
        space (): EventSpaceID.T RAISES {NetObj.Error, Thread.Alerted};

        (* So other spaces can get our space ip endpoint. *)
        endpoint (): IP.Endpoint RAISES {NetObj.Error, Thread.Alerted};

        (* Used by newSpace to connect to another space. *)
        connect (from: T)
                 RAISES {Thread.Alerted, NetObj.Error, SharedObj.Error};

        (* Disconnect from a space. *)
        disconnect (id: EventSpaceID.T)
                   RAISES {Thread.Alerted, NetObj.Error, SharedObj.Error};

        (* Register a new object.  Called by the space that created
           the object when we are to be its sequencer and it is going
           to be passed to another machine for the first time. *)
        newObject (id: EventSpaceID.T; wrep: SharedObjRep.WireRep;
                   seqNo: SharedObj.SequenceNumber)
                   RAISES {SharedObj.Error, NetObj.Error, Thread.Alerted};

        (* Register a new copy of an object at a given "seqNo", being
           sequenced by "seq".  The sequencer will return the next
           sequence number it will send.  If it is more than one
           greater than the "seqNo" argument, the caller should make
           other arangements to get the intermediate events, or to get
           a more recent copy of the object. *)
        newCopy (id   : EventSpaceID.T; seq  : T;
                 wrep : SharedObjRep.WireRep;
                 seqNo: SharedObj.SequenceNumber): SharedObj.SequenceNumber
                 RAISES {SharedObj.Error, NetObj.Error, Thread.Alerted};

        (* Notify the sequencer that we no longer have a copy of this
           object. *)
        deleteCopy (id: EventSpaceID.T; wrep: SharedObjRep.WireRep)
                    RAISES {SharedObj.Error, NetObj.Error, Thread.Alerted};

        (* Notify this space that it has the last copy of an object.
           The last sequence number sent out is passed.  For all
           intents and purposes, the object is now back to its initial
           state, where the space does the sequencing.  For example,
           when it passes off another copy, it must reregister it with
           the sequencer as if the object were new. *)
        lastCopy (wrep: SharedObjRep.WireRep; seqNo: SharedObj.SequenceNumber)
                  RAISES {SharedObj.Error, NetObj.Error, Thread.Alerted};

        (* When we need to get a copy of a shared object that is at least
           as resent as seqNo. *)
        get (obj: SharedObjRep.WireRep; 
             seqNo: SharedObj.SequenceNumber) :  SharedObj.T
             RAISES {Thread.Alerted, NetObj.Error, SharedObj.Error};

        (* when we receive an object in an update message, we only get
           its WireRep, and the NetObj.WRep and NetObj.Address of its
           sequencer.  We call findObj, which will call us back with a
           space to try asking for the object, and the sequence number
           we should get from that space. If proc returns TRUE, it
           means the object was successfully retrieved, and findObj
           will return.  Otherwise, findObj will try each space it
           knows about in turn, followed by asking the sequencer of
           the object for each space it knows about.  When findobj
           returns, the object should exist locally.  If not, findObj
           failed. *)
        findObj(obj: SharedObjRep.WireRep;
                cbobj: FindObjCallBack) RAISES {Thread.Alerted,
           NetObj.Error, SharedObj.Error}; 

        (* Methods for handling locking, changing ownership, etc.  They all
           follow the model that the sequencer will forward the call if it
           is not the sequencer, or fail if it doesn't know who to forward
           to.  It will return the sequence number of the last event sent
           out before the call was processed.  Thus, each caller should
           immediately wait for the appropriate event to finish. *)

        (* So a sequencer can control its subsequencers, telling them they
           should send updates for certain objects to certain spaces. *)
        (*
        sendTo (wrep: SharedObjRep.WireRep; to: EventSpaceID.T)
                RAISES {Thread.Alerted, NetObj.Error, SharedObj.Error};
        stopSendingTo (wrep: SharedObjRep.WireRep; to: EventSpaceID.T)
                RAISES {Thread.Alerted, NetObj.Error, SharedObj.Error};
        *)
        printState() RAISES {Thread.Alerted, NetObj.Error};
        (* Print a pile of output to stdout describing the state of
           the shared object runtime. *)
      END;

TYPE
  FindObjCallBack = NetObj.T OBJECT METHODS
    try(seqNo: SharedObj.SequenceNumber; space: T) 
      RAISES {Thread.Alerted, NetObj.Error, SharedObj.Error};
  END;

END ObjectSpace.
