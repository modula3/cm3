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
 * Created On      : Wed May 24 10:28:43 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Jul  9 15:50:09 1998
 * Update Count    : 456
 * 
 * $Source: /opt/cvs/cm3/m3-comm/sharedobj/src/SharedObjRT.m3,v $
 * $Date: 2001-12-02 13:41:17 $
 * $Author: wagner $
 * $Revision: 1.2 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.1.1.1  2001/12/02 13:14:14  wagner
 * Blair MacIntyre's sharedobj package
 *
 * Revision 1.8  1998/07/14 02:34:02  bm
 * tried to fix the distribution problems -- not yet working, but closer
 *
 * Revision 1.7  1998/07/02 21:43:37  bm
 * small bug fixes
 *
 * Revision 1.6  1998/05/11 02:34:16  bm
 * bug fixes, added SharedObj.Wait
 *
 * Revision 1.5  1997/10/24 19:32:57  bm
 * Added the ability to flush the incoming updates.
 *
 * Revision 1.4  1997/03/12 21:50:41  bm
 * Bug fix.
 *
 * Revision 1.3  1997/01/23 15:27:17  bm
 * Lot's of little bug fixes.
 *
 * Revision 1.2  1996/10/07 19:24:18  bm
 * Numerous bug fixes.
 * - added support for passing SharedObj/NetObj parameters!
 * - tidied up to support sharedobjgen
 *
 * 
 * HISTORY
 *
 *  DONE.
 *   - the signal waiting stuff is dangerous.  Need a way to make sure
 *     we only signal the correct thread once!!!!!
 *   - fix to allow passing of shared objects
 *
 *  TODO
 *   - add code to pickler to allow passing of new copy of failed
 *     object
 *   - add code for globallock(), own()
 *   - remove connections to spaces we no longer communicate with
 *     (either time delay or garbage collect)
 *   - change pickler to not pass object unless needed!
 *   - handle sequencer crashes
 *   - when two or more threads try creating an object at similar
 *     times, which one gets to put it in the table?  Furthermore, how
 *     do we handle the different sequence numbers?  Can probably just
 *     have the first one to register the set up object win, and if
 *     it's not new enough, have the other ones wait until it is.
 *)

MODULE SharedObjRT EXPORTS SharedObj, SharedObjRT, SharedObjRTF,
                           SharedObjError;

IMPORT Atom, AtomList, NetObj, Rd, Wr, Thread, EventWireRep,
       Text, EventCounter, EventStubLib, ObjectInfo, IO, SharedObjRep,
       EventConnList, EventConnListFuncs, Debug, RdWrMutex, 
       EventPort, Message, ObjectInfoTbl, SpaceConn,
       EventNumber, EventNumberF, WeakRef, ThreadF, 
       ObjectSpace, Event, ObjCopy, ObjCopyList, ObjCopyListFuncs,
       Process, Fmt, EventProtocol, EventSeq; 

FROM SharedObjRep IMPORT WireRep;

(* A handle is used during event construction to hold the information
   needed to build the event. *)

CONST Protocol: EventProtocol.StubProtocol = 1;

(* The protocol implemented by this module. *)

(* Make this routine safe to be called repeatedly *)
PROCEDURE Init(self: T): T =
  BEGIN
    IF debug_level >= 1 THEN
      debug.print(1, "Initializing Sharedobj");
    END;
    IF self.mu = NIL THEN
      self.mu := NEW(RdWrMutex.T).init();
    END;
    IF self.wrep = EventWireRep.NullT THEN
      self.wrep := EventWireRep.New();
    END;
    IF self.seqNoCnt = NIL THEN
      (* The next seqNo is not zero! *)
      self.seqNoCnt := EventCounter.New(self.mu, oneSeqNo);
    END;
    IF debug_level >= 1 THEN
      debug.print(1, "New SharedObj: " & SharedObjRep.ToText(self));
    END;
    RETURN self;
  END Init;

PROCEDURE NewMessage(ev: Event.T; h: EventStubLib.Handle; thread: INTEGER; 
                     objInfo: ObjectInfo.T): Message.T = 
  VAR pm: Message.T;
  BEGIN
    LOCK msgMu DO
      IF freeMessage # NIL THEN
        pm := freeMessage; freeMessage := freeMessage.next;
      ELSE
        pm := NEW(Message.T, next := NIL);
      END;
    END;

    pm.ev := ev;
    pm.h := h;
    pm.thread := thread;
    pm.objInfo := objInfo;

    RETURN pm;
  END NewMessage; 

PROCEDURE FreeMessage(m: Message.T) =
  BEGIN
    LOCK msgMu DO
      m.ev := NIL;
      m.objInfo := NIL;
      m.h := NIL;

      m.next := freeMessage;
      freeMessage := m;
    END;
  END FreeMessage;

PROCEDURE DebugLevel(p: INTEGER) =
  BEGIN 
    (* Who cares if someone is executing right when this changes.  No
       big deal. 
       We will use level 1 for external entry points, 10 for internal
       entry points.  
    *)
    debug := NEW(Debug.T).init("SharedObj", p);
    debug_level := p;
  END DebugLevel;

(****************************************************************************
 * for adding and removing host connections 
 ****************************************************************************)
PROCEDURE EventPortConnect(conn: SpaceConn.T) RAISES {Error} =
  BEGIN
    TRY
      ep.connect(conn);
    EXCEPT
    | Event.Error(al) => RaiseEventFailure(al);
    END;
  END EventPortConnect;

PROCEDURE EventPortDisconnect (conn: SpaceConn.T): EventSeq.T RAISES {Error} =
  VAR wrep: WireRep;
      objInfo: ObjectInfo.T;
      copy := NEW(ObjCopy.T, conn := conn);
  BEGIN
    (* Need to iterate through all objects and remove all traces of
       the connection we are dropping. *)
    IF debug_level >= 10 THEN
      debug.print(10, "EventPortDisconnect: scanning for " & 
        "objects with connections to the invalid port.");
    END;
    WITH iterator = objTbl.iterate() DO
      WHILE iterator.next(wrep, objInfo) DO
        LOCK objInfo DO
          (* If this obj's sequencer connection has been lost, mark it
             as invalid *)
          IF objInfo.sequencer = conn THEN
            MarkObjectInvalid(objInfo);
          END;

          IF ObjCopyListFuncs.DeleteD(objInfo.clients, copy) # NIL THEN
            (* Delete the conn, if there is one, in the fastClient list.  If
               it's not a fastClient, it will be in the conns list, so
               delete it from there. *)

            IF ObjCopyListFuncs.DeleteD(objInfo.fastClients, copy) = NIL THEN
              IF EventConnListFuncs.DeleteD(objInfo.conns, conn) = NIL THEN
              END;
            END;

            IF debug_level >= 10 THEN
              debug.print(10, "EventPortDisconnect: removed client.");
            END;
            (*****************************************************)
            (* NEED TO CHECK FOR LOCK and OWNER and do something *)
            (* reasonable!!!!!!!                                 *)
            (*****************************************************)
            <* ASSERT objInfo.lock = NIL *>
            <* ASSERT objInfo.owner = NIL *>

            CheckForLastClient(objInfo);
          END;
        END;
      END;
    END;
    IF debug_level >= 10 THEN
      debug.print(10, "EventPortDisconnect: done scanning for " & 
        "objects with connections to the invalid port.");
    END;

    TRY
      RETURN ep.disconnect(conn);
    EXCEPT
    | Event.Error(el) => RaiseEventFailure(el);
    (* To shut up the compiler. *) RETURN NIL;
    END;
  END EventPortDisconnect;

(****************************************************************************
 * some utility routines 
 ****************************************************************************)
PROCEDURE MarkObjectInvalid(objInfo: ObjectInfo.T; obj: T := NIL) =
  BEGIN
    IF obj = NIL THEN
      obj := GetObjRef(objInfo);
    END;

    IF obj # NIL THEN
      IF debug_level >= 10 THEN
        debug.print(10, "MarkObjectInvalid: marking " & 
          SharedObjRep.ToText(obj) & " invalid.");
      END;
      obj.ok := FALSE;
      
      (* Awake all the sleepers, who will then raise errors *)
      WITH blocked = objInfo.waiting DO
        FOR i := FIRST(blocked^) TO LAST(blocked^) DO
          IF blocked[i].used THEN
            IF debug_level >= 10 THEN
              debug.print(10, "MarkObjectInvalid: signalling thread slot " & 
                Fmt.Int(i) & ".");
            END;
            Thread.Signal(blocked[i].cv);
          END;
        END;
      END;
    END;
  END MarkObjectInvalid;

PROCEDURE GetObjRef(objInfo: ObjectInfo.T): T =
  BEGIN
    IF objInfo.hasCopy THEN
      RETURN WeakRef.ToRef(objInfo.obj);
    END;
    RETURN NIL;
  END GetObjRef;

PROCEDURE GetObjInfo(wrep: WireRep; VAR objInfo: ObjectInfo.T): BOOLEAN =
  BEGIN
    RETURN objTbl.get(wrep, objInfo);
  END GetObjInfo;

(****************************************************************************
 * dealing with sequencers
 ****************************************************************************)
PROCEDURE SequencerFailed(<*UNUSED*>seq: SpaceConn.T; ec: AtomList.T) =
  BEGIN
    Debug.Crash("Sequencer Failure", ec);
  END SequencerFailed;

(* We are a sequencer for an object if we are a sequencer and
   someone else is not this objects sequencer. *)
PROCEDURE IsObjSequencer(objInfo: ObjectInfo.T): BOOLEAN =
  BEGIN
    RETURN isSequencer AND objInfo.sequencer = NIL;
  END IsObjSequencer; 

(****************************************************************************
 * use weak refs to cleanup shared objects 
 ****************************************************************************)
TYPE
  ObjectCleanerClosure = Thread.Closure OBJECT 
    mu: Thread.Mutex;
    cv: Thread.Condition;
    tbl: ObjectInfoTbl.T;
    it: ObjectInfoTbl.Iterator;
  METHODS
    init (): ObjectCleanerClosure := InitDeadObject;
    addObject (wrep: EventWireRep.T; o: ObjectInfo.T) := AddDeadObject;
  OVERRIDES
    apply := ObjectCleaner;
  END;

PROCEDURE InitDeadObject (self: ObjectCleanerClosure):
  ObjectCleanerClosure =
  BEGIN
    self.mu := NEW(Thread.Mutex);
    self.cv := NEW(Thread.Condition);
    self.tbl := NEW(ObjectInfoTbl.Default).init();
    self.it := self.tbl.iterate();
    RETURN self;
  END InitDeadObject;

PROCEDURE AddDeadObject (self: ObjectCleanerClosure; 
                         wrep: EventWireRep.T; o: ObjectInfo.T) =
  BEGIN
    LOCK self.mu DO
      <*ASSERT self.tbl.put(wrep, o) = FALSE*>
      Thread.Broadcast(self.cv);
    END;
  END AddDeadObject;

PROCEDURE ObjectCleaner (self: ObjectCleanerClosure): REFANY =
  VAR 
    obj: ObjectInfo.T;
    wrep: EventWireRep.T;
  BEGIN
    LOOP
      LOCK self.mu DO
        WHILE self.tbl.size() = 0 DO
          Thread.Wait(self.mu, self.cv);
        END;
        self.it := self.it.init();
        <*ASSERT self.it.next(wrep, obj)*>
        <*ASSERT self.tbl.delete(wrep, obj)*>
      END;

      (* Note that the weakref is no longer valid. *)
      obj.hasCopy := FALSE;
      IF debug_level >= 5 THEN
        debug.print(5, "ObjectCleaner: starting.");
      END;
      IF NOT IsObjSequencer(obj) THEN
        (* We are not this objects sequencer. 
           If obj.fastClient # NIL, the sequencer will know this and
           handle it, so we don't need to worry about it. *)
        IF obj.sequencer # NIL THEN
          TRY
            IF debug_level >= 5 THEN
              debug.print(5, 
                          "ObjectCleaner: deleting from custom sequencer.");
            END;
            obj.sequencer.objSpace.deleteCopy(localSpace.space(), 
                                              wrep);
            IF debug_level >= 5 THEN
              debug.print(5,
                          "ObjectCleaner: deleted from custom sequencer.");
            END;
          EXCEPT
          | NetObj.Error(ec) => SequencerFailed(obj.sequencer, ec);
          | Error(ec) => SequencerFailed(obj.sequencer, ec);
          | Thread.Alerted => SequencerFailed(obj.sequencer, NIL);
          END;
        ELSIF sequencer # NIL THEN
          TRY
            IF debug_level >= 5 THEN
              debug.print(5, 
                          "ObjectCleaner: deleting from default sequencer.");
            END;
            sequencer.objSpace.deleteCopy(localSpace.space(), wrep);
            IF debug_level >= 5 THEN
              debug.print(5,
                          "CleanupSharedObj: deleted from default sequencer.");
            END;
          EXCEPT
          | NetObj.Error(ec) => SequencerFailed(sequencer, ec);
          | Error(ec) => SequencerFailed(sequencer, ec);
          | Thread.Alerted => SequencerFailed(sequencer, NIL);
          END;
        END;
        (* We've notified the sequencer, if needed, and the last
           reference to this is gone.  Get rid of all traces! *)
      ELSE
        CheckForLastClient(obj);
      END;
    END;
  END ObjectCleaner;

PROCEDURE CleanupSharedObj(<*UNUSED*> READONLY w: WeakRef.T; r: REFANY) =
  VAR obj := NARROW(r, T);
      o: ObjectInfo.T;
  BEGIN
    (* Have to do all of this within the localspace lock.
       MIGHT CHANGE THIS:  if one of the netobj calls hangs, we will
       lock out the localspace for a _long_ time! *)
    IF debug_level >= 1 THEN
      debug.print(1, "CleanupSharedObj: begin!");
    END;
    LOCK objTblMu DO
      IF NOT GetObjInfo(obj.wrep, o) THEN
        IF debug_level >= 1 THEN
          debug.print(1, "CleanupSharedObj: Already clean.  Return!");
        END;
        RETURN;
      END;

      LOCK o DO
        WITH realObj = GetObjRef(o) DO
          IF realObj # NIL AND realObj # obj THEN
            IF debug_level >= 1 THEN
              debug.print(1, "CleanupSharedObj: cleaning a fake copy! Return");
            END;
            RETURN;
          END;
        END;
        
        EVAL objTbl.delete(obj.wrep, o);
        objCleaner.addObject(obj.wrep, o);
        IF debug_level >= 5 THEN
          debug.print(5, "CleanupSharedObj: deleted object info.");
        END;
      END;
    END;
    IF debug_level >= 1 THEN
      debug.print(1, "CleanupSharedObj: end.");
    END;
  END CleanupSharedObj;

PROCEDURE RegisterObject(obj: T; wrep: WireRep;
                         sequencer: SpaceConn.T; standAlone: BOOLEAN; 
                         seqNo: SequenceNumber): ObjectInfo.T =
  VAR objInfo: ObjectInfo.T;
      objRef := WeakRef.T{ARRAY [0..7] OF BITS 8 FOR [0..255] {0, ..}}; 
      hasCopy := FALSE;
  BEGIN
    (* Put it in the local table. *)
    IF obj # NIL THEN
      objRef := WeakRef.FromRef(obj, CleanupSharedObj);
      hasCopy := TRUE;
    END;
    objInfo := NEW(ObjectInfo.T, obj := objRef,
                   hasCopy := hasCopy, sequencer := sequencer,
                   isStandalone := standAlone,
                   wrep := wrep).init(seqNo);
    EVAL objTbl.put(wrep, objInfo);
    IF debug_level >= 10 THEN
      debug.print(10, "RegisterObject: object registered.");
    END;
    RETURN objInfo;
  END RegisterObject;

(***************************************************************************
 * Adding and removing a client for an object 
 ***************************************************************************)
PROCEDURE AddClient(objInfo: ObjectInfo.T; 
                    conn: SpaceConn.T) RAISES {Error} =
  BEGIN
    WITH copy = NEW(ObjCopy.T, conn := conn) DO
      IF ObjCopyList.Member(objInfo.clients, copy) THEN
        RaiseError(Atom.FromText("Object doesn't exist but in client list??"));
      END;
      objInfo.clients := ObjCopyList.Cons(copy, objInfo.clients);
      objInfo.conns := EventConnList.Cons(conn, objInfo.conns);
      IF debug_level >= 10 THEN
        debug.print(10, "AddClient: added to clients and conns.");
      END;
    END;
  END AddClient;

PROCEDURE DeleteClient(objInfo: ObjectInfo.T; conn: SpaceConn.T) 
  RAISES {Error} =
  BEGIN
    WITH copy = NEW(ObjCopy.T, conn := conn) DO
      DeleteCopy(objInfo, conn, copy);
    END;
  END DeleteClient;

PROCEDURE DeleteCopy(objInfo: ObjectInfo.T; conn: SpaceConn.T;
                     copy: ObjCopy.T) 
  RAISES {Error} =
  BEGIN
    IF ObjCopyListFuncs.DeleteD(objInfo.clients, copy) = NIL THEN
      RaiseError(Atom.FromText("Object exists but isn't in client list??"));
    END;

    (* Delete the conn, if there is one, in the fastClient list.  If
       it's not a fastClient, it will be in the conns list, so
       delete it from there. *)

    IF ObjCopyListFuncs.DeleteD(objInfo.fastClients, copy) = NIL THEN
      IF EventConnListFuncs.DeleteD(objInfo.conns, conn) = NIL THEN
        RaiseError(Atom.FromText("Object exists but isn't in conns list??"));
      END;
    END;

    IF debug_level >= 10 THEN
      debug.print(10, "DeleteCopy: removed client.");
    END;

    (*****************************************************)
    (* NEED TO CHECK FOR LOCK and OWNER and do something *)
    (* reasonable!!!!!!!                                 *)
    (*****************************************************)
    <* ASSERT objInfo.lock = NIL *>
    <* ASSERT objInfo.owner = NIL *>

    CheckForLastClient(objInfo);
  END DeleteCopy;

(***************************************************************************
 ***************************************************************************
 ** Handling of incoming messages 
 ***************************************************************************
 ***************************************************************************)

PROCEDURE MethodDispatcher (ev: Event.T; <*UNUSED*> data: REFANY)
  RAISES {Thread.Alerted} =
  VAR
    h       : EventStubLib.Handle;
    wrep    : WireRep;
    objInfo : ObjectInfo.T;
    thread  : INTEGER;
    m       : Message.T;
    msgApplied: BOOLEAN := TRUE;
  BEGIN
    TRY
      IF debug_level >= 12 THEN
        debug.print(12, "MethodDispatcher: new event (" & 
          Event.ToText(ev) & ")");
      END;

      h := EventStubLib.StartRead(ev);
      EventStubLib.InBytes(h, wrep.byte);
      LOCK objTblMu DO
        (* If we don't have any info for this object, return. *)
        IF NOT GetObjInfo(wrep, objInfo) THEN
          IF debug_level >= 12 THEN
            debug.print(12, "MethodDispatcher: non-existant object.  done.");
          END;
          RETURN;
        END;
      END;

      thread := EventStubLib.InInteger(h);
      IF debug_level >= 15 THEN
        debug.print(15, "MethodDispatcher: distributing event");
      END;

      TRY
        m := NewMessage(ev, h, thread, objInfo);
        WITH obj = DistributeMsg(m) DO
          IF obj # NIL THEN
            msgApplied := ApplyMsg(m, obj);
          END;
        END;
      FINALLY
        FreeMessage(m);
        IF msgApplied THEN
          EventStubLib.EndRead(h);
        END;
      END;

      IF debug_level >= 15 THEN
        debug.print(15, "MethodDispatcher: done distributing event");
      END;
    EXCEPT
    | Error (al) => 
      Debug.PrintAtomList("Dispatcher SharedObj.Error", al);
    | Event.Error (al) => Debug.PrintAtomList("Dispatcher Event.Error", al);
    | Rd.Failure (al) => Debug.PrintAtomList("Dispatcher Rd.Failure", al);
    | Wr.Failure (al) => Debug.PrintAtomList("Dispatcher Wr.Failure", al);
    END;
  END MethodDispatcher; 

(* Incoming message are distributed here. *)
PROCEDURE DistributeMsg(m: Message.T) : T
        RAISES {Error, Thread.Alerted} =
  VAR obj: T := NIL;
  BEGIN
    (********************************************)
    (* We Don't handle owning or locking yet!!! *)
    (* Just sequence and forward ...            *)
    (********************************************)
    IF debug_level >= 12 THEN
      debug.print(12, "DistributeMsg: message received [" & 
      Message.ToText(m) & "]");
    END;
    LOCK m.objInfo DO
      TRY
        (* If the seqNo is zero, this is unsequenced.  Sequence it. *) 
        IF m.ev.num.compare(zeroSeqNo) = 0 THEN
          IF NOT SequenceMsg(m) THEN
            (* If it is still not sequenced, return.  We have sent it
               to the sequencer and will handle it upon return. *)
            RETURN NIL;
          END;
        END;
	<* ASSERT m.ev.num.compare(zeroSeqNo) # 0 *>
  
	(* Update to the local object if present.  First, get a
           tangible reference to it.  If this is a sequenced reply to
           a message we sent, signal the waiting thread and return. *) 
        obj := GetObjRef(m.objInfo);

	IF obj # NIL THEN
          IF m.ev.from = localSpace.space() THEN
            (* If we sent this out, and its sequenced, we should have
               a thread blocked waiting on it!  If not, and the object
               is not ok, we'll just drop the event. Otherwise, we'll
               forward the messages.  *)
            IF debug_level >= 15 THEN
              debug.print(15, "DistributeMsg: our message! Signalling thread");
            END;
            IF SignalWaiting(m) THEN RETURN NIL END;
            IF NOT obj.ok THEN RETURN NIL END;
          END;
	END;

        (* Forward this message to our clients.  Do this after the
           SignalWaiting, since it should only be done it one place! *)
	IF m.objInfo.conns # NIL THEN
          IF debug_level >= 15 THEN
            debug.print(15, "DistributeMsg: sending message to clients.");
          END;
	  ep.mcast(m.objInfo.conns, m.ev);
	END;
      EXCEPT
      | Event.Error(ec) => RaiseEventFailure(ec);
      | NetObj.Error(ec) => RaiseNetObjFailure(ec);
      END;
    END;

    IF debug_level >= 12 THEN
      debug.print(12, "DistributeMsg: done.");
    END;

    (* Return the local object, if there is one. *)
    RETURN obj;
  END DistributeMsg;

PROCEDURE SignalWaiting(m: Message.T): BOOLEAN =
  BEGIN
    (* blocked.used should normally be TRUE, it's in use.  The blocked
       thread will reset it to FALSE when it runs.  Furthermore, the
       blocked entry maintains the eventnumber that was last woken on
       it.  If the entry in the event is no longer used, or it is used
       by has an event number at least as big as the incoming event,
       then the event has already woken the thread and thus this event
       is a duplicate OR the blocked thread was Alerted.  In the
       latter case, we will end up trying to apply the event again.
       Chances are, it's been applied so we will get a "Duplicate"
       error when we try to grab the EventCounter.  That's cool. *)
    WITH blocked = m.objInfo.waiting[m.thread] DO
      IF NOT blocked.used OR blocked.en.compare(m.ev.num) >= 0 THEN
        IF debug_level >= 12 THEN
          debug.print(12, "SignalWaiting: thread " & 
          Fmt.Int(m.thread) & " has already been signalled or alerted.");
        END;
        RETURN FALSE;
      ELSE
        IF debug_level >= 12 THEN
          debug.print(12, "SignalWaiting: signalling thread " & 
          Fmt.Int(m.thread) & ".");
        END;
        EVAL blocked.en.init(m.ev.num);
        Thread.Signal(blocked.cv);
      END;
    END;
    RETURN TRUE;
  END SignalWaiting;

(**************************************************************************
 * Incoming message are applied here.
 * Either apply now, or defer it till later using a HandlerEvent 
 **************************************************************************)

PROCEDURE ApplyMsg (m: Message.T; obj: T): BOOLEAN
  RAISES {Error, Rd.Failure, Wr.Failure, Thread.Alerted} =
  BEGIN
    IF debug_level >= 12 THEN
      debug.print(12,
                  "ApplyMsg: message received [" & Message.ToText(m) & "].");
    END;

    (* see if we are in sequence. *)
    TRY
      IF obj.seqNoCnt.tryAcquire(m.ev.num) THEN
        IF debug_level >= 15 THEN
          debug.print(15, "ApplyMsg: acquired counter, applying.");
        END;

        LOCK m.objInfo DO
          (* Now, the actual object might have changed while we were
             blocked, so we will get it again! *)
          obj := GetObjRef(m.objInfo);
          IF obj = NIL THEN
            RaiseError(Atom.FromText("DistributedMsg: Obj Disappeared!"));
          END;
            
          (* Set the sequence number in the objinfo struct to
             reflect this latest successful update.  The
             objInfo.seqNo should be the same as seqNoCnt, after the
             release below. *) 
          IF NOT IsObjSequencer(m.objInfo) THEN
            TRY
              m.objInfo.seqNo.init(m.ev.num).inc();
            EXCEPT
            | EventNumber.Overflow =>
            END;
          END;
        END;
        IF debug_level >= 12 THEN
          debug.print(12, "ApplyMsg: trying to applyUpdate().");
        END;

        TRY
          TRY
            obj.applyUpdate(m.ev, m.h);

            IF debug_level >= 15 THEN
              debug.print(15, "ApplyMsg: applyUpdate() succeeded.");
            END;
          FINALLY
            obj.seqNoCnt.release();
          END;
        EXCEPT
        | Fatal (ec) =>
          (* obj.ok := FALSE;*)
          MarkObjectInvalid(m.objInfo, obj);
          RAISE Error(AtomList.Cons(DeadObject, ec));

          (*
          IO.Put("*** FATAL Error called object method.\n");
            Debug.PrintAtomList(
              "    Add code to Pickle reader so we can recover!!\n", ec);
          *)
        | Event.Error (ec) => RaiseEventFailure(ec);
        END;
        IF debug_level >= 12 THEN debug.print(12, "ApplyMsg: done."); END;
        RETURN TRUE;
      ELSE
        IF debug_level >= 11 THEN
          debug.print(11, "ApplyMsg: couldn't acquire, enqueueing event.");
          IF debug_level = 11 THEN
            debug.print(11, "ApplyMsg: event " & Event.ToText(m.ev));
          END;
        END;

        WITH handler = NEW(EventHandler, objInfo := m.objInfo, ev := m.ev,
                           h := m.h) DO
          obj.seqNoCnt.enqueueAction(m.ev.num, handler);
        END;
        IF debug_level >= 12 THEN debug.print(12, "ApplyMsg: done."); END;
        RETURN FALSE;
      END;
    EXCEPT
    | EventCounter.Duplicate =>
        (* Ignore duplicates.  output something now, since we shouldn't get
           any with TCP! *)
        IF debug_level >= 15 THEN
          debug.print(15, "*** Duplicate event " & Event.ToText(m.ev)
                            & " with #" & obj.seqNoCnt.value().fmt());
        END;
        RETURN TRUE;
    END;
  END ApplyMsg;

TYPE
  EventHandler = EventCounter.Handler OBJECT 
    objInfo: ObjectInfo.T;
    ev: Event.T;
    h: EventStubLib.Handle;
  OVERRIDES
    handle := HandlerHandleMethod;
    duplicate := HandlerDuplicateMethod;
  END;
    
PROCEDURE HandlerHandleMethod (self: EventHandler) =
  VAR obj: T;
  BEGIN
    IF debug_level >= 11 THEN
      debug.print(11, "MsgHandlerMeth: enqueued event applying.");
    END;

    (* Now, the actual object might have changed while we were
       blocked, so we will get it again! *)
    LOCK self.objInfo DO
      obj := GetObjRef(self.objInfo);
      IF obj = NIL THEN
        EventStubLib.EndRead(self.h);
        RETURN;
      END;

      (* Set the sequence number in the objinfo struct to
         reflect this latest successful update *)
      IF NOT IsObjSequencer(self.objInfo) THEN
        TRY
          self.objInfo.seqNo.init(self.ev.num).inc();
        EXCEPT
        | EventNumber.Overflow =>
        END;
      END;
    END;

    IF debug_level >= 15 THEN
      debug.print(15, "MsgHandlerMeth: trying to applyUpdate().");
    END;

    TRY
      TRY
        obj.applyUpdate(self.ev, self.h); 
      FINALLY
        EventStubLib.EndRead(self.h);
      END;
      IF debug_level >= 15 THEN
        debug.print(15, "MsgHandlerMeth: applyUpdate() succeeded.");
      END;
    EXCEPT
    | Fatal (ec) => 
      (* obj.ok := FALSE;*)
      MarkObjectInvalid(self.objInfo, obj);
      Debug.PrintAtomList("MsgHandler SharedObj.Fatal", ec);

      (*
      IO.Put("*** FATAL Error called object method.\n");
      Debug.PrintAtomList("  Add code to Pickle reader so we can recover!!\n", 
                          ec);
      *)
    | Error (al) => Debug.PrintAtomList("MsgHandler SharedObj.Error", al);
    | Event.Error (al) => Debug.PrintAtomList("MsgHandler Event.Error", al);
    | Rd.Failure (al) => Debug.PrintAtomList("MsgHandler Rd.Failure", al);
    | Wr.Failure (al) => Debug.PrintAtomList("MsgHandler Wr.Failure", al);
    | Thread.Alerted => IO.Put("MsgHandler Thread.Alerted\n");
    END;
  END HandlerHandleMethod; 

PROCEDURE HandlerDuplicateMethod (self: EventHandler) =
  BEGIN
    IF debug_level >= 10 THEN
      debug.print(10, "DuplicateMethod: enqueued event is a duplicate.");
    END;
    EventStubLib.EndRead(self.h);
  END HandlerDuplicateMethod;

(**************************************************************************
 * sequencing of updates 
 **************************************************************************)
PROCEDURE SequenceMsg(m: Message.T): BOOLEAN RAISES {Error} =
  BEGIN
    IF debug_level >= 12 THEN
      debug.print(12, "SequenceMsg: begin.");
    END;
    TRY
      IF m.objInfo.sequencer # NIL THEN
        (* We are a sequencer, but not for this object. 
           Forward it to its sequencer. *)
        IF debug_level >= 15 THEN
          debug.print(15, "SequenceMsg: message unsequenced, fwd to obj seq.");
        END;
        ep.send(m.objInfo.sequencer, m.ev);
        IF debug_level >= 12 THEN
          debug.print(12, "SequenceMsg: fwded to obj seq.");
        END;
        RETURN FALSE;
      ELSIF sequencer # NIL THEN
        (* We are not a sequencer, and we have had our sequencer
           set.  Send it off to its sequencer. *)
        IF debug_level >= 15 THEN
          debug.print(15, "SequenceMsg: message unsequenced, fwd to dflt seq");
        END;
        ep.send(sequencer, m.ev);
        IF debug_level >= 12 THEN
          debug.print(12, "SequenceMsg: fwded to dflt seq.");
        END;
        RETURN FALSE;
      ELSIF isSequencer THEN
        (* Sequence ourselves and continue. *)
        IF debug_level >= 15 THEN
          debug.print(15, "SequenceMsg: Distributing message locally.");
        END;

        (* The current value of seqNo is the next sequence number! *)
        EventStubLib.ChangeNumber(m.ev, m.objInfo.seqNo);

        IF debug_level >= 12 THEN
          debug.print(12, "SequenceMsg: assigned seqNo " & 
            m.objInfo.seqNo.fmt(10) & " to message");
        END;

        TRY
          m.objInfo.seqNo.inc();
        EXCEPT
        | EventNumber.Overflow => 
          Process.Crash("Event Number Overflowed!");
        END;
      ELSE
        RaiseError(
            Atom.FromText("SequenceMsg: Obj shared, but no sequencer."));
      END;
    EXCEPT
    | Event.Error(ec) => RaiseEventFailure(ec);
    END;
    IF debug_level >= 12 THEN
      debug.print(12, "SequenceMsg: end.");
    END;
    RETURN TRUE;
 END SequenceMsg;

PROCEDURE SequenceCall(obj: T; ev: Event.T; thread: INTEGER; 
                       objInfo: ObjectInfo.T; en: SequenceNumber;
                       VAR alerted: BOOLEAN) 
  RAISES {Error} =
  BEGIN
    alerted := FALSE; (* Can't be alerted for now! *)
    TRY
      (* Need the TRY in case AlertWait is Alerted. *)
      VAR
        m: Message.T := NewMessage(ev, NIL, thread, objInfo);
        sequenced: BOOLEAN;
      BEGIN
        TRY
          IF debug_level >= 15 THEN
            debug.print(15, "SequenceCall: message created [" & 
              Message.ToText(m) & "]");
          END;

          LOCK objInfo DO
            (* If the object died between the start of our method call
               and here, raise an exception! *)
            IF NOT obj.ok THEN RaiseDeadObject() END;
            
            sequenced := SequenceMsg(m);

            IF NOT sequenced THEN
              (* Wait for reply. *)
              IF debug_level >= 12 THEN
                debug.print(12, "SequenceCall: forwarded, threadID " &
                  Fmt.Int(ThreadF.MyId()) & " waiting as thread " & 
                  Fmt.Int(m.thread));
              END;
	    
              (* Just before going to sleep, we must tell the event
                 port that this thread is no longer available for
                 handling incoming events, if it is such a thread *)
              EVAL ep.stealWorker();

              obj.mu.wait(objInfo, m.objInfo.waiting[m.thread].cv);
              (* If we are Alerted, we will just return from here.  This
                 should be ok. *)
    
              (* If the object died while we were asleep, raise an
                 exception! *)
              IF NOT obj.ok THEN RaiseDeadObject() END;

              IF debug_level >= 12 THEN
                debug.print(12, "SequenceCall: threadID " &
                  Fmt.Int(ThreadF.MyId()) & " (thread " & 
                  Fmt.Int(m.thread) & ") has been woken.");
              END;
          
              (* The event number is put in tht thread slot before we are
                 woken, since I don't want to change the event itself. *)
              EVAL en.init(objInfo.waiting[thread].en);
            ELSE
              (* The event number was sequenced locally, so the event
                 number is in the event itself. *)
              EVAL en.init(ev.num);
            END;
          END;
        FINALLY
          IF debug_level >= 15 THEN
            debug.print(15, "SequenceCall: done, releasing thread slot " &
              Fmt.Int(thread));
          END;
          LOCK objInfo DO
            objInfo.releaseThreadSlot(thread);
          END;
          FreeMessage(m);  
        END;
      END;

      TRY
        LOCK objInfo DO
          IF objInfo.conns # NIL THEN
            IF debug_level >= 15 THEN
              debug.print(15, "SequenceCall: sending message to clients.");
            END;
            ep.mcast(objInfo.conns, ev);
          END;
        END;
      EXCEPT
      | Event.Error => 
        (* Toss this error!  ConnProblem will be notified. *)
        IF debug_level >= 15 THEN
          debug.print(15, "SequenceCall: tossing Event.Error from mcast.");
        END;
      END;
    FINALLY
      ev.dropRef();
    END;
    RETURN;
  END SequenceCall;

(**************************************************************************
 * If a client of the sequencer is deleted, or the copy of the object
 * that resides on the sequencer is deleted, we must check to see if
 * that was the last copy switch the object back to residing only on
 * the machine with that copy!
 **************************************************************************)

PROCEDURE CheckForLastClient(objInfo: ObjectInfo.T) =
  BEGIN
    IF debug_level >= 5 THEN 
      debug.print(5, "CheckForLastClient: begin");
    END;
    (* First check to see if there are any clients.  If not, we are
       either standalone, or the object is completely gone. *)
    IF objInfo.clients = NIL THEN
      IF objInfo.hasCopy THEN
        IF debug_level >= 5 THEN 
          debug.print(5, "CheckForLastClient: Sequencer has last copy!");
        END;
        objInfo.isStandalone := TRUE;
      ELSE
        (* We removed both the last client and the local reference 
           to this object.  Get rid of all traces! *)
        EVAL objTbl.delete(objInfo.wrep, objInfo);
        IF debug_level >= 5 THEN
          debug.print(5, "CheckForLastClient: No clients left!");
        END;
      END;
    ELSIF objInfo.clients.tail = NIL AND NOT objInfo.hasCopy THEN
      (* Last client!  Inform them of it and then delete our info. 
         Won't do it for now!  Just remind me!  We may need to do more
         than the code commented out here, since the last copy client
         may have sent some events we haven't gotten and processed
         yet!   Or, we could have the client wake up the blocked
         threads and sequence them, probably a better solution.  Not
         for now, though. 

         Perhaps have the client count the number of outstanding
         methods and add to their sequence number.  We will add a
         "wait" after that sequence number to remove the object in the
         sequencer, and the client will add a "wait" to revert to
         standalone *)

      (*
      TRY
        objInfo.clients.head.conn.objSpace.lastCopy (objInfo.wrep, 
                                                     objInfo.seqNo);
        (* if the lastCopy command suceeded (which it won't yet!) then
           we will need to delete the object locally eventually, but
           we can't yet because we might be getting messages back that
           need to be forwarded, which require that we have an
           objectInfo structure!   There might also be messages coming
           from this client that need to be returned. 

           To handle this, we will need to have the client (when it
           reaches the object sequence number we currently are at that
           we sent above) send us a message to delete the objectInfo
           structure, since we will then not get any more messages.
        *)
        (*
        EVAL objTbl.delete(obj.wrep, objInfo);
        *)
      EXCEPT
      | SharedObj.Error, NetObj.Error, Thread.Alerted =>
        (* the connection is most likely dead, so just toss this
           puppy! *)
      END;
      *)
      (*
      IO.Put("CheckForLastClient: Only one client left!  ");
      IO.Put("Write the code to revert it to standalone!\n");
      *)
    ELSE
      IF debug_level >= 5 THEN
        debug.print(5, "CheckForLastClient: not last client.");
      END;
    END;
    IF debug_level >= 5 THEN
      debug.print(5, "CheckForLastClient: end");
    END;
  END CheckForLastClient;

(*---------------------Sequencer Control---------------------*)

PROCEDURE LocalSpace(): ObjectSpace.T =
  BEGIN
    RETURN localSpace;
  END LocalSpace;

PROCEDURE SetDfltSequencer (space: ObjectSpace.T) RAISES {Error} = 
  BEGIN
    LOCK objTblMu DO
      IF debug_level >= 1 THEN
        debug.print(1, "SetDfltSequencer: begin.");
      END;
      IF space = localSpace THEN
	IF sequencer # NIL THEN
	  RaiseError(Atom.FromText("Space is not a sequencer!"));
	END;
	isSequencer := TRUE;
        IF debug_level >= 1 THEN
          debug.print(1, "SetDfltSequencer: we are a sequence");
        END;
        RETURN;
      ELSE
	IF isSequencer THEN
	  RaiseError(Atom.FromText("Space is a Sequencer!"));
	END;
	IF sequencer # NIL AND sequencer.objSpace # space THEN
	  RaiseError(Atom.FromText("Sequencer already set!"));
	END;
        (* done below *)
      END;

      (* Ok, so register it with the sequencer.  It might be
         registered already, but that's ok. *)
      IF debug_level >= 1 THEN
        debug.print(1, "SetDfltSequencer: setting sequence");
      END;
      sequencer := localSpace.newSpace(space);

      IF debug_level >= 1 THEN
        debug.print(1, "SetDfltSequencer: done setting sequence. " &
        "Waking sleepers."); 
      END;
      Thread.Broadcast(seqCV);
    END;
  END SetDfltSequencer;

PROCEDURE WaitForSequencer() =
  BEGIN
    IF isSequencer OR sequencer # NIL THEN
      IF debug_level >= 15 THEN
        debug.print(15, "WaitForSequencer: sequencer defined.");
      END;
      RETURN;
    END;

    IF debug_level >= 10 THEN
      debug.print(10, "WaitForSequencer: sequencer not defined. " & 
        "Making ourself a sequencer.");
    END;
    isSequencer := TRUE;
    RETURN;

    (*
    IF debug_level >= 10 THEN
      debug.print(10, "WaitForSequencer: sequencer not defined.  Waiting.");
    END;
    Thread.AlertWait(objTblMu, seqCV);
    IF debug_level >= 10 THEN
      debug.print(10, "WaitForSequencer: sequencer defined.  returning.");
    END;
    *)
  END WaitForSequencer;

PROCEDURE GetDfltSequencer(): ObjectSpace.T =
  BEGIN
    WaitForSequencer();
    IF isSequencer THEN
      RETURN localSpace;
    ELSE
      RETURN sequencer.objSpace;
    END;
  END GetDfltSequencer;

PROCEDURE GetSequencer(wrep: WireRep): ObjectSpace.T RAISES {Error} =
  VAR 
    objInfo: ObjectInfo.T;
  BEGIN
    (* If we aren't a sequencer, ask our sequencer who sequences
       for this object, or raise and exception if our sequencer
       hasn't been set. *)
    IF debug_level >= 1 THEN
      debug.print(1, "GetSequencer: get seq for " & 
      EventWireRep.ToText(wrep));
    END;
    IF NOT isSequencer THEN
      IF debug_level >= 10 THEN
        debug.print(10, "GetSequencer: not seq.  Asking our seq.");
      END;
      IF sequencer = NIL THEN
        RaiseError(Atom.FromText("Sequencer is NIL"));
      END;
      TRY
        RETURN sequencer.objSpace.getSequencer(wrep);
      EXCEPT
      | NetObj.Error(ec) => SequencerFailed(sequencer, ec);
      | Thread.Alerted => SequencerFailed(sequencer, NIL);
      END;
    END;

    LOCK objTblMu DO
      (* If we are a sequencer, either ask the sequencer for this
         object who sequences it, or return ourself as the sequencer. *)
      IF debug_level >= 10 THEN
        debug.print(10, "GetSequencer: we are a seq. looking up object.");
      END;
      IF NOT GetObjInfo(wrep, objInfo) THEN
        RaiseError(Atom.FromText("Object doesn't exist"));
      END;
    END;

    IF debug_level >= 10 THEN
      debug.print(10, "GetSequencer: found objinfo.");
    END;
    IF objInfo.sequencer # NIL THEN
      TRY
        IF debug_level >= 10 THEN
          debug.print(10, "GetSequencer: return custom seq " & 
            SpaceConn.ToText(objInfo.sequencer));
        END;
        RETURN objInfo.sequencer.objSpace.getSequencer(wrep);
      EXCEPT
      | NetObj.Error(ec) => 
        SequencerFailed(objInfo.sequencer, ec);
      | Thread.Alerted => 
        SequencerFailed(objInfo.sequencer, NIL);
      END;
    END;
    IF debug_level >= 10 THEN
      debug.print(10, "SpaceGetSequencer: return localSpace.");
    END;
    RETURN localSpace;
  END GetSequencer;

PROCEDURE ExportSpace(name: Text.T) RAISES {Error} =
  BEGIN
    LOCK objTblMu DO
      TRY
        NetObj.Export("SharedObjRT-" & name, localSpace, NIL);
      EXCEPT
      | NetObj.Error(ec) => RaiseNetObjFailure(ec);
      | Thread.Alerted => RaiseNetObjAlerted();
      END;
    END;
  END ExportSpace;

PROCEDURE ImportSpace (host: Text.T; name: Text.T): ObjectSpace.T
  RAISES {Error} = 
  VAR
    agent: NetObj.Address;
    space: ObjectSpace.T;
  BEGIN
    TRY
      agent := NetObj.Locate(host);
      space := NetObj.Import("SharedObjRT-" & name, agent);
    EXCEPT
    | NetObj.Invalid =>
      RaiseNetObjFailure(AtomList.List1(Atom.FromText("Invalid Hostname")));
    | NetObj.Error(ec) => RaiseNetObjFailure(ec);
    | Thread.Alerted => RaiseNetObjAlerted();
    END;
    RETURN space;
  END ImportSpace;

PROCEDURE FlushIncomingUpdates() RAISES {Thread.Alerted} =
  BEGIN
    ep.flushReader();
  END FlushIncomingUpdates;

PROCEDURE FlushQueuedUpdates() RAISES {Thread.Alerted} =
  BEGIN
    ep.flushWork();
  END FlushQueuedUpdates; 

(*-------------------SharedObj Procedures-----------------------*)
PROCEDURE Wait(obj: T; c: Thread.Condition; m: Thread.Mutex := NIL) =
  VAR o: ObjectInfo.T := NIL;
  BEGIN
    IF m = NIL THEN
      (* Get the object info, creating a new info entry for objects for whom
         this is their first method call. *)
      LOCK objTblMu DO
        IF NOT GetObjInfo(obj.wrep, o) THEN
          o := RegisterObject(obj, obj.wrep, NIL, TRUE, obj.seqNoCnt.value());
        END;
      END;
      Thread.Acquire(o);
      m := o;
    END;

    (* Just before going to sleep, we must tell the event
       port that this thread is no longer available for
       handling incoming events, if it is such a thread *)
    EVAL ep.stealWorker();

    obj.mu.wait(m, c);

    IF o # NIL THEN
      Thread.Release(o);
    END;
  END Wait;

PROCEDURE AcquireGlobalLock (<*UNUSED*>obj: T) RAISES {Error} =
  BEGIN
    (* Not implemented yet.  Raise exception. *)
    RaiseError(Atom.FromText("Locking not yet implemented."));
  END AcquireGlobalLock;

PROCEDURE ReleaseGlobalLock (<*UNUSED*>obj: T) RAISES {Error} =
  BEGIN
    (* Not implemented yet.  Raise exception. *)
    RaiseError(Atom.FromText("Locking not yet implemented."));
  END ReleaseGlobalLock;

PROCEDURE SetTimeliness (<*UNUSED*>obj: T; <*UNUSED*>value: Timeliness)
  RAISES {Error} =
  BEGIN
    (* Not implemented yet.  Raise exception. *)
    RaiseError(Atom.FromText("Timeliness not yet implemented."));
  END SetTimeliness;

PROCEDURE Own (<*UNUSED*>obj: T; <*UNUSED*>willingness: Timeliness := 0)
  RAISES {Error} =
  BEGIN
    (* Not implemented yet.  Raise exception. *)
    RaiseError(Atom.FromText("Ownership not yet implemented."));
  END Own;

PROCEDURE Disown (<*UNUSED*>obj: T) RAISES {Error} =
  BEGIN
    (* Not implemented yet.  Raise exception. *)
    RaiseError(Atom.FromText("Ownership not yet implemented."));
  END Disown;

(*---------------------Error Procedures---------------------*)
  
PROCEDURE RaiseCommFailure (ec: AtomList.T) RAISES {Error} =
  BEGIN
    RAISE Error(AtomList.Cons(CommFailure, ec));
  END RaiseCommFailure;

PROCEDURE RaiseNetObjFailure (ec: AtomList.T) RAISES {Error} =
  BEGIN
    RAISE Error(AtomList.Cons(NetObjFailure, ec));
  END RaiseNetObjFailure;

PROCEDURE RaiseIPFailure (ec: AtomList.T) RAISES {Error} =
  BEGIN
    RAISE Error(AtomList.Cons(IPFailure, ec));
  END RaiseIPFailure;

PROCEDURE RaiseNetObjAlerted () RAISES {Error} =
  BEGIN
    RAISE Error(AtomList.List1(NetObjAlerted));
  END RaiseNetObjAlerted;

PROCEDURE RaiseAlerted () RAISES {Error} =
  BEGIN
    RAISE Error(AtomList.List1(Alerted)); 
  END RaiseAlerted;

PROCEDURE RaiseEventFailure (ec: AtomList.T) RAISES {Error} =
  BEGIN
    RAISE Error(AtomList.Cons(EventFailure, ec));
  END RaiseEventFailure;

PROCEDURE RaiseDeadObject () RAISES {Error} =
  BEGIN
    RaiseError(DeadObject);
  END RaiseDeadObject;

PROCEDURE RaiseRecursiveUpdate() RAISES {Error} =
  BEGIN
    RaiseError(RecursiveUpdate);
  END RaiseRecursiveUpdate;

PROCEDURE RaiseError(a: Atom.T) RAISES {Error} =
  BEGIN
    RAISE Error(AtomList.List1(a));
  END RaiseError;

(*----------------------Debugging-------------------------*)
PROCEDURE ObjTblToText(): TEXT =
  VAR t := "";
      wrep: WireRep;
      objInfo: ObjectInfo.T;
  BEGIN
    LOCK objTblMu DO
      t := t & "Object Table: " & Fmt.Int(objTbl.size()) & " objects\n";
      WITH iterator = objTbl.iterate() DO
        WHILE iterator.next(wrep, objInfo) DO
          LOCK objInfo DO
            t := t & " "  & ObjectInfo.ToText(objInfo) & "\n";
            WITH obj = GetObjRef(objInfo) DO
              IF obj # NIL THEN
                t := t & "  " & SharedObjRep.ToText(obj) & "\n";
                t := t & "  " & EventCounter.ToText(obj.seqNoCnt) & "\n";
              END;
            END;
          END;
        END;
      END;
    END;
    RETURN t;
  END ObjTblToText;

VAR 
  ep: EventPort.T := NIL;
  seqCV  : Thread.Condition;

  sequencer: SpaceConn.T := NIL;
  isSequencer: BOOLEAN := FALSE;	 (* Are we a sequencer? *)

  objTbl: ObjectInfoTbl.T;		 (* The objects we know about. *)

  (* For saving Messages *)
  msgMu: MUTEX;
  freeMessage: Message.T := NIL;

  objCleaner :ObjectCleanerClosure;
BEGIN
  (* Standard errors. *)
  NetObjFailure := Atom.FromText("SharedObj.NetObjError");
  NetObjAlerted := Atom.FromText("SharedObj.NetObjAlerted");
  IPFailure := Atom.FromText("SharedObj.IPError");
  CommFailure := Atom.FromText("SharedObj.CommFailure");
  EventFailure := Atom.FromText("SharedObj.EventError");
  DeadObject := Atom.FromText("SharedObj.DeadObject");
  Alerted := Atom.FromText("SharedObj.Alerted");
  RecursiveUpdate := Atom.FromText("SharedObj.RecursiveUpdate");

  (* Want a static sequence number of value 1 *)
  zeroSeqNo := EventNumber.New();
  oneSeqNo := EventNumber.New();
  TRY
    oneSeqNo.inc();
  EXCEPT
  | EventNumber.Overflow =>
  END;

  debug := NEW(Debug.T).init("SharedObj", 0);

  msgMu := NEW(MUTEX);

  ep := NEW(EventPort.T).init(FALSE);
  TRY
    ep.register(ORD(Op.MethodCall), Protocol, MethodDispatcher, NIL);
(*    ep.register(ORD(Op.MethodAck), Protocol, Space, NIL);*)
  EXCEPT
  | Event.Error => 
    Process.Crash("Failed to register Shared Object Event callback handler");
  END;
  objTbl := NEW(ObjectInfoTbl.Default).init();
  objTblMu := NEW(MUTEX);

  objCleaner := NEW(ObjectCleanerClosure).init();

  seqCV := NEW(Thread.Condition);
END SharedObjRT.
