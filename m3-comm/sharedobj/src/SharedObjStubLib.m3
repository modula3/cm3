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
 * Created On      : Wed Sep 13 11:16:03 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Jul  9 19:13:34 1998
 * Update Count    : 96
 * 
 * $Source: /opt/cvs/cm3/m3-comm/sharedobj/src/SharedObjStubLib.m3,v $
 * $Date: 2001-12-02 13:41:17 $
 * $Author: wagner $
 * $Revision: 1.2 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.1.1.1  2001/12/02 13:14:14  wagner
 * Blair MacIntyre's sharedobj package
 *
 * Revision 1.11  1998/07/14 02:34:03  bm
 * tried to fix the distribution problems -- not yet working, but closer
 *
 * Revision 1.10  1998/05/11 02:34:17  bm
 * bug fixes, added SharedObj.Wait
 *
 * Revision 1.9  1998/01/24 00:21:03  bm
 * timing bug
 *
 * Revision 1.8  1997/10/22 14:43:00  bm
 * Bug fix.  Naming conflicts.
 *
 * Revision 1.7  1997/08/04 20:18:16  bm
 * Fixed BRANDs
 *
 * Revision 1.6  1997/01/23 15:27:20  bm
 * Lot's of little bug fixes.
 *
 * Revision 1.5  1996/11/22 19:03:33  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

MODULE SharedObjStubLib;

IMPORT Atom, NetObj, Pickle2 AS Pickle, Rd, Wr, Thread, EventWireRep,
       EventCounter, EventStubLib, ObjectInfo, RdWrMutex, Debug,
       SpaceConn, EventNumber, EventNumberF, WeakRef, PickleStubs,
       ObjectSpace, Event, Process, Fmt, NetObjF,
       Word;
(* IMPORT AckInfo, AckList;*)
IMPORT SharedObjRTF, SharedObj, SharedObjRep, SharedObjError;
FROM SharedObjRTF IMPORT debug, debug_level, zeroSeqNo, oneSeqNo, localSpace; 
FROM EventProtocol IMPORT Byte8, Int32;

(* A handle is used during event construction to hold the information
   needed to build the event. *)
REVEAL
  Handle = PublicHandle BRANDED "SharedObjStubLib.Handle" OBJECT 
    next: Handle := NIL;

    objInfo: ObjectInfo.T;
    obj: SharedObj.T;
    thread: INTEGER;
    alerted: BOOLEAN := FALSE;

    (* Used during sequencing. *)
    en: SharedObj.SequenceNumber := NIL;
  END;

VAR
  (* For saving Handles *)
  handleMu: MUTEX;
  freeHandle: Handle := NIL;

PROCEDURE NewHandle(o: ObjectInfo.T; obj: SharedObj.T; 
                    eh: EventStubLib.Handle; local: BOOLEAN): Handle = 
  VAR ph: Handle;
  BEGIN
    LOCK handleMu DO
      IF freeHandle # NIL THEN
        ph := freeHandle; freeHandle := freeHandle.next;
      ELSE
        ph := NEW(Handle, next:=NIL);
        ph.en := EventNumber.New();
      END;
    END;

    ph.eh := eh;
    ph.local := local;
    ph.objInfo := o;
    ph.obj := obj;
    ph.thread := o.pickThreadSlot();
    ph.alerted := FALSE;

    RETURN ph;
  END NewHandle; 

PROCEDURE FreeHandle(h: Handle) =
  BEGIN
    LOCK handleMu DO
      h.eh := NIL;
      h.objInfo := NIL;
      h.obj := NIL;

      h.next := freeHandle;
      freeHandle := h;
    END;
  END FreeHandle; 

PROCEDURE IncMsgSeqNo(objInfo: ObjectInfo.T; old: SharedObj.SequenceNumber) =
  BEGIN
    (* Set the old number to the current value sequence number. *) 
    EVAL old.init(objInfo.seqNo);

    IF debug_level >= 15 THEN
      debug.print(15, "SequenceMsgStandalone: assigned seqNo " & 
      objInfo.seqNo.fmt(10) & "to update");
    END;
    TRY
      objInfo.seqNo.inc();
    EXCEPT
    | EventNumber.Overflow => 
      Process.Crash("Event Number Overflowed!");
    END;
  END IncMsgSeqNo;

PROCEDURE StartCall (obj: SharedObj.T): Handle
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR o: ObjectInfo.T;
  BEGIN
    (* Get the object info, creating a new info entry for objects for whom
       this is their first method call. *)
    LOCK SharedObjRTF.objTblMu DO
      IF NOT SharedObjRTF.GetObjInfo(obj.wrep, o) THEN
        IF debug_level >= 15 THEN
          debug.print(15, "StartCall: new object.");
        END;
        o := SharedObjRTF.RegisterObject(
               obj, obj.wrep, NIL, TRUE, obj.seqNoCnt.value());
      END;
    END;

    (* If the object is currently standalone, we can execute the procedure
       without packing the args.  However, to do this we must lock the
       objectInfo structure until we sequence the object.  Otherwise, it
       might be passed off the machine before we hit the sequencer and then
       we cannot execute the event without forwarding it.

       So, lock the object info till we are reading to try to sequence
       it. *)

    LOCK o DO
      IF o.isStandalone THEN
        IF debug_level >= 15 THEN
          debug.print(15, "StartCall: standalone, returning local=TRUE.");
        END;
        WITH h = NewHandle(o, obj, NIL, TRUE) DO
          IncMsgSeqNo(h.objInfo, o.waiting[h.thread].en);
          RETURN h;
        END;
      END;

      WITH h = NewHandle(o, obj, EventStubLib.StartCreate(), FALSE) DO
        (* Encode the default, non-sequenced EventNumber.T *)
        EventStubLib.OutBytes(h.eh, obj.wrep.byte);
        EventStubLib.OutInteger(h.eh, h.thread);
        IF debug_level >= 15 THEN
          debug.print(
            15, "StartCall: new handle (thread " & Fmt.Int(h.thread) & ").");
        END;
        RETURN h;
      END;
    END;
  END StartCall;

PROCEDURE MarshalArgs(m: Handle): BOOLEAN =
  BEGIN
    RETURN NOT m.local;
  END MarshalArgs; 

PROCEDURE SequenceCall(h: Handle; stubProt: StubProtocol) 
  RAISES {SharedObj.Error, Thread.Alerted} =
  VAR
    obj: SharedObj.T := h.obj;
  BEGIN
    IF h.local THEN
      (* We have the objInfo locked until we get a sequence number. *)
      IF debug_level >= 15 THEN
        debug.print(15, "SequenceCall: standalone, already sequenced.");
      END;
      EVAL h.en.init(h.objInfo.waiting[h.thread].en);
      LOCK h.objInfo DO
        h.objInfo.releaseThreadSlot(h.thread);
      END;
    ELSE
      IF debug_level >= 15 THEN
        debug.print(15, "SequenceCall: creating message.");
      END;
      
      WITH ev = EventStubLib.EndCreate(h.eh, 
                                       ORD(SharedObjRTF.Op.MethodCall), 
                                       stubProt, zeroSeqNo) DO
        SharedObjRTF.SequenceCall(h.obj, ev, h.thread, h.objInfo,
                                  h.en, h.alerted); 
      END;

      (* just in case it's changed while we were sequencing *)
      obj := h.obj;
    END;

    IF debug_level >= 15 THEN
      debug.print(15, "SequenceCall: waiting for counter.");
    END;
    TRY
      obj.seqNoCnt.acquire(h.en);
    EXCEPT
    | EventCounter.Duplicate => 
      SharedObjError.RaiseError(
          Atom.FromText("Update applied by someone else."));
    END;
    IF debug_level >= 15 THEN
      debug.print(15, "SequenceCall: got counter.  returning.");
    END;
  END SequenceCall;

PROCEDURE EndCall(h: Handle) RAISES {Thread.Alerted} =
  BEGIN
    IF debug_level >= 15 THEN
      debug.print(15, "EndCall: releasing counter.");
    END;
    h.obj.seqNoCnt.release();
    IF debug_level >= 15 THEN
      debug.print(15, "EndCall: counter released.");
    END;

    (* Free the handle *)
    FreeHandle(h);

    (* Now that we have at least tried to apply the event, we can
       reraise the Alerted exception. *)
    IF h.alerted THEN RAISE Thread.Alerted END;
  END EndCall;
 
(* We create wrapper here for the InRef/OutRef of the Event library.
   These wrappers handle passing of Shared Objects inside events.
   Eventually they will hopefully handle Network Objects as well. (But
   not bloodly likely, since I'd have to integrate it more tightly
   with the network object package, and that seems a little hairy). *)

TYPE
  MSpec = {NetObjRef, SharedObjRef, Ref};

TYPE
  InRefFindCB = ObjectSpace.FindObjCallBack OBJECT 
    wrep: SharedObjRep.WireRep;
    obj: SharedObj.T := NIL;
  OVERRIDES
    try := InRefTry;
  END;

PROCEDURE InRefTry(self: InRefFindCB; seqNo: SharedObj.SequenceNumber; 
                   space: ObjectSpace.T) 
  RAISES {Thread.Alerted, NetObj.Error, SharedObj.Error} =
  BEGIN
    self.obj := space.get(self.wrep, seqNo);
  END InRefTry;

PROCEDURE InRef (h: EventStubLib.Handle; tc: INTEGER): REFANY
  RAISES {SharedObj.Error, Rd.Failure, Thread.Alerted} =
  VAR
    r      : REFANY;
    wrep   : SharedObjRep.WireRep;
    objInfo: ObjectInfo.T;
  BEGIN
    TRY
      CASE EventStubLib.InByte(h) OF
      | ORD(MSpec.SharedObjRef) =>
        BEGIN
          EventStubLib.InBytes(h, wrep.byte);
          LOCK SharedObjRTF.objTblMu DO
            IF SharedObjRTF.GetObjInfo(wrep, objInfo) THEN
              r := SharedObjRTF.GetObjRef(objInfo);
              IF r # NIL THEN RETURN r END;
            END;
          END;
          
          WITH trycb = NEW(InRefFindCB, wrep := wrep),
               seq = SharedObjRTF.GetDfltSequencer() DO
            TRY
              TRY
                RETURN seq.get(wrep, zeroSeqNo);
              EXCEPT SharedObj.Error =>
                (* if our sequencer doesn't have it, ask it if it can
                   find it! *)
              END;
              TRY
                seq.findObj(wrep, trycb);
              EXCEPT
              | SharedObj.Error =>
                (* if our sequencer doesn't know where it is, repeat
                   the whole process with the senders sequencer. *)
                   
                VAR objseq: ObjectSpace.T;
                    nwrep: NetObjF.WRep;
                    where: NetObj.Address;
                BEGIN
                  EventStubLib.InBytes(h, nwrep.byte);
                  where := EventStubLib.InRef(h);
                  objseq := NetObjF.FromWireRep(nwrep, where);

                  TRY
                    RETURN objseq.get(wrep, zeroSeqNo);
                  EXCEPT SharedObj.Error =>
                    (* if the sequencer doesn't have it, ask it if it can
                       find it! *)
                  END;
                  objseq.findObj(wrep, trycb);
                END;
              END;
            EXCEPT
            | NetObj.Error(ec) =>
              SharedObjError.RaiseNetObjFailure(ec);
            END;
      
            (* If we make it this far, we have the object in "trycb.obj" *)
            <* ASSERT trycb.obj # NIL *>
            RETURN trycb.obj;  
          END;
        END;
      | ORD(MSpec.NetObjRef) =>
        VAR nwrep: NetObjF.WRep;
            where: NetObj.Address;
        BEGIN
          EventStubLib.InBytes(h, nwrep.byte);
          where := EventStubLib.InRef(h);
          TRY
            r := NetObjF.FromWireRep(nwrep, where);
            RETURN r;
          EXCEPT
          | NetObj.Error(ec) =>
            SharedObjError.RaiseNetObjFailure(ec);
            (* To shut up the compiler.  Will never get here. *)
            <* ASSERT FALSE *>
            (**********************************************)
          END;
        END;
      | ORD(MSpec.Ref) => RETURN EventStubLib.InRef(h, tc);
      ELSE
        EventStubLib.RaiseUnmarshalFailure();
        (* To shut up the compiler.  Will never get here. *)
        RETURN NIL;
      END;
    EXCEPT
    | Event.Error (ec) => SharedObjError.RaiseEventFailure(ec); 
      RETURN NIL; (*shut up the compiler *)
    END;
  END InRef;

PROCEDURE OutRef(h: Handle; r: REFANY)
   RAISES {Wr.Failure, Thread.Alerted} =
  VAR wrep: NetObjF.WRep;
      where: NetObj.Address;
  BEGIN
    TYPECASE r OF
    | NULL =>
      EventStubLib.OutByte(h.eh, ORD(MSpec.Ref)); 
      EventStubLib.OutRef(h.eh, r);
    | SharedObj.T(o) => 
      TRY
        EVAL CheckObjectRegistered(o);
      EXCEPT
      | SharedObj.Error(el) => 
        RAISE Wr.Failure(el);
      | NetObj.Error(el) => 
        RAISE Wr.Failure(el);
      END;
      EventStubLib.OutByte(h.eh, ORD(MSpec.SharedObjRef)); 
      EventStubLib.OutBytes(h.eh, o.wrep.byte);
      NetObjF.ToWireRep(SharedObjRTF.GetDfltSequencer(), wrep, where);
      EventStubLib.OutBytes(h.eh, wrep.byte);
      EventStubLib.OutRef(h.eh, where);
    | NetObj.T(o) =>
      EventStubLib.OutByte(h.eh, ORD(MSpec.NetObjRef));
      NetObjF.ToWireRep(o, wrep, where);
      EventStubLib.OutBytes(h.eh, wrep.byte);
      EventStubLib.OutRef(h.eh, where);
    ELSE 
      EventStubLib.OutByte(h.eh, ORD(MSpec.Ref)); 
      EventStubLib.OutRef(h.eh, r);
    END;
  END OutRef;

(* We now create a new pickle special for NetObj.T that will
   send/recieve the wirerep if it's one of our writers/readers.
   Otherwise it will call the old one *)
TYPE
  NetObjTSpecial = Pickle.Special OBJECT
                       OVERRIDES
                         write := Write_NetObjT;
                         read := Read_NetObjT;
                       END;

PROCEDURE Write_NetObjT (ts: NetObjTSpecial; 
                         ref: REFANY; out: Pickle.Writer)
  RAISES {Pickle.Error, Wr.Failure, Thread.Alerted} =
  VAR o := NARROW(ref, NetObj.T);
  BEGIN
    IF EventStubLib.IsEventWriter(out) THEN
      VAR wrep: NetObjF.WRep;
          where: NetObj.Address;
      BEGIN
        NetObjF.ToWireRep(o, wrep, where);
        PickleStubs.OutBytes(out, wrep.byte);
        PickleStubs.OutRef(out, where);
      END;
    ELSE
      ts.prev.write(ref,out);
    END;
  END Write_NetObjT;

PROCEDURE Read_NetObjT (ts: NetObjTSpecial;
                        in: Pickle.Reader;
                        id: Pickle.RefID):REFANY
  RAISES {Pickle.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    IF EventStubLib.IsEventReader(in) THEN
      VAR nwrep: NetObjF.WRep;
          where: NetObj.Address;
      BEGIN
        PickleStubs.InBytes(in, nwrep.byte);
        where := PickleStubs.InRef(in);
        TRY
          RETURN NetObjF.FromWireRep(nwrep, where);
        EXCEPT
        | NetObj.Error(ec) =>
          RAISE Pickle.Error("NetObj.Error: " & Debug.AtomListToText(ec));
        END;
      END;
    ELSE
      RETURN ts.prev.read(in, id);
    END;
  END Read_NetObjT;

(*******************************************************)
(* Wrappers around the EventStubLib routines           *)
(*******************************************************)
PROCEDURE OutChars (h: Handle; READONLY chars: ARRAY OF CHAR)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    EventStubLib.OutChars(h.eh,chars);
  END OutChars;

PROCEDURE OutBytes (h: Handle; READONLY bytes: ARRAY OF Byte8)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    EventStubLib.OutBytes(h.eh,bytes);
  END OutBytes;

PROCEDURE OutInteger (h: Handle; i: INTEGER)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    EventStubLib.OutInteger(h.eh,i);
  END OutInteger;

PROCEDURE OutInt32 (h: Handle; i: Int32) RAISES
  {Wr.Failure, Thread.Alerted} =
  BEGIN
    EventStubLib.OutInt32(h.eh,i);
  END OutInt32;

PROCEDURE OutByte (h: Handle; i: Byte8) RAISES
  {Wr.Failure, Thread.Alerted}= 
  BEGIN
    EventStubLib.OutByte(h.eh,i);
  END OutByte;

PROCEDURE OutBoolean (h: Handle; bool: BOOLEAN)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    EventStubLib.OutBoolean(h.eh,bool);
  END OutBoolean;

PROCEDURE OutReal (h: Handle; r: REAL) RAISES
  {Wr.Failure, Thread.Alerted} =
  BEGIN
    EventStubLib.OutReal(h.eh,r);
  END OutReal;

PROCEDURE OutLongreal (h: Handle; card: LONGREAL)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    EventStubLib.OutLongreal(h.eh,card);
  END OutLongreal;

PROCEDURE OutExtended (h: Handle; card: EXTENDED)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    EventStubLib.OutExtended(h.eh,card);
  END OutExtended;

PROCEDURE OutCardinal (h: Handle; card: CARDINAL)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    EventStubLib.OutCardinal(h.eh,card);
  END OutCardinal;

PROCEDURE InChars (h: EventStubLib.Handle; VAR chars: ARRAY OF CHAR)
  RAISES {SharedObj.Error, Rd.Failure, Thread.Alerted} =
  BEGIN
    TRY
      EventStubLib.InChars(h, chars);
    EXCEPT
    | Event.Error (ec) => SharedObjError.RaiseEventFailure(ec);
    END;
  END InChars;

PROCEDURE InBytes (h: EventStubLib.Handle; VAR bytes: ARRAY OF Byte8)
  RAISES {SharedObj.Error, Rd.Failure, Thread.Alerted} =
  BEGIN
    TRY
      EventStubLib.InBytes(h,bytes);
    EXCEPT
    | Event.Error (ec) => SharedObjError.RaiseEventFailure(ec);
    END;
  END InBytes;

PROCEDURE InInteger (h: EventStubLib.Handle;
                     min            := FIRST(INTEGER);
                     max            := LAST(INTEGER)   ): INTEGER
  RAISES {SharedObj.Error, Rd.Failure, Thread.Alerted} =
  BEGIN
    TRY
      RETURN EventStubLib.InInteger(h, min, max);
    EXCEPT
    | Event.Error (ec) => 
      SharedObjError.RaiseEventFailure(ec);
      RETURN 0; (* not reached, to shut up the compiler *)
    END;
  END InInteger;

PROCEDURE InInt32 (h: EventStubLib.Handle; min := FIRST(Int32); 
                   max := LAST(Int32)): Int32 
  RAISES {SharedObj.Error, Rd.Failure, Thread.Alerted} =
  BEGIN
    TRY
      RETURN EventStubLib.InInt32(h, min, max);
    EXCEPT
    | Event.Error (ec) => 
      SharedObjError.RaiseEventFailure(ec);
      RETURN 0; (* not reached, to shut up the compiler *)
    END;
  END InInt32;

PROCEDURE InByte (h: EventStubLib.Handle; max := LAST(Byte8)): Byte8
  RAISES {SharedObj.Error, Rd.Failure, Thread.Alerted} =
  BEGIN
    TRY
      RETURN EventStubLib.InByte(h, max);
    EXCEPT
    | Event.Error (ec) => 
      SharedObjError.RaiseEventFailure(ec);
      RETURN 0; (* not reached, to shut up the compiler *)
    END;
  END InByte;

PROCEDURE InBoolean (h: EventStubLib.Handle): BOOLEAN
  RAISES {SharedObj.Error, Rd.Failure, Thread.Alerted} = 
  BEGIN
    TRY
      RETURN EventStubLib.InBoolean(h);
    EXCEPT
    | Event.Error (ec) => 
      SharedObjError.RaiseEventFailure(ec);
      RETURN FALSE; (* not reached, to shut up the compiler *)
    END;
  END InBoolean;

PROCEDURE InReal (h: EventStubLib.Handle): REAL
  RAISES {SharedObj.Error, Rd.Failure, Thread.Alerted} = 
  BEGIN
    TRY
      RETURN EventStubLib.InReal(h);
    EXCEPT
    | Event.Error (ec) => 
      SharedObjError.RaiseEventFailure(ec);
      RETURN 0.0; (* not reached, to shut up the compiler *)
    END;
  END InReal;

PROCEDURE InLongreal (h: EventStubLib.Handle): LONGREAL
  RAISES {SharedObj.Error, Rd.Failure, Thread.Alerted} =
  BEGIN
    TRY
      RETURN EventStubLib.InLongreal(h);
    EXCEPT
    | Event.Error (ec) => 
      SharedObjError.RaiseEventFailure(ec);
      RETURN 0.0D0; (* not reached, to shut up the compiler *)
    END;
  END InLongreal;

PROCEDURE InExtended (h: EventStubLib.Handle): EXTENDED
  RAISES {SharedObj.Error, Rd.Failure, Thread.Alerted} =
  BEGIN
    TRY
      RETURN EventStubLib.InExtended(h);
    EXCEPT
    | Event.Error (ec) => 
      SharedObjError.RaiseEventFailure(ec);
      RETURN 0.0X0; (* not reached, to shut up the compiler *)
    END;
  END InExtended;

PROCEDURE InCardinal (h: EventStubLib.Handle; lim: CARDINAL := LAST(CARDINAL)):
  CARDINAL RAISES {SharedObj.Error, Rd.Failure, Thread.Alerted} =
  BEGIN
    TRY
      RETURN EventStubLib.InCardinal(h, lim);
    EXCEPT
    | Event.Error (ec) => 
      SharedObjError.RaiseEventFailure(ec);
      RETURN 0; (* not reached, to shut up the compiler *)
    END;
  END InCardinal;


(*******************************************************)
PROCEDURE CheckObjectRegistered(obj: SharedObj.T): ObjectInfo.T RAISES
  {Thread.Alerted, SharedObj.Error, NetObj.Error} =
  VAR 
    objInfo: ObjectInfo.T;
  BEGIN
    (* we must wait until the sequencer has been set, or
       we have identified ourself as a sequencer. *)
    LOCK SharedObjRTF.objTblMu DO
      SharedObjRTF.WaitForSequencer();
    END;

    (* Get the objInfo.  If it doesn't exist, we have never called a
       method or sent this object before, so register it. *)
    LOCK SharedObjRTF.objTblMu DO
      IF NOT SharedObjRTF.GetObjInfo(obj.wrep, objInfo) THEN
        IF debug_level >= 1 THEN
          debug.print(1, "CheckObjectRegistered: new object. " &
            "Register locally first.");
        END;
        objInfo := SharedObjRTF.RegisterObject(obj, obj.wrep, NIL, TRUE,
                                               obj.seqNoCnt.value()); 
      END;
    END;

    (* If it's standalone, this is the first time we've sent it, so
       register a new object with the sequencer if we are writing
       across the net. *)
    LOCK objInfo DO
      IF objInfo.isStandalone THEN
        IF debug_level >= 1 THEN
          debug.print(1, "CheckObjectRegistered: standalone object. " &
            "register with seq");
        END;
        WITH seq = SharedObjRTF.GetDfltSequencer() DO
          IF seq # localSpace THEN
            seq.newObject(localSpace.space(), obj.wrep,  objInfo.seqNo);
          END;
        END;
        objInfo.isStandalone := FALSE;
        IF debug_level >= 5 THEN
          debug.print(5, "WriteSharedObjPickle: registered with seq");
        END;
      END;
    END;
    RETURN objInfo;
  END CheckObjectRegistered;

(*******************************************************)

PROCEDURE StartWritePickle(obj: SharedObj.T; wr: Pickle.Writer) 
  RAISES {Pickle.Error, Wr.Failure, Thread.Alerted} =
  VAR 
    objInfo: ObjectInfo.T;
  BEGIN
    (* First, if it is not ok, we can't pass it.  Fail! *)
    IF debug_level >= 1 THEN
      debug.print(1, "WriteSharedObjPickle: begin pickling " &
        SharedObjRep.ToText(obj) &" ");
    END;
    IF NOT obj.ok THEN
      IF debug_level >= 5 THEN
        debug.print(5, "WriteSharedObjPickle: object invalid");
      END;
      RAISE Pickle.Error("Object is invalid.");
    END;

    TRY
      objInfo := CheckObjectRegistered(obj);
    EXCEPT
    | SharedObj.Error(el) => 
      RAISE Pickle.Error("SharedObj.Error: " & 
            Debug.AtomListToText(el));
    | NetObj.Error(el) => 
      RAISE Pickle.Error("NetObj.Error: " & Debug.AtomListToText(el));
    END;

    IF NetObjF.IsNetObjWriter(wr) OR EventStubLib.IsEventWriter(wr) THEN
      (* Netobj transfer, so write our space object across.  *)
      IF debug_level >= 5 THEN
        debug.print(5, "WriteSharedObjPickle: netobj call.");
      END;
      wr.write(SharedObjRTF.localSpace);
    ELSE
      (* Other pickling action, so don't write our space. *)
      IF debug_level >= 5 THEN
        debug.print(5, "WriteSharedObjPickle: write to normal file.");
      END;
      wr.write(NIL);
    END;

    (* Now, aquire a read lock on the object, since we need to
       read it without it changing. *)
    IF debug_level >= 5 THEN
      debug.print(5, "WriteSharedObjPickle: acquire read lock.");
    END;
    AcquireReadLock(obj);
    IF debug_level >= 5 THEN
      debug.print(5, "WriteSharedObjPickle: read lock acquired.");
    END;

    IF debug_level >= 5 THEN
      debug.print(5, "WriteSharedObjPickle: writing basic obj info.");
    END;
    PickleStubs.OutBytes(wr, obj.wrep.byte);
    IF debug_level >= 5 THEN
      debug.print(5, "WriteSharedObjPickle: wrep: " & 
      EventWireRep.ToText(obj.wrep));
    END;
    PickleStubs.OutBoolean(wr, obj.ok);
    WITH val = obj.seqNoCnt.value() DO
      PickleStubs.OutInteger(wr, val.hi);
      PickleStubs.OutInteger(wr, val.lo);
      IF debug_level >= 5 THEN
        debug.print(5, "WriteSharedObjPickle: counter: " & val.fmt(10));
      END;
    END;
  END StartWritePickle;

PROCEDURE EndWritePickle(obj: SharedObj.T; wr: Pickle.Writer) 
  RAISES {Pickle.Error, Wr.Failure, Thread.Alerted} =
  BEGIN
    PickleStubs.OutRef(wr, obj.proxy);
    IF debug_level >= 5 THEN
      debug.print(5, "WriteSharedObjPickle: release read lock.");
    END;
    ReleaseReadLock(obj);
    IF debug_level >= 1 THEN
      debug.print(1, "WriteSharedObjPickle: done pickling.");
    END;
  END EndWritePickle;

PROCEDURE StartReadPickle(obj: SharedObj.T; rd: Pickle.Reader; 
                          from: ObjectSpace.T) 
  RAISES {Pickle.Error, Rd.Failure, Thread.Alerted} =
  VAR hi, lo: Word.T;
      en: EventNumber.T;
  BEGIN
    IF debug_level >= 1 THEN
      debug.print(1, "ReadSharedObjPickle: read basic object.");
    END;

    PickleStubs.InBytes(rd, obj.wrep.byte);
    IF debug_level >= 5 THEN
      debug.print(5, "ReadSharedObjPickle: wrep: " & 
      EventWireRep.ToText(obj.wrep));
    END;
    obj.ok := PickleStubs.InBoolean(rd);
    hi := PickleStubs.InInteger(rd);
    lo := PickleStubs.InInteger(rd);
    en := NEW(EventNumber.T, hi := hi, lo := lo);
    IF debug_level >= 5 THEN
      debug.print(5, "ReadSharedObjPickle: counter: " & en.fmt(10));
    END;

    IF from = NIL THEN
      IF debug_level >= 5 THEN
        debug.print(5, "ReadSharedObjPickle: from=NIL, " &
          "new object read from file.");
      END;
      (* This is a new object. *)
      obj.wrep := EventWireRep.New();
      InitFromPickle(obj, oneSeqNo);
    ELSE
      IF debug_level >= 5 THEN
        debug.print(5, "ReadSharedObjPickle: new copy of shared obj read.");
      END;
      InitFromPickle(obj, en);
    END;
    IF debug_level >= 5 THEN
      debug.print(5, "ReadSharedObjPickle: unmarshalled obj.");
    END;
  END StartReadPickle;

PROCEDURE InitFromPickle(self: SharedObj.T; seqNo: EventNumber.T) =
  BEGIN
    IF debug_level >= 1 THEN
      debug.print(1, "Initializing Sharedobj from Pickle");
    END;
    self.mu := NEW(RdWrMutex.T).init();
    self.seqNoCnt := EventCounter.New(self.mu, seqNo);
    IF debug_level >= 1 THEN
      debug.print(1, "New SharedObj from Pickle: " & 
        SharedObjRep.ToText(self));
    END;
  END InitFromPickle;

PROCEDURE SetupNewCopy(obj: SharedObj.T; rd: Pickle.Reader; 
                       id: Pickle.RefID; from: ObjectSpace.T): SharedObj.T 
  RAISES {Pickle.Error, Thread.Alerted} =
  VAR objInfo: ObjectInfo.T;
      nextSeqNo: SharedObj.SequenceNumber := NIL;
      objSeq : ObjectSpace.T := NIL;
      oldObj: SharedObj.T;
      seqConn : SpaceConn.T := NIL;
  BEGIN
    (* Now, if the object is new, we can just return.  The rest of
       the setup will happen when it is passed off machine.  *)
    IF debug_level >= 1 THEN
      debug.print(1, "SetupNewCopy: setting up new copy of object.");
    END;
    IF from = NIL THEN
      IF debug_level >= 1 THEN
        debug.print(1, "SetupNewCopy: new object, no setup required.");
      END;
      rd.noteRef(obj, id);
      RETURN obj;
    END;

    (* As with WriteT above, we must wait for our sequencer to be
       properly set up before trying to set up the new copy. *)
    IF debug_level >= 5 THEN
      debug.print(5, 
                  "SetupNewCopy: first, wait for sequencer to be defined.");
    END;
    LOCK SharedObjRTF.objTblMu DO
      SharedObjRTF.WaitForSequencer();
    END;
    IF debug_level >= 20 THEN
      debug.print(20, "SetupNewCopy: sequencer defined.");
    END;

    (* Get the objInfo record, or create a new one.  When we create
       it, we must fetch the sequencer for this object and pass it to
       our default sequencer for setting up our new copy of the
       object. *)
    LOCK SharedObjRTF.objTblMu DO
      IF NOT SharedObjRTF.GetObjInfo(obj.wrep, objInfo) THEN
        IF debug_level >= 1 THEN
          debug.print(1, "SetupNewCopy: new object.");
        END;

        TRY
	  (* We can ask for the sequencer for this object without worry,
	     because if we are the sequencer (and would thus get called
	     from the remote machine as a result of this call), we
	     wouldn't be here because the objInfo entry would already
	     exist! *)
          IF debug_level >= 5 THEN
            debug.print(5, "SetupNewCopy: get seq from sender.");
          END;

          TRY
            Thread.Release(SharedObjRTF.objTblMu);
            objSeq := from.getSequencer(obj.wrep);
          FINALLY
            Thread.Acquire(SharedObjRTF.objTblMu);
          END;
        EXCEPT
        | SharedObj.Error(el) => 
          RAISE Pickle.Error("SharedObj.Error: " & Debug.AtomListToText(el));
        | NetObj.Error(el) => 
          RAISE Pickle.Error("NetObj.Error: " & Debug.AtomListToText(el));
        END;
      END;
    END;

    (* now, we have the object sequencer, but check to see if the
       object got created in the mean time *)
    LOCK SharedObjRTF.objTblMu DO
      IF NOT SharedObjRTF.GetObjInfo(obj.wrep, objInfo) THEN
        IF debug_level >= 1 THEN
          debug.print(1, "SetupNewCopy: new object.");
        END;

        TRY
          WITH seq = SharedObjRTF.GetDfltSequencer() DO
            IF seq # localSpace THEN
              (* Create our new object info. *)
              IF debug_level >= 5 THEN
                debug.print(5, "SetupNewCopy: register local copy.");
              END;
              objInfo := SharedObjRTF.RegisterObject(obj, obj.wrep,
                                                     NIL, FALSE, 
                                                     obj.seqNoCnt.value());
  
              IF debug_level >= 5 THEN
                debug.print(5, "SetupNewCopy: notify our " & 
                  "sequencer of new copy.");
              END;
              nextSeqNo := seq.newCopy(localSpace.space(), objSeq, obj.wrep,
                                       obj.seqNoCnt.value()); 
            ELSE
              (* I'm a sequencer! *)
              IF debug_level >= 5 THEN
                debug.print(5, "SetupNewCopy: we are a sequencer." & 
                  " Notify obj seq of our copy.");
              END;
              seqConn := localSpace.newSpace(objSeq);
              
              (* Create our new object info. *)
              IF debug_level >= 5 THEN
                debug.print(5, "SetupNewCopy: register local copy.");
              END;
              objInfo := SharedObjRTF.RegisterObject(obj, obj.wrep,
                                                     seqConn, FALSE, 
                                                     obj.seqNoCnt.value());
              
              (* We haven't seen this object before so register a new
                 copy!  *)
              nextSeqNo := objSeq.newCopy (localSpace.space(), objSeq,
                                           obj.wrep, obj.seqNoCnt.value());
              IF debug_level >= 5 THEN
                debug.print(5, "SetupNewCopy: register copy with obj seq." &
                  " seqno=" & nextSeqNo.fmt(10) & ".");
              END;
            END;
	  END;

	  IF obj.seqNoCnt.value().compare(nextSeqNo) >= 0 THEN
	    (* The copy is recent enough, so we can just use it! *)
            IF debug_level >= 5 THEN
              debug.print(5, "SetupNewCopy: local copy is new enough.");
            END;
	    nextSeqNo := NIL;
	  ELSE
	    (* Register the object so that we know what sequence number we
	       need, and we also can tell that we haven't got it since the
	       object sequence number counter will be less than the object
	       info sequence number! *)
            IF debug_level >= 5 THEN
              debug.print(5, "SetupNewCopy: local copy is too old.");
            END;
	    objInfo.seqNo := nextSeqNo;
	  END;
        EXCEPT
        | SharedObj.Error(el) => 
          RAISE Pickle.Error("SharedObj.Error: " & Debug.AtomListToText(el));
        | NetObj.Error(el) => 
          RAISE Pickle.Error("NetObj.Error: " & Debug.AtomListToText(el));
        END;
      END;
    END;

    (* make sure nobody changes obj or objInfo *)
    AcquireReadLock(obj);
    Thread.Acquire(objInfo);

    IF obj.seqNoCnt.value().compare(objInfo.seqNo) >= 0 THEN
      IF debug_level >= 5 THEN
        debug.print(5, "SetupNewCopy: new copy at least" &
          " as new as local one.");
      END;
      (* the object we are unpickling is newer, so replace the one
         in the object info structure {\it iff} the value of the
         object stored in the objInfo structure is older than the
         value in the objInfo structure.  This corresponds to the
         case where someone is waiting for a new version and we have
         not yet received it.  Otherwise, we will always use the
         older, local version.  This is because we want all the
         events to be applied locally so that all the callbacks will
         be applied. *) 
      oldObj := SharedObjRTF.GetObjRef(objInfo);
      IF oldObj # NIL AND oldObj # obj THEN
        (* we need to release objInfo while we try to acquire the read
           lock. *)
        Thread.Release(objInfo);

        (* Make sure nobody changes oldObj *)
        AcquireReadLock(oldObj);

        (* reaquire *)
        Thread.Acquire(objInfo);

        IF oldObj.seqNoCnt.value().compare(objInfo.seqNo) < 0 THEN
          VAR 
            (* Save the mutex, so we can release it after we have
               blown away obj. *)
            tempMu    := obj.mu;
          BEGIN
            IF debug_level >= 5 THEN
              debug.print(5, "SetupNewCopy: local copy is out of date, "&
                "set new seqno=" & obj.seqNoCnt.value().fmt(10) & ".");
            END;

            (* we need to release objInfo while we try to call set
               obj oldObj *)
            Thread.Release(objInfo);

            (* Got the one we are waiting for, so use it!  First,
               set the old sequence number counter to the new value.
               Then, replace the counter in the new object with the
               old one.  Must do this, since there could be events
               blocked on the old counter.  Copy the other old fields,
               as well. *)
            TRY
              oldObj.seqNoCnt.set(obj.seqNoCnt.value());
            EXCEPT
            | EventCounter.Invalid =>
              (* We know this will succeed, so don't do anything.  The
                 only way to get this exception is if 
                 obj.seqNoCnt < oldObj.seqNoCnt, which we check for
                 above (since obj.seqNoCnt < objInfo.seqNo) *)
            END;
            obj.seqNoCnt := oldObj.seqNoCnt;
            obj.mu := oldObj.mu;
            obj.timeliness := oldObj.timeliness;
            obj.callbacks := oldObj.callbacks;

            LOCK objInfo DO
              IF debug_level >= 5 THEN
                debug.print(5, "SetupNewCopy: replace old obj.");
              END;
              objInfo.obj := WeakRef.FromRef(obj, 
                                             SharedObjRTF.CleanupSharedObj);
            END;

            obj.seqNoCnt.release();
            tempMu.releaseRead(); (* instead of ReleaseReadLock(obj); *)
          END;
        ELSE
          (* Drop the newer one in favour of the older one already
             in use on this machine! *)
          IF debug_level >= 5 THEN
            debug.print(5, "SetupNewCopy: use older copy.");
          END;
          obj := oldObj;
          Thread.Release(objInfo);
        END;
      ELSIF oldObj = NIL THEN
        (* The old ref is gone, so we can just use the new one. *)
        IF debug_level >= 5 THEN
          debug.print(5, "SetupNewCopy: old ref gone.  use new copy.");
        END;
        objInfo.obj := WeakRef.FromRef(obj, SharedObjRTF.CleanupSharedObj);
        objInfo.hasCopy := TRUE;
        Thread.Release(objInfo);
      ELSE
        IF debug_level >= 5 THEN
          debug.print(5, "SetupNewCopy: old and new copies are the same.");
        END;
        Thread.Release(objInfo);
      END;
    ELSE
      (* We will arrive here in two ways.  First, the first time
         through, if the object is too old for the sequencer, we
         will arrive here.  In that case, this assignment is
         harmless, and we will request the the new copy below.  In
         the second case, while waiting for the newer 
         copy of the object to arrive, another old copy arrives!
         So, we'll force this thread to also request a newer copy.
         A tad wasteful, but that's ok, since it will force it to
         wait for a reasonable copy too.  Otherwise we'll have to add
         a condition variable to all objects, which is very
         wasteful, considering the tiny chance of this happening. *)
      IF debug_level >= 5 THEN
        debug.print(5, "SetupNewCopy: obj too old, ask for a recent one.");
      END;
      nextSeqNo := objInfo.seqNo;
      Thread.Release(objInfo);
    END;

    ReleaseReadLock(obj);

    (* Request a newer copy of the object.  While we are waiting for the
       reply, the netobj package will unpickle the reply object, which
       will flow though here (since it will retrieve an objInfo entry
       above). *)
    IF nextSeqNo # NIL THEN
      (* Get one that is recent enough!   If there is an exception, we
         could manually remove our copy, but let's just let it all
         clean up itself someday ... if there are no more refs, the
         copy will be collected, for example. *)
      IF debug_level >= 5 THEN
        debug.print(5, "SetupNewCopy: asking for more recent one.");
      END;
      TRY
        nextSeqNo := NEW(SharedObj.SequenceNumber).init(nextSeqNo);
        nextSeqNo.dec();
        obj := from.get(obj.wrep, nextSeqNo);
      EXCEPT
      | EventNumber.Overflow => 
        Process.Crash("Event Number Underflowed!");
      | SharedObj.Error(el) => 
        RAISE Pickle.Error("SharedObj.Error: " & Debug.AtomListToText(el));
      | NetObj.Error(el) => 
        RAISE Pickle.Error("NetObj.Error: " & Debug.AtomListToText(el));
      END;
      IF debug_level >= 5 THEN
        debug.print(5, "SetupNewCopy: got more recent one.");
      END;
    END;
        
    rd.noteRef(obj, id);
    RETURN obj;
  END SetupNewCopy;

TYPE
  InhibitSpecial = Pickle.Special OBJECT
                     reason: TEXT;
                   OVERRIDES
                     write := WriteInhibitTransmission;
                     read  := ReadInhibitTransmission;
                   END;

PROCEDURE WriteInhibitTransmission (           self: InhibitSpecial;
                                    <*UNUSED*> ref : REFANY;
                                    <*UNUSED*> wr  : Pickle.Writer   )
  RAISES {Pickle.Error} =
  BEGIN
    RAISE Pickle.Error(self.reason);
  END WriteInhibitTransmission;

PROCEDURE ReadInhibitTransmission (           self: InhibitSpecial;
                                   <*UNUSED*> rd  : Pickle.Reader;
                                   <*UNUSED*> id  : Pickle.RefID    ):
  REFANY RAISES {Pickle.Error} =
  BEGIN
    RAISE Pickle.Error(self.reason);
  END ReadInhibitTransmission;

PROCEDURE InhibitTransmission (tc: INTEGER; reason: TEXT) =
  BEGIN
    Pickle.RegisterSpecial(NEW(InhibitSpecial, sc := tc, reason := reason));
  END InhibitTransmission;

(*---------------------Object Locking Procedures---------------------*)

PROCEDURE ReleaseReadLock(self: SharedObj.T) =
  BEGIN
    IF debug_level >= 12 THEN
      debug.print(12, "ReleaseReadLock: releasing...");
    END;
    self.mu.releaseRead();
    IF debug_level >= 12 THEN
      debug.print(12, "ReleaseReadLock: released.");
    END;
  END ReleaseReadLock;

PROCEDURE AcquireReadLock(self: SharedObj.T) =
  BEGIN
    IF debug_level >= 12 THEN
      debug.print(12, "AcquireReadLock: acquiring...");
    END;
    IF self.mu = NIL THEN
      EVAL SharedObj.Init(self);
    END;
    self.mu.acquireRead();
    IF debug_level >= 12 THEN
      debug.print(12, "AcquireReadLock: acquired.");
    END;
  END AcquireReadLock;

PROCEDURE ReleaseWriteLock(self: SharedObj.T) =
  BEGIN
    IF debug_level >= 12 THEN
      debug.print(12, "ReleaseWriteLock: releasing...");
    END;
    self.mu.releaseWrite();
    IF debug_level >= 12 THEN
      debug.print(12, "ReleaseWriteLock: released.");
    END;
  END ReleaseWriteLock;

PROCEDURE AcquireWriteLock(self: SharedObj.T) =
  BEGIN
    IF debug_level >= 12 THEN
      debug.print(12, "AcquireWriteLock: acquiring...");
    END;
    IF self.mu = NIL THEN
      EVAL SharedObj.Init(self);
    END;
    self.mu.acquireWrite();
    IF debug_level >= 12 THEN
      debug.print(12, "acquireWrite: acquired.");
    END;
  END AcquireWriteLock;

BEGIN
  handleMu := NEW(MUTEX);
  Pickle.ReRegisterSpecial(NEW(NetObjTSpecial, sc := TYPECODE(NetObj.T)));
END SharedObjStubLib.
