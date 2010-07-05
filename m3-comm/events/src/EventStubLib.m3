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
 * Created On      : Sat Apr 15 17:39:20 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Jun 18 19:04:27 1998
 * Update Count    : 137
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.3  2010-01-16 02:33:01  hosking
 * Initial support for LONGINT and LONGCARD.
 * Still needs vetting for proper conversion of 64-bit and 32-bit LONGINT/LONGCARD
 * (the integrated Win32 backend still treats LONGINT as 32 bits).
 *
 * Revision 1.2  2001/12/02 00:20:37  wagner
 * add copyright notes, fix overrides for cm3, and make everything compile
 *
 * added: events/COPYRIGHT-COLUMBIA
 * added: events/src/COPYRIGHT-COLUMBIA
 * modified: events/src/Event.i3
 * modified: events/src/Event.m3
 * modified: events/src/EventConn.i3
 * modified: events/src/EventConn.m3
 * modified: events/src/EventCounter.i3
 * modified: events/src/EventCounter.m3
 * modified: events/src/EventHandle.i3
 * modified: events/src/EventIO.i3
 * modified: events/src/EventNumber.i3
 * modified: events/src/EventNumber.m3
 * modified: events/src/EventNumberF.i3
 * modified: events/src/EventPort.i3
 * modified: events/src/EventPort.m3
 * modified: events/src/EventProtocol.i3
 * modified: events/src/EventRd.i3
 * modified: events/src/EventRd.m3
 * modified: events/src/EventSpaceID.i3
 * modified: events/src/EventSpaceID.m3
 * modified: events/src/EventStubLib.i3
 * modified: events/src/EventStubLib.m3
 * modified: events/src/EventWireRep.i3
 * modified: events/src/EventWireRep.m3
 * modified: events/src/EventWr.i3
 * modified: events/src/EventWr.m3
 * modified: events/src/EventWrF.i3
 * modified: events/src/HostInfo.i3
 * modified: events/src/HostInfo.m3
 * modified: events/src/RdWrMutex.i3
 * modified: events/src/RdWrMutex.m3
 * modified: events/src/Work.i3
 * modified: events/src/WorkerPool.i3
 * modified: events/src/WorkerPool.m3
 * modified: events/src/Zombie.i3
 * modified: events/src/m3makefile
 * modified: events/src/m3overrides
 *
 * Revision 1.1.1.1  2001/12/02 00:06:45  wagner
 * Blair MacIntyre's events library
 *
 * Revision 1.7  1998/07/02 21:41:13  bm
 * small bug fixes
 *
 * Revision 1.6  1997/08/04 20:15:12  bm
 * Fixed BRANDs
 *
 * Revision 1.5  1997/03/12 21:46:56  bm
 * bug fix -- object in same process as sequencer, race condition
 *
 * Revision 1.4  1997/01/23 15:26:39  bm
 * Lots of little bug fixes.
 *
 * 
 * HISTORY
 *   Large parts were gratuitously stolen from the NetObject StubLib.m3.
 *   The copyright in it is this: *)
(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* StubLib.m3 *)
(* Last modified on Tue Jan 31 08:47:30 PST 1995 by kalsow *)
(*      modified on Wed Aug 31 16:52:41 PDT 1994 by wobber *)
(*      modified on Wed Feb 10 17:10:17 PST 1993 by owicki *)

UNSAFE MODULE EventStubLib EXPORTS EventStubLib, EventProtocol, EventIO;
   (* unsafe because of marshalling code *)
   
IMPORT Pickle2 AS Pickle, Event, EventRd, EventWr, EventHandle, EventNumber, 
	EventNumberF;
IMPORT Atom, AtomList, Rd, RTType, Wr, Text, (* TextF, *)
       Thread, RdClass, WrClass, UnsafeRd, UnsafeWr,
       FloatMode, Swap, Text8; 
(* IMPORT IO, Fmt;*)

REVEAL RdClass.Private <: MUTEX;
REVEAL WrClass.Private <: MUTEX;

(* Most if not all of the following could be inline in stub code *)

(* A "Handle" is returned to a client when it wants to send an event.
   Clients of "Handle" must avoid accessing the streams concurrently,
   so we lock the embedded streams before giving them to the client.  
*)

REVEAL
  Handle = EventHandle.Public BRANDED "EventStubLib.Handle" OBJECT next: Handle END;

(* Pickle.Reader and Pickle.Writer subtypes and free list headers *)
TYPE SpecWr = Pickle.Writer OBJECT
  next: SpecWr;
  END;

TYPE SpecRd = Pickle.Reader OBJECT
  next: SpecRd;
  END;

VAR mu: MUTEX;
    freeWr: SpecWr := NIL;
    freeRd: SpecRd := NIL;
    freeHandle: Handle := NIL;

PROCEDURE StartCreate() : Handle =
  VAR c := NewHandle(Event.New());
  BEGIN
    c.wr := EventRd.ToWr(c.event.rd);
    IF c.wr = NIL THEN
      c.wr := NEW(EventWr.T).init();
    END;
    RETURN c;
  END StartCreate;
  
PROCEDURE EndCreate(c: Handle; id: Byte8; stubProt: StubProtocol; 
                    num: EventNumber.T): Event.T =
  VAR ev := c.event;
  <*FATAL Wr.Failure, Thread.Alerted *>
  BEGIN
    ev.init(id, stubProt, num);
    Wr.Flush(c.wr);
    IF ev.rd # NIL THEN
      EVAL ev.rd.init(c.wr);
    ELSE
      ev.rd := EventRd.New(c.wr);
    END;
    FreeHandle(c);
    RETURN ev;
  END EndCreate;
  
PROCEDURE StartRead(ev: Event.T) : Handle =
  VAR c := NewHandle(ev);
  BEGIN
    ev.addRef();
    (* Don't do the seek here -- it's unprotected, and unnecessary *)
    (* EVAL ev.rd.seek(0, FALSE); *)
    RETURN c;
  END StartRead;
  
PROCEDURE EndRead(c: Handle) =
  BEGIN
    c.event.dropRef();
    FreeHandle(c);
  END EndRead;

PROCEDURE NewHandle(ev: Event.T): Handle = 
  VAR ph: Handle;
  BEGIN
    LOCK mu DO
      IF freeHandle # NIL THEN
        ph := freeHandle; freeHandle := freeHandle.next;
      ELSE
        ph := NEW(Handle, next:=NIL);
      END;
    END;
    ph.event := ev;
    ph.cur := 0;
    ph.wr := NIL;
    RETURN ph;
  END NewHandle; 

PROCEDURE FreeHandle(h: Handle) =
  BEGIN
    LOCK mu DO
      (* Don't free the event, since we could (conceptually) have
         multiple handles on one event.  Basically, a handle is either
         used to create an event, in which case we don't free it, or
         to read an event, in which case there could be multiple
         readers. *)
      h.event := NIL;
      h.wr := NIL;
      h.next := freeHandle;
      freeHandle := h;
    END;
  END FreeHandle; 

PROCEDURE ChangeNumber(ev: Event.T; en: EventNumber.T) =
  BEGIN
    (* Assign the new event number. *)
    EVAL ev.num.init(en);
    ev.hdr.numLo := ev.num.lo;
    ev.hdr.numHi := ev.num.hi;

    (* Store it in the header in the correct format for the event.
       Remember, the data in the event must be in the format of the
       sending machine! *)
    IF ev.hdr.rep.intFmt # NativeRep.intFmt THEN
      IF NOT NativeEndian(ev.hdr.rep) THEN
        ev.hdr.numLo := Swap.Swap4(ev.num.lo);
        ev.hdr.numHi := Swap.Swap4(ev.num.hi);
      END;
    END;
  END ChangeNumber;

(* Can't use RdCopy.ToProc(), which this is an almost exact copy of,
   since it locks the stream.  This means we cannot lock it before the
   call, which we need to do to guarantee another reader does not read
   some first. *)
PROCEDURE ToProc (rd  : Rd.T;
                  proc: PROCEDURE (READONLY a: ARRAY OF CHAR) 
                          RAISES {Wr.Failure, Thread.Alerted};
                  length: CARDINAL := LAST(CARDINAL)): CARDINAL
  RAISES {Wr.Failure, Rd.Failure, Thread.Alerted} =
  VAR i := 0;
  BEGIN
    (*RdClass.Lock(rd);*)
    EVAL rd.seek(0, FALSE);                   (* reset so we read it all! *)
    TRY
      LOOP
        WITH len = MIN(length - i, rd.hi - rd.cur) DO
          IF len > 0 THEN
            proc(SUBARRAY(rd.buff^, rd.st + rd.cur - rd.lo, len));
            INC(i, len);
            INC(rd.cur, len);
          END;
        END;
        IF i = length OR rd.seek(rd.cur, FALSE) = RdClass.SeekResult.Eof THEN
          EXIT;
        END;
      END;
    FINALLY
      (*RdClass.Unlock(rd);*)
    END;
    RETURN i;
  END ToProc;

PROCEDURE Write (wr: Wr.T; ev: Event.T)
  RAISES {Rd.Failure, Wr.Failure, Thread.Alerted} =
    VAR
      h: UNTRACED REF MsgHeader;
      from := LOOPHOLE(ADR(ev.from), UNTRACED REF ARRAY [0..65535] OF CHAR);
      ts := LOOPHOLE(ADR(ev.ts), UNTRACED REF ARRAY [0..65535] OF CHAR);
      count: INTEGER := 0;

    PROCEDURE PutString (READONLY a: ARRAY OF CHAR)
      RAISES {Wr.Failure, Thread.Alerted} =
      BEGIN
        UnsafeWr.FastPutString(wr, a);
      END PutString;

  BEGIN
    LOCK wr DO
      h := LOOPHOLE(ADR(wr.buff[wr.st + wr.cur - wr.lo]),
                    UNTRACED REF MsgHeader);

      IF wr.hi - wr.cur < BYTESIZE(MsgHeader) THEN
        RAISE Wr.Failure(AtomList.List1(
                             Atom.FromText("not enough buffer space")));
      END;
      h^ := ev.hdr;
      INC(wr.cur, BYTESIZE(MsgHeader));
      wr.putString(SUBARRAY(from^, 0, NUMBER(ev.from.byte)));
      wr.putString(SUBARRAY(ts^, 0, NUMBER(ev.ts.r)));

      WITH c = StartRead(ev) DO
        TRY 
          TRY
            AcquireRd(c);
            count := ToProc(ev.rd, PutString);
          EXCEPT
          | Event.Error =>  <*ASSERT FALSE*>
            (* should never happen, since AcquireRd should end up
               trying to seek(0), if anything, since it's a new handle *)
          END;
        FINALLY
          ReleaseRd(c);
          EndRead(c);
        END;
      END;
      (* IO.Put("EventStubLib.Write put " & Fmt.Int(count) & " bytes\n");*)
    END;
  END Write;

PROCEDURE Read (rd: Rd.T): Event.T RAISES {Event.Error, Rd.Failure, 
                                              Thread.Alerted} =
  VAR ev := Event.New();
      hi, lo: Word32;
  BEGIN
    IF rd.hi - rd.cur < BYTESIZE(MsgHeader) THEN
      WITH hdrChar = LOOPHOLE(ADR(ev.hdr), 
                              UNTRACED REF ARRAY [0..65535] OF CHAR) DO
        IF rd.getSub(SUBARRAY(hdrChar^, 0, 
                              BYTESIZE(MsgHeader))) # BYTESIZE(MsgHeader) THEN
          RaiseUnmarshalFailure();
        END;
      END;
    ELSE
      WITH hdr = LOOPHOLE(ADR(rd.buff[rd.st+rd.cur-rd.lo]), 
                      UNTRACED REF MsgHeader) DO
        ev.hdr := hdr^;
        INC(rd.cur, BYTESIZE(MsgHeader));
      END;
    END;
    WITH from = LOOPHOLE(ADR(ev.from), 
                         UNTRACED REF ARRAY [0..65535] OF CHAR) DO
      IF rd.getSub(SUBARRAY(from^, 0, 
                            NUMBER(ev.from.byte))) # NUMBER(ev.from.byte) THEN
        RaiseUnmarshalFailure();
      END;
    END;
    WITH ts = LOOPHOLE(ADR(ev.ts), UNTRACED REF ARRAY [0..65535] OF CHAR) DO
      IF rd.getSub(SUBARRAY(ts^, 0, NUMBER(ev.ts.r))) # NUMBER(ev.ts.r) THEN
        RaiseUnmarshalFailure();
      END;
    END;
    ev.prot := ev.hdr.prot;
    hi := ev.hdr.numHi;
    lo := ev.hdr.numLo;
    IF ev.hdr.rep.intFmt # NativeRep.intFmt THEN
      IF NOT NativeEndian(ev.hdr.rep) THEN
        ev.prot := Swap.Swap4(ev.prot);
        hi := Swap.Swap4(hi);
        lo := Swap.Swap4(lo);
      END;
    END;
    (* ev.num := NEW(EventNumber.T, hi := hi, lo := lo); *)
    ev.num.hi := hi;
    ev.num.lo := lo;

    ev.rd := EventRd.FromRd(rd, ev.rd);
    RETURN ev;
  END Read;

(*---------marshalling/unmarshalling routines-----------*)


PROCEDURE AcquireRd(c: Handle) RAISES {Event.Error, Rd.Failure, 
                                       Thread.Alerted} =
  BEGIN 
    Thread.Acquire(c);
    WITH rd = c.event.rd DO
      Thread.Acquire(rd.extMu);
      RdClass.Lock(rd);
      IF rd.cur # c.cur THEN
        IF rd.seek(c.cur, FALSE) # RdClass.SeekResult.Ready THEN
          RaiseUnmarshalFailure();
        END;
      END;
    END;
  END AcquireRd;

PROCEDURE ReleaseRd(c: Handle) =
  BEGIN 
    WITH rd = c.event.rd DO
      c.cur := rd.cur;
      RdClass.Unlock(rd);
      Thread.Release(rd.extMu);
      Thread.Release(c);
    END;
  END ReleaseRd;

PROCEDURE FastGetSub(c: Handle; VAR (*OUT*) str: ARRAY OF CHAR): CARDINAL
    RAISES {Rd.Failure, Thread.Alerted} =
  VAR ret: CARDINAL;
  BEGIN 
    LOCK c DO
      WITH rd = c.event.rd DO
        LOCK rd.extMu DO
          LOCK rd DO
            IF rd.cur # c.cur THEN
              IF rd.seek(c.cur, FALSE) # RdClass.SeekResult.Ready THEN
                (* A CARDINAL guaranteed to be # NUMBER(str) *)
                RETURN LAST(CARDINAL)-NUMBER(str);
              END;
            END;
            ret := UnsafeRd.FastGetSub(rd, str);
            c.cur := rd.cur;
          END;
        END;
      END;
    END;
    RETURN ret;
  END FastGetSub;

PROCEDURE FastGetChar(c: Handle): CHAR 
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR ret: CHAR;
  BEGIN 
    LOCK c DO
      WITH rd = c.event.rd DO
        LOCK rd.extMu DO
          LOCK rd DO
            IF rd.cur # c.cur THEN
              EVAL rd.seek(c.cur, FALSE);
            END;
            ret := UnsafeRd.FastGetChar(rd);
            c.cur := rd.cur;
          END;
        END;
      END;
    END;
    RETURN ret;
  END FastGetChar;

PROCEDURE InChars(c: Handle; VAR arr: ARRAY OF CHAR)
    RAISES {Event.Error, Rd.Failure, Thread.Alerted} =
  BEGIN
    IF c.event.hdr.rep.charSet # NativeRep.charSet THEN
      RaiseUnsupportedDataRep();
    END;
    IF FastGetSub(c, arr) # NUMBER(arr) THEN
      RaiseUnmarshalFailure();
    END;
  END InChars;

PROCEDURE OutChars(c: Handle; READONLY arr: ARRAY OF CHAR)
    RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    c.wr.putString(arr);
  END OutChars;

PROCEDURE OutTextI(c: Handle; READONLY t: TEXT)
    RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    Wr.PutText(c.wr, t);
  END OutTextI; 

PROCEDURE InBytes(c: Handle; VAR arr: ARRAY OF Byte8)
    RAISES {Event.Error, Rd.Failure, Thread.Alerted} =
  VAR p := LOOPHOLE(ADR(arr[0]), UNTRACED REF ARRAY [0..65335] OF CHAR);
  BEGIN
    IF FastGetSub(c, SUBARRAY(p^, 0, NUMBER(arr))) # NUMBER(arr) THEN
      RaiseUnmarshalFailure();
    END;
  END InBytes;

PROCEDURE OutBytes(c: Handle; READONLY arr: ARRAY OF Byte8)
    RAISES {Wr.Failure, Thread.Alerted} =
  VAR p := LOOPHOLE(ADR(arr[0]), UNTRACED REF ARRAY [0..65335] OF CHAR);
  BEGIN
    c.wr.putString(SUBARRAY(p^, 0, NUMBER(arr)));
  END OutBytes;

CONST
  BigEndianFmt   = 16;
  IntFmt32Little = 0;
  IntFmt64Little = 1;
  IntFmt32Big    = BigEndianFmt;
  IntFmt64Big    = BigEndianFmt + 1;

  FloatIEEE      = 0;
  FloatOther     = 1;

TYPE
  Int64 = ARRAY [0..1] OF Int32;

(* this code is integer-length dependent *)
(* we also rely on the invariant that MsgRd/MsgWr will
   provide contiguous 8-byte chunks at proper alignment ..
   as long as there is no intervening flush *)

PROCEDURE InInteger(c: Handle; 
                    min := FIRST(INTEGER);
                    max := LAST(INTEGER)): INTEGER
    RAISES {Event.Error, Rd.Failure, Thread.Alerted} =
  VAR i: INTEGER := 0;
  BEGIN
    WITH e = c.event, hdr = e.hdr, rd = e.rd, rep = hdr.rep DO
      TRY
        AcquireRd(c);
        IF rep.intFmt = NativeRep.intFmt THEN
          i := LOOPHOLE(AlignRd(rd, BYTESIZE(INTEGER)), UNTRACED REF INTEGER)^;
          INC(rd.cur, BYTESIZE(INTEGER));
        ELSE
          CASE rep.intFmt OF
          | IntFmt32Little, IntFmt32Big =>
            VAR ii: Int32 :=
                LOOPHOLE(AlignRd(rd, BYTESIZE(Int32)), UNTRACED REF Int32)^;
            BEGIN
              INC(rd.cur, BYTESIZE(Int32));
              IF NOT NativeEndian(rep) THEN ii := Swap.Swap4(ii); END;
              i := ii;
            END;
          | IntFmt64Little =>
            (* this can only be 64 -> 32 bit conversion *)
            (* no 64 -> 64 bit byte swap at this point in time *)
            VAR
              ip := LOOPHOLE(AlignRd(rd, BYTESIZE(Int64)), UNTRACED REF Int64);
            BEGIN
              INC(rd.cur, BYTESIZE(Int64));
              IF (ip[0] < 0 AND ip[1] # -1) OR (ip[0] >= 0 AND ip[1] # 0) THEN
                RaiseUnsupportedDataRep();
              END;
              IF NativeEndian(rep) THEN
                i := ip[0];
              ELSE
                i := Swap.Swap4(ip[0]);
              END;
            END;
          ELSE
            RaiseUnsupportedDataRep();
          END;
        END;
        IF i < min OR i > max THEN RaiseUnmarshalFailure(); END;
        RETURN i;
      FINALLY
        ReleaseRd(c);
      END;
    END;
  END InInteger;

PROCEDURE InInt32(c: Handle; 
                    min := FIRST(Int32);
                    max := LAST(Int32)): Int32
    RAISES {Event.Error, Rd.Failure, Thread.Alerted} =
  VAR i: Int32 := 0;
  BEGIN
    WITH e = c.event, hdr = e.hdr, rd = e.rd, rep = hdr.rep DO
      TRY
        AcquireRd(c);
        IF rep.intFmt = NativeRep.intFmt THEN
          i := LOOPHOLE(AlignRd(rd, BYTESIZE(Int32)), UNTRACED REF Int32)^;
          INC(rd.cur, BYTESIZE(Int32));
        ELSE
          CASE rep.intFmt OF
          | IntFmt32Little, IntFmt32Big, IntFmt64Little =>
            i := LOOPHOLE(AlignRd(rd, BYTESIZE(Int32)), UNTRACED REF Int32)^;
            INC(rd.cur, BYTESIZE(Int32));
            IF NOT NativeEndian(rep) THEN i := Swap.Swap4(i); END;
          ELSE
            RaiseUnsupportedDataRep();
          END;
        END;
        IF i < min OR i > max THEN RaiseUnmarshalFailure(); END;
        RETURN i;
      FINALLY
        ReleaseRd(c);
      END;
    END;
  END InInt32;

PROCEDURE AlignRd(rd: Rd.T; nb: CARDINAL) : ADDRESS
    RAISES {Event.Error, Rd.Failure, Thread.Alerted} =
  VAR diff := rd.cur MOD nb;
      res: ADDRESS;
  BEGIN
    (*
     * Original comment said 
     *    "Here we rely on the alignment invariants of MsgRd.T"  
     * I think that the invariants are held for "EventRd.T", but I'm
     * not positive.  Time will tell.
     *) 
    IF diff # 0 THEN
      VAR n := rd.cur + nb - diff; BEGIN
        IF n > rd.hi THEN RaiseUnmarshalFailure(); END;
        rd.cur := n;
      END;
    END;
    IF rd.cur = rd.hi THEN EVAL rd.seek(rd.cur, FALSE); END;
    IF rd.hi - rd.cur < nb THEN RaiseUnmarshalFailure(); END;
    res := ADR(rd.buff[rd.st + rd.cur - rd.lo]);
    RETURN res;
  END AlignRd;

  (*
    A MsgRd fragment must be 64-bit aligned.  Fragments of types call, return,
    or call-failed, must either be a multiple of 8 bytes in length, or else
    contain the end-of-message.    MsgRd buffers must be 64-bit aligned in
    length.
   *)
  (* NOTE:  The previous comment, again, is not very relevant.  Our
     EventRd buffers are 64-bit aligned, but definition.   Each
     message is in it's own EventRd.T, so its header is definately
     aligned.
   *)

PROCEDURE OutInteger(c: Handle; i: INTEGER)
    RAISES {Wr.Failure, Thread.Alerted} =
  VAR ip := LOOPHOLE(AlignWr(c.wr, BYTESIZE(INTEGER)), UNTRACED REF INTEGER);
  BEGIN
    ip^ := i;
    INC(c.wr.cur, BYTESIZE(INTEGER));
  END OutInteger;

PROCEDURE OutEventNumber(c: Handle; n: EventNumber.T)
    RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    OutInteger(c, n.lo);
    OutInteger(c, n.hi);
  END OutEventNumber;

PROCEDURE InEventNumber(c: Handle): EventNumber.T
     RAISES {Event.Error, Rd.Failure, Thread.Alerted} =
  BEGIN
    RETURN NEW(EventNumber.T, lo := InInteger(c), hi := InInteger(c));
  END InEventNumber;

PROCEDURE OutInt32(c: Handle; i: Int32)
    RAISES {Wr.Failure, Thread.Alerted} =
  VAR ip := LOOPHOLE(AlignWr(c.wr, BYTESIZE(Int32)), UNTRACED REF Int32);
  BEGIN
    ip^ := i;
    INC(c.wr.cur, BYTESIZE(Int32));
  END OutInt32;

PROCEDURE AlignWr(wr: Wr.T; align: CARDINAL) : ADDRESS
    RAISES {Wr.Failure, Thread.Alerted} =
  VAR diff := wr.cur MOD align;
      res: ADDRESS;
  BEGIN
    (*
     * Original comment said 
     *    "here we rely on the alignment invariants of MsgWr.T"
     * I think that the invariants are held for "EventWr.T", but I'm
     * not positive.  Time will tell.
     *) 
    IF diff # 0 THEN INC(wr.cur, align-diff); END;
    IF wr.cur = wr.hi THEN wr.seek(wr.cur); END;
    res := ADR(wr.buff[wr.st + wr.cur - wr.lo]);
    RETURN res;
  END AlignWr;

  (*
    A MsgWr fragment must be 64-bit aligned.  Fragments of types call, return,
    or call-failed, must either be a multiple of 8 bytes in length, or else
    contain the end-of-message.  MsgWr buffers must be 64-bit aligned in
    length.
   *)
  (* NOTE:  The previous comment, again, is not very relevant.  Our
     EventWr buffers are 64-bit aligned, but definition.   Each
     message is written into a new EventWr.T, so its header is definately
     aligned.
   *)

PROCEDURE InByte(c: Handle; 
                 max := LAST(Byte8)): Byte8
     RAISES {Event.Error, Rd.Failure, Thread.Alerted} =
  VAR b: Byte8;
  BEGIN
    TRY
      b := LOOPHOLE(FastGetChar(c), Byte8);
    EXCEPT
    | Rd.EndOfFile => RaiseUnmarshalFailure();
    END;
    IF b > max THEN
      RaiseUnmarshalFailure();
    END;
    RETURN b
  END InByte;

PROCEDURE OutByte(c: Handle; b: Byte8)
   RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    UnsafeWr.FastPutChar(c.wr, LOOPHOLE(b, CHAR));
  END OutByte;

TYPE MSpec = {Pickle, Text, Texts};

PROCEDURE InRef(c: Handle; tc: INTEGER): REFANY
     RAISES {Event.Error, Rd.Failure, Thread.Alerted} =
  VAR r: REFANY;
      srd: SpecRd;  
  BEGIN
    CASE InByte(c) OF
    | ORD(MSpec.Pickle) => 
      TRY
        srd := NewRd(c);
        VAR ok := FALSE; BEGIN
          TRY
            AcquireRd(c);
            RdClass.Unlock(c.event.rd);
            r := srd.read();
            ok := TRUE;
          FINALLY
            IF ok THEN FreeRd(srd); END;
            RdClass.Lock(c.event.rd);
            ReleaseRd(c);
          END;
        END;
      EXCEPT
      | Rd.EndOfFile => RaiseUnmarshalFailure();
      | Pickle.Error(cause) =>
          RAISE Event.Error(
            AtomList.List2(UnmarshalFailure, Atom.FromText(cause)));
      END;
      IF tc # -1 AND NOT RTType.IsSubtype(TYPECODE(r), tc) THEN
        RaiseUnmarshalFailure();
      END;      
    | ORD(MSpec.Text) => r := InText(c);
    | ORD(MSpec.Texts) => r := InTexts(c);
    ELSE RaiseUnmarshalFailure();
    END;
    RETURN r;
  END InRef;

PROCEDURE OutRef(c: Handle; r: REFANY)
   RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    TYPECASE r OF
    | TEXT(x) => OutByte(c, ORD(MSpec.Text)); OutText(c, x);
    | REF ARRAY OF TEXT(x) => OutByte(c, ORD(MSpec.Texts)); OutTexts(c, x);
    ELSE 
      OutByte(c, ORD(MSpec.Pickle)); 
      TRY
        VAR swr := NewWr(c); ok := FALSE; BEGIN
          TRY
            swr.write(r);
            ok := TRUE;
          FINALLY
            IF ok THEN FreeWr(swr); END;
          END;
        END;
      EXCEPT
      | Pickle.Error(cause) =>
          RAISE Wr.Failure(AtomList.List1(Atom.FromText(cause)));
      END;
    END;
  END OutRef;

PROCEDURE InCardinal(c: Handle;
     lim: CARDINAL := LAST(CARDINAL)): CARDINAL
     RAISES {Event.Error, Rd.Failure, Thread.Alerted} =
  BEGIN
    RETURN InInteger(c, 0, lim);
  END InCardinal;

PROCEDURE OutCardinal(c: Handle; card: CARDINAL)
   RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    OutInteger(c, card);
  END OutCardinal;

PROCEDURE InReal(c: Handle): REAL
    RAISES {Event.Error, Rd.Failure, Thread.Alerted} =
  VAR i: REAL;
  BEGIN
    IF c.event.hdr.rep.floatFmt # NativeRep.floatFmt THEN
      RaiseUnsupportedDataRep();
    END;
    IF FastGetSub(c, LOOPHOLE(i, ARRAY [0..BYTESIZE(REAL)-1] OF CHAR)) # 
      BYTESIZE(REAL) THEN
      RaiseUnmarshalFailure();
    END;
    IF NOT NativeEndian(c.event.hdr.rep) THEN i := SwapReal(i); END;
    RETURN i;
  END InReal;

PROCEDURE OutReal(c: Handle; i: REAL)
   RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    c.wr.putString(LOOPHOLE(i, ARRAY [0..BYTESIZE(REAL)-1] OF CHAR));
  END OutReal;

PROCEDURE InLongreal(c: Handle): LONGREAL
    RAISES {Event.Error, Rd.Failure, Thread.Alerted} =
  VAR i: LONGREAL;
  BEGIN
    IF c.event.hdr.rep.floatFmt # NativeRep.floatFmt THEN
      RaiseUnsupportedDataRep();
    END;
    IF FastGetSub(c,
                  LOOPHOLE(i, ARRAY [0..BYTESIZE(LONGREAL)-1] OF CHAR)) #
                  BYTESIZE(LONGREAL) THEN
      RaiseUnmarshalFailure();
    END;
    IF NOT NativeEndian(c.event.hdr.rep) THEN i := SwapLongReal(i); END;
    RETURN i;
  END InLongreal;

PROCEDURE OutLongreal(c: Handle; i: LONGREAL)
   RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    c.wr.putString(LOOPHOLE(i, ARRAY [0..BYTESIZE(LONGREAL)-1] OF CHAR));
  END OutLongreal;

PROCEDURE InExtended(c: Handle): EXTENDED
    RAISES {Event.Error, Rd.Failure, Thread.Alerted} =
  BEGIN
    RETURN LOOPHOLE(InLongreal(c), EXTENDED);
  END InExtended;

PROCEDURE OutExtended(c: Handle; i: EXTENDED)
   RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    c.wr.putString(LOOPHOLE(i, ARRAY [0..BYTESIZE(EXTENDED)-1] OF CHAR));
  END OutExtended;

PROCEDURE InBoolean(c: Handle) : BOOLEAN
    RAISES {Event.Error, Rd.Failure, Thread.Alerted} =
  VAR res: BOOLEAN;
  BEGIN
    TRY
      res := FastGetChar(c) # '\000';
    EXCEPT
    | Rd.EndOfFile => RaiseUnmarshalFailure();
    END;
    RETURN res;
  END InBoolean;

PROCEDURE OutBoolean(c: Handle; bool: BOOLEAN)
    RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF bool THEN
      UnsafeWr.FastPutChar(c.wr, '\001');
    ELSE
      UnsafeWr.FastPutChar(c.wr, '\000');
    END;
  END OutBoolean;

PROCEDURE InText(c: Handle) : TEXT
   RAISES {Event.Error, Rd.Failure, Thread.Alerted} =
  VAR len: INTEGER;
  VAR text: TEXT;
  VAR buf: REF ARRAY OF CHAR;
  BEGIN
    len := InInt32(c);
    IF len = -1 THEN
      RETURN NIL;
    ELSIF len < 0 THEN
      RaiseUnmarshalFailure();
    ELSE
      buf := NEW(REF ARRAY OF CHAR, len+1);
      InChars(c, buf^);
      buf[len] := '\000';
      text := Text8.New(buf^);
    END;
    RETURN text;
  END InText;

PROCEDURE OutText(c: Handle; text: TEXT)
   RAISES {Wr.Failure, Thread.Alerted} =
  VAR len: INTEGER;
  BEGIN
    IF text # NIL THEN
      len := Text.Length(text);
    ELSE
      len := -1;
    END;
    OutInt32(c, len);
    IF len > 0 THEN OutTextI(c, text); END;
  END OutText;

PROCEDURE InTexts(c: Handle) : REF ARRAY OF TEXT
   RAISES {Event.Error, Rd.Failure, Thread.Alerted} =
  VAR n: CARDINAL;
  VAR rt: REF ARRAY OF TEXT;
  BEGIN
    n := InInt32(c, 0);
    IF n = 0 THEN
      RETURN NIL;
    END;
    rt := NEW(REF ARRAY OF TEXT, n);
    IF n > 0 THEN
      FOR i := 0 TO n-1 DO rt[i] := InText(c); END;
    END;
    RETURN rt;
  END InTexts;

PROCEDURE OutTexts(c: Handle; rt: REF ARRAY OF TEXT)
    RAISES {Wr.Failure, Thread.Alerted} =
  VAR n: CARDINAL;
  BEGIN
    IF (rt = NIL) THEN n := 0 ELSE n := NUMBER(rt^); END;
    OutInt32(c, n);
    IF n > 0 THEN
      FOR i := 0 TO n-1 DO OutText(c, rt[i]); END;
    END;
  END OutTexts;
 
(* Procedures for Pickling -- free list management *)

PROCEDURE IsEventWriter(wr: Pickle.Writer): BOOLEAN =
  BEGIN
    RETURN ISTYPE(wr, SpecWr);
  END IsEventWriter;

PROCEDURE IsEventReader(rd: Pickle.Reader): BOOLEAN =
  BEGIN
    RETURN ISTYPE(rd, SpecRd);
  END IsEventReader;

PROCEDURE NewWr(c: Handle): SpecWr = 
  VAR pwr: SpecWr;
  BEGIN
    LOCK mu DO
      IF freeWr # NIL THEN
        pwr := freeWr; freeWr := freeWr.next;
      ELSE
        pwr := NEW(SpecWr, next:=NIL);
      END;
    END;
    pwr.wr := c.wr;
    RETURN pwr
  END NewWr;

PROCEDURE FreeWr(pwr: SpecWr) =
  BEGIN
    LOCK mu DO
      pwr.next := freeWr;
      pwr.wr := NIL;
      freeWr := pwr;
    END;
  END FreeWr;

PROCEDURE NewRd(c: Handle): SpecRd = 
  VAR prd: SpecRd;
  BEGIN
    LOCK mu DO
      IF freeRd # NIL THEN
        prd := freeRd; freeRd := freeRd.next;
      ELSE
        prd := NEW(SpecRd, next:=NIL);
      END;
    END;
    prd.rd := c.event.rd;
    RETURN prd
  END NewRd;

PROCEDURE FreeRd(prd: SpecRd) =
  BEGIN
    LOCK mu DO
      prd.rd := NIL;
      prd.next := freeRd;
      freeRd := prd;
    END;
  END FreeRd;

PROCEDURE RaiseUnmarshalFailure() RAISES {Event.Error} =
  BEGIN
    RaiseError(UnmarshalFailure);
  END RaiseUnmarshalFailure;

PROCEDURE RaiseUnsupportedDataRep() RAISES {Event.Error} =
  BEGIN
    RaiseError(UnsupportedDataRep);
  END RaiseUnsupportedDataRep;

PROCEDURE RaiseError(a: Atom.T) RAISES {Event.Error} =
  BEGIN
    RAISE Event.Error(AtomList.List1(a));
  END RaiseError;

PROCEDURE SwapReal(i: REAL) : REAL =
  BEGIN
    RETURN LOOPHOLE(Swap.Swap4(LOOPHOLE(i, Int32)), REAL);
  END SwapReal;

TYPE LR = RECORD a, b: Int32; END;

PROCEDURE SwapLongReal(i: LONGREAL) : LONGREAL =
  VAR res: LONGREAL;
  BEGIN
    WITH p = LOOPHOLE(ADR(i), UNTRACED REF LR) DO
      WITH r = LOOPHOLE(ADR(res), UNTRACED REF LR) DO
        r.a := Swap.Swap4(p.b);
        r.b := Swap.Swap4(p.a);
      END;
    END;
    RETURN res;
  END SwapLongReal;

PROCEDURE NativeEndian(rep: DataRep) : BOOLEAN =
  BEGIN
    RETURN (rep.intFmt >= BigEndianFmt) = (Swap.endian = Swap.Endian.Big);
  END NativeEndian;

PROCEDURE ChooseIntFmt(): Byte8 =
  BEGIN
    IF BYTESIZE(INTEGER) = 8 THEN
      IF Swap.endian = Swap.Endian.Little THEN
        RETURN IntFmt64Little;
      ELSE
        RETURN IntFmt64Big;
      END;
    ELSE
      IF Swap.endian = Swap.Endian.Little THEN
        RETURN IntFmt32Little;
      ELSE
        RETURN IntFmt32Big;
      END;
    END;
  END ChooseIntFmt;

PROCEDURE ChooseFloatFmt(): Byte8 =
  BEGIN
    IF FloatMode.IEEE THEN
      RETURN FloatIEEE;
    ELSE
      RETURN FloatOther;
    END;
  END ChooseFloatFmt;

BEGIN
  NativeRep := DataRep{id := 0,
                       intFmt := ChooseIntFmt(),
                       charSet := 0,
                       floatFmt := ChooseFloatFmt()};
  UnmarshalFailure := Atom.FromText("EventStubLib.UnmarshalFailure");
  UnsupportedDataRep := Atom.FromText("EventStubLib.UnsupportedDataRep");

  (* Initialization for Pickle specials and free list *)
  mu := NEW(MUTEX);

  (* IO.Put("EventStubLib.m3 Initialization: NativeRep=0+" & 
         Fmt.Int(NativeRep.intFmt, 16) & "+" & 
         Fmt.Int(NativeRep.floatFmt, 16) & "+0\n"); *)
END EventStubLib.
