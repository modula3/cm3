(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* StubProt.m3 *)
(* Last modified on Mon Feb 14 13:04:21 PST 1994 by wobber *)
(*      modified on Thu Apr  1 08:49:03 PST 1993 by owicki *)
(*      modified on Fri Sep 11 16:32:48 PDT 1992 by evers  *)

UNSAFE MODULE StubProt EXPORTS StubLib, StubLibPrivate;
   
IMPORT NetObj, NetObjRep, NetObjRT,
       Protocol, Transport, TransportUtils, WireRep;
IMPORT AtomList, Thread, Rd, Wr, RTType;
IMPORT RdClass, WrClass; (* UnsafeWr *)

FROM Protocol IMPORT Header, CallHeader, Op;

(* Since clients of "Conn" must avoid accessing them concurrently,
   we operate on the embedded streams without locking them.
*)

EXCEPTION FatalError;  <* FATAL FatalError *>

(*
REVEAL RdClass.Private <: MUTEX;
REVEAL WrClass.Private <: MUTEX;
*)

TYPE ObjectStack = RECORD
    pos: CARDINAL := 0;
    objs: REF ARRAY OF NetObj.T := NIL;
  END;
  
CONST DefaultObjStackSize = 8;

REVEAL
  Conn = Transport.Conn BRANDED OBJECT
    objStack: ObjectStack := ObjectStack {};
    inObj: BOOLEAN := FALSE;
  END;

(* The field "t.objStack" is used to record the set of network objects
   marshalled during any single method invocation (at either client or
   server).  This record is required for cleanup at the termination of
   the call.   The "t.objStack" field is managed by the network object
   runtime and should not be modified by any transport implementation. *)

REVEAL
  Transport.T = TransportUtils.Public BRANDED OBJECT
  OVERRIDES
    serviceCall := ServiceCall;
  END;
  
PROCEDURE ServiceCall(<*UNUSED*> tt: Transport.T; c: Conn) : BOOLEAN
    RAISES {Thread.Alerted} =
  BEGIN
      TRY
        VAR
          dispatcher: Dispatcher;
          obj: NetObj.T;
          prot: StubProtocol;
          rd := c.rd;
          h := LOOPHOLE(ADR(rd.buff[rd.st+rd.cur-rd.lo]),
                                     UNTRACED REF CallHeader);
        BEGIN
          IF rd.hi - rd.cur < BYTESIZE(CallHeader) OR
                     h.hdr.private # ORD(Op.MethodCall) THEN
               RaiseUnmarshalFailure();
          END;
          INC(rd.cur, BYTESIZE(CallHeader));
          IF h.hdr.intFmt # NativeRep.intFmt THEN
            IF NOT NativeEndian(h.hdr) THEN
              prot := Swap32(h.prot);
            END;
          END;
          obj := NetObjRT.FindTarget(h.obj, prot, dispatcher);
          TRY
            c.objStack.pos := 0;
            dispatcher(c, obj, h.hdr, prot);
            IF (c.objStack.pos # 0) THEN
              (* take this out, later *)
              (* UnsafeWr.FastPutChar(c.wr, '\000'); *)
              c.wr.nextMsg();
              IF NOT rd.nextMsg() OR rd.hi - rd.cur < BYTESIZE(Header) THEN
                RETURN FALSE;
              END;
              VAR hh := LOOPHOLE(ADR(rd.buff[rd.st+rd.cur-rd.lo]),
                                              UNTRACED REF Header)^;
              BEGIN
                INC(rd.cur, BYTESIZE(Header));
                IF hh.private # ORD(Op.ResultAck) THEN RETURN FALSE; END;
              END;
            ELSE
              c.wr.nextMsg();
            END;
          FINALLY
            IF (c.objStack.pos # 0) THEN
              NetObjRT.Unpin(SUBARRAY(c.objStack.objs^, 0, c.objStack.pos));
              FOR i := 0 TO c.objStack.pos-1 DO
                c.objStack.objs[i] := NIL;
              END;
            END;
          END;
        END;
      EXCEPT
      | Rd.Failure, Wr.Failure => RETURN FALSE;
      | NetObj.Error(ec) =>
          TRY
            (* this test checks whether we have started marshalling results *)
            IF c.wr.cur = 0 THEN
              VAR wr := c.wr;
                  h := LOOPHOLE(ADR(wr.buff[wr.st+wr.cur-wr.lo]),
                                             UNTRACED REF Header);
              BEGIN
                h^ := NativeRep;
                h.private := ORD(Op.CallFailed);
                INC(wr.cur, BYTESIZE(Header));
              END;
              OutRef(c, ec);
            END;
            c.wr.nextMsg();
          EXCEPT
          | Wr.Failure => RETURN FALSE;
          END;
      END;
    RETURN TRUE;
  END ServiceCall;
  

(* exports to StubLib *)

PROCEDURE StartCall(obj: NetObj.T; stubProt: StubProtocol) : Conn
    RAISES {NetObj.Error, Wr.Failure, Thread.Alerted} =
  VAR c := NARROW(obj.r, Transport.Location).new();
  BEGIN
    c.objStack.pos := 0;
    c.inObj := FALSE;
    VAR wr := c.wr;
        h := LOOPHOLE(ADR(wr.buff[wr.st+wr.cur-wr.lo]),
                                 UNTRACED REF CallHeader);
    BEGIN
      <* ASSERT (wr.hi - wr.cur >= BYTESIZE(CallHeader)) *>
      INC(wr.cur, BYTESIZE(CallHeader));
      h.hdr := NativeRep;
      h.hdr.private := ORD(Op.MethodCall);
      h.prot := stubProt;
      h.obj := obj.w;
    END;
    RETURN c;
  END StartCall;
  
PROCEDURE AwaitResult(c: Conn) : DataRep
    RAISES {NetObj.Error, Rd.Failure, Wr.Failure, Thread.Alerted} =
  VAR hdr: Header;
      rd := c.rd;
  BEGIN
    c.wr.nextMsg();
    TRY
      IF NOT rd.nextMsg() OR rd.hi - rd.cur < BYTESIZE(Header) THEN
        RaiseUnmarshalFailure();
      END;
      hdr := LOOPHOLE(ADR(rd.buff[rd.st+rd.cur-rd.lo]), UNTRACED REF Header)^;
      INC(rd.cur, BYTESIZE(Header));
    EXCEPT
    | Thread.Alerted => RAISE NetObj.Error(AtomList.List1(NetObj.Alerted));
    END;
    CASE hdr.private OF
    | ORD(Op.Return) =>
    | ORD(Op.CallFailed) => 
        RAISE NetObj.Error(InRef(c, hdr, TYPECODE(AtomList.T)));
    ELSE
      RaiseUnmarshalFailure();
    END;
    RETURN hdr;
  END AwaitResult;

PROCEDURE EndCall(c: Conn; reUse: BOOLEAN)
    RAISES {NetObj.Error, Rd.Failure, Wr.Failure, Thread.Alerted} =
  BEGIN
    TRY
      IF c.objStack.pos # 0 THEN
        NetObjRT.Unpin(SUBARRAY(c.objStack.objs^, 0, c.objStack.pos));
        FOR i := 0 TO c.objStack.pos-1 DO
          c.objStack.objs[i] := NIL;
        END;
      END;
      IF reUse AND c.inObj (* OR NOT UnsafeRd.FastEOF(c.rd) *) THEN
        VAR wr := c.wr;
            h := LOOPHOLE(ADR(wr.buff[wr.st+wr.cur-wr.lo]),
                                       UNTRACED REF Header);
        BEGIN
          h^ := NativeRep;
          h.private := ORD(Op.ResultAck);
          INC(wr.cur, BYTESIZE(Header));
          wr.nextMsg();
        END;
      END;
    FINALLY
      c.loc.free(c, reUse);
    END;
  END EndCall;
  
PROCEDURE StartResult(c: Conn)
    RAISES {Wr.Failure, Thread.Alerted} =
  VAR wr := c.wr;
      h := LOOPHOLE(ADR(wr.buff[wr.st+wr.cur-wr.lo]),
                                 UNTRACED REF Header);
  BEGIN
    h^ := NativeRep;
    h.private := ORD(Op.Return);
    INC(wr.cur, BYTESIZE(Header));
  END StartResult;

PROCEDURE OutObject (c: Conn; o: NetObj.T)
    RAISES {Wr.Failure, Thread.Alerted} =
  VAR s: REF ARRAY OF NetObj.T;
  BEGIN
    IF o = NIL THEN
      OutBytes(c, WireRep.NullT.byte);
    ELSE
      OutBytes(c, NetObjRT.InsertAndPin(o).byte);
      s := c.objStack.objs;
      IF s = NIL THEN
        s := NEW(REF ARRAY OF NetObj.T, DefaultObjStackSize);
        c.objStack.objs := s;
      ELSIF c.objStack.pos = NUMBER(s^) THEN
        s := NEW(REF ARRAY OF NetObj.T, 2 * c.objStack.pos);
        SUBARRAY(s^, 0, c.objStack.pos) := c.objStack.objs^;
        c.objStack.objs := s;
      END;
      s[c.objStack.pos] := o;
      INC(c.objStack.pos);
    END;
  END OutObject;

PROCEDURE InObject (c: Conn; tc: INTEGER := -1): NetObj.T
    RAISES {NetObj.Error, Rd.Failure, Thread.Alerted} =
  VAR w: WireRep.T;
      o: NetObj.T;
  BEGIN
    InBytes(c, w.byte);
    IF w = WireRep.NullT THEN RETURN NIL END;
    o := NetObjRT.Find(w, c.loc);
    IF tc # -1 AND NOT RTType.IsSubtype(TYPECODE(o), tc) THEN
      RaiseUnmarshalFailure();
    END;
    c.inObj := TRUE;
    RETURN o;
  END InObject;

BEGIN
END StubProt.
