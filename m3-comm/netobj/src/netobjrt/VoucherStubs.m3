(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* VoucherStubs.m3 *)
(* Last modified on Thu Feb  2 18:40:18 PST 1995 by wobber *)

UNSAFE MODULE VoucherStubs EXPORTS NetObjInit, NetStream;
   
IMPORT Voucher;
IMPORT AtomList, NetObj, StubLib, Protocol, Transport;
IMPORT RdClass, WrClass, Thread, Rd, Wr;

FROM Protocol IMPORT MsgHeader, Op;

TYPE
  Procs = {ClaimRd, ClaimWr};

  T = Voucher.T OBJECT
  OVERRIDES
    claimRd := SurrogateClaimRd;
    claimWr := SurrogateClaimWr;
  END;

TYPE
  SurrogateRd = Rd.T OBJECT
    c: StubLib.Conn;
    v: T;
    eof := FALSE;
  OVERRIDES
    seek := SRSeek;
    close := SRClose;
  END;

  SurrogateWr = Wr.T OBJECT
    c: StubLib.Conn;
    v: T;
    (* offset: INTEGER;    (* wr.c.wr.cur + wr.offset = wr.cur *) *)
    dataOp := Op.StreamData;
  OVERRIDES
    seek := SWSeek;
    (* putString := SWPutString; *)
    flush := SWFlush;
    close := SWClose;
  END;


(* dispatcher for Voucher invocations *)

PROCEDURE Invoke(
    c: StubLib.Conn;
    obj: NetObj.T;
    rep: StubLib.DataRep;
    <*UNUSED*> stubProt: StubLib.StubProtocol)
    RAISES {NetObj.Error, Rd.Failure, Wr.Failure, Thread.Alerted} =
  VAR v := NARROW(obj, Voucher.T);
  BEGIN
    CASE StubLib.InInt32(c, rep) OF
    | ORD(Procs.ClaimRd) => ClaimRdStub(c, v, rep);
    | ORD(Procs.ClaimWr) => ClaimWrStub(c, v, rep);
    ELSE
        StubLib.RaiseUnmarshalFailure();
    END;
  END Invoke;
  
PROCEDURE ClaimRdStub(c: StubLib.Conn; v: Voucher.T;
       <*UNUSED*> rep: StubLib.DataRep)
    RAISES {NetObj.Error, Rd.Failure, Wr.Failure, Thread.Alerted} =
  VAR
    rd: Rd.T;
    pos: CARDINAL;
  BEGIN
    rd := v.claimRd();
    IF rd = NIL THEN
      RAISE NetObj.Error(AtomList.List1(NetObj.MissingObject));
    END;
    StubLib.StartResult(c);
    pos := Rd.Index(rd);
    StubLib.OutInteger(c, pos);
    c.wr.nextMsg();
    SendOp(c, Op.StreamData);
    TRY
      PlugRd(rd, c.wr);
      c.wr.nextMsg();
      (* send result of reading from stream *)
      SendOp(c, Op.StreamOK);
    EXCEPT
    | Rd.Failure(x) =>
        c.wr.nextMsg();
        SendOp(c, Op.StreamError);
        StubLib.OutRef(c, x);
    END;
    c.wr.nextMsg();
    CASE RecvOp(c).private OF
    | ORD(Op.StreamClose) =>
        TRY
          Rd.Close(rd);
          SendOp(c, Op.StreamOK);
        EXCEPT
        | Rd.Failure(x) =>
            SendOp(c, Op.StreamError);
            StubLib.OutRef(c, x);
        END;
    | ORD(Op.StreamRelease) =>
    ELSE StubLib.RaiseUnmarshalFailure();
    END;
  END ClaimRdStub;
  
PROCEDURE ClaimWrStub(c: StubLib.Conn; v: Voucher.T;
       <*UNUSED*> rep: StubLib.DataRep)
    RAISES {NetObj.Error, Rd.Failure, Wr.Failure, Thread.Alerted} =
  VAR
    wr: Wr.T;
    pos: CARDINAL;
  BEGIN
    (* wr might be lost here if an error is encountered during
       outbound marshalling.  This will be fixed by object cleanup. *)
    wr := v.claimWr();
    IF wr = NIL THEN
      RAISE NetObj.Error(AtomList.List1(NetObj.MissingObject));
    END;
    StubLib.StartResult(c);
    pos := Wr.Index(wr);
    StubLib.OutInteger(c, pos);
    REPEAT
      c.wr.nextMsg();
      TRY
        CASE RecvOp(c).private OF
        | ORD(Op.StreamData) => PlugWr(c.rd, wr); Wr.Flush(wr);
        | ORD(Op.StreamClose) =>  Wr.Close(wr); wr := NIL;
        | ORD(Op.StreamRelease) =>  wr := NIL;
        ELSE StubLib.RaiseUnmarshalFailure();
        END;
        SendOp(c, Op.StreamOK);
      EXCEPT
      | Wr.Failure(x) =>
          SendOp(c, Op.StreamError);
          StubLib.OutRef(c, x);
      END;
    UNTIL (wr = NIL);
  END ClaimWrStub;


(* stub code for surrogate vouchers *)

PROCEDURE SurrogateClaimRd(v: T) : Rd.T RAISES {NetObj.Error, Thread.Alerted} =
  VAR steal := FALSE;
      c: StubLib.Conn;
      pos: CARDINAL;
      rd: SurrogateRd;
      rep: StubLib.DataRep;
  BEGIN
    TRY
      c := StubLib.StartCall(v, StubLib.SystemStubProtocol);
      TRY
        StubLib.OutInt32(c, ORD(Procs.ClaimRd));
        rep := StubLib.AwaitResult(c);
        pos := StubLib.InInteger(c, rep, 0);
        rep := RecvOp(c);
        IF rep.private # ORD(Op.StreamData) THEN
          StubLib.RaiseUnmarshalFailure();
        END;
        (* from this point on, we assume that we "own" c.rd *)
        rd := NEW(SurrogateRd,
          c := c,
          v := v,
          buff := c.rd.buff,
          st := c.rd.st + (c.rd.cur - c.rd.lo),
          lo := pos,
          hi := pos + (c.rd.hi - c.rd.cur),
          cur := pos,
          intermittent := TRUE,
          seekable := FALSE,
          closed := FALSE);
        steal := TRUE;
      FINALLY
        IF NOT steal THEN c.loc.free(c, FALSE); END;
      END;
    EXCEPT
    | Rd.Failure(ec) => StubLib.RaiseCommFailure(ec);
    | Wr.Failure(ec) => StubLib.RaiseCommFailure(ec);
    END;
    RETURN rd;
  END SurrogateClaimRd;
  
PROCEDURE SurrogateClaimWr(v: T) : Wr.T RAISES {NetObj.Error, Thread.Alerted} =
  VAR steal := FALSE;
      c: StubLib.Conn;
      pos: CARDINAL;
      wr: SurrogateWr;
      rep: StubLib.DataRep;
  BEGIN
    TRY
      c := StubLib.StartCall(v, StubLib.SystemStubProtocol);
      TRY
        StubLib.OutInt32(c, ORD(Procs.ClaimWr));
        rep := StubLib.AwaitResult(c);
        pos := StubLib.InInteger(c, rep, 0);
        SendOp(c, Op.StreamData);
        wr := NEW(SurrogateWr,
          c := c,
          v := v,
          buff := c.wr.buff,
        (* offset := pos - c.wr.cur, *)
          st := c.wr.st + (c.wr.cur - c.wr.lo),
          lo := pos,
          hi := pos + (c.wr.hi - c.wr.cur),
          cur := pos,
          buffered := TRUE,
          seekable := FALSE,
          closed := FALSE);
        steal := TRUE;
      FINALLY
        IF NOT steal THEN c.loc.free(c, FALSE); END;
      END;
    EXCEPT
    | Rd.Failure(ec) => StubLib.RaiseCommFailure(ec);
    | Wr.Failure(ec) => StubLib.RaiseCommFailure(ec);
    END;
    RETURN wr;
  END SurrogateClaimWr;
  

(* surrogate reader methods *)

PROCEDURE SRSeek(rd: SurrogateRd; pos: CARDINAL;
                 dontBlock: BOOLEAN) : RdClass.SeekResult
                 RAISES {Rd.Failure, Thread.Alerted} =
  VAR mrd := rd.c.rd;
  VAR res: RdClass.SeekResult;
  BEGIN
    (* rd.cur = rd.hi = pos,  mrd.cur = mrd.hi *)
    IF rd.eof THEN
      RETURN RdClass.SeekResult.Eof;
    END;
    res := mrd.seek(pos, dontBlock);
    IF res = RdClass.SeekResult.Ready THEN
      rd.buff := mrd.buff;
      rd.st := mrd.st;
      rd.lo := rd.hi;
      rd.hi := rd.lo + (mrd.hi - mrd.lo);
    ELSIF res = RdClass.SeekResult.Eof THEN
      rd.eof := TRUE;
      VAR err: AtomList.T; BEGIN
        EVAL CheckResult(rd.c, err);
        IF err # NIL THEN RAISE Rd.Failure(err); END;
      END;
    END;
    RETURN res;
  END SRSeek;  

PROCEDURE SRClose(rd: SurrogateRd)
    RAISES {Rd.Failure, Thread.Alerted} =
  BEGIN
    TerminateRd(rd, Op.StreamClose);
  END SRClose;

PROCEDURE ReleaseRd(rd: Rd.T)
    RAISES {Rd.Failure, Thread.Alerted} =
  BEGIN
    TYPECASE rd OF
    | SurrogateRd(s) =>
        RdClass.Lock(s);
        TRY
          IF NOT s.closed THEN
            TerminateRd(s, Op.StreamRelease);
          END;
        FINALLY
          s.closed := TRUE;
          s.cur := s.hi;
          s.lo := s.hi;
          s.buff := NIL;
          RdClass.Unlock(s);
        END;
    ELSE
    END;
  END ReleaseRd;

PROCEDURE TerminateRd(rd: SurrogateRd; op: Op)
    RAISES {Rd.Failure, Thread.Alerted} =
  VAR ok := FALSE;
  BEGIN
    TRY
      IF NOT rd.eof THEN
        rd.eof := TRUE;
        EVAL RecvOp(rd.c);  (* flush the reader status message *)
      END;
      TRY
        SendOp(rd.c, op);
        rd.c.wr.nextMsg();
      EXCEPT
      | Wr.Failure(x) => RAISE Rd.Failure(x);
      END;
      VAR err: AtomList.T; BEGIN
        ok := CheckResult(rd.c, err);
        IF err # NIL THEN RAISE Rd.Failure(err); END;
      END;
    FINALLY
      rd.c.loc.free(rd.c, ok);
    END;
  END TerminateRd;

(* surrogate writers *)

  (*
           --  target(surrogateWr) = target(concreteWr)
           --  surrogateWr.flush() implies concreteWr.flush()
           --  surrogateWr.close() implies concreteWr.close()
           --  an failure on concreteWr will result in a
                       similar failure on surrogateWr
   
       Note that surrogateWr.close() does not close or even shutdown
       the underlying connection.
  *)
  
PROCEDURE SWSeek(wr: SurrogateWr; <*UNUSED*> n: CARDINAL)
    RAISES {Wr.Failure, Thread.Alerted} =
  VAR mwr := wr.c.wr;
  BEGIN
    mwr.cur := mwr.hi;
    mwr.seek(mwr.cur);
    wr.buff := mwr.buff;
    wr.st := mwr.st + (mwr.cur - mwr.lo);
    wr.lo := wr.cur;
    wr.hi := wr.cur + (mwr.hi - mwr.lo);
  END SWSeek;
  
(*
PROCEDURE SWPutString(wr: SurrogateWr; READONLY arr: ARRAY OF CHAR)
    RAISES {Wr.Failure, Thread.Alerted} =
  VAR mwr := wr.c.wr;
  BEGIN
    mwr.putString(arr);
    wr.buff := mwr.buff;
    wr.st := mwr.st + (mwr.cur - mwr.lo);
    wr.lo := MIN(mwr.lo + wr.offset, 0);
    wr.cur := mwr.cur + wr.offset;
    wr.hi := mwr.hi + wr.offset;
  END SWPutString;
*)

PROCEDURE SWFlush(wr: SurrogateWr)
    RAISES {Wr.Failure, Thread.Alerted} =
  VAR mwr := wr.c.wr;
  BEGIN
    (* make sure the message writer sees any unflushed chars *)
    INC(mwr.cur, wr.cur - wr.lo);
    (* this call flushes the message stream *)
    mwr.nextMsg();
    VAR err: AtomList.T; BEGIN
      EVAL CheckResult(wr.c, err);
      IF err # NIL THEN RAISE Wr.Failure(err); END;
    END;
    SendOp(wr.c, wr.dataOp);
    (* offset := wr.cur - c.wr.cur, *)
    wr.st := mwr.st + (mwr.cur - mwr.lo);
    wr.lo := wr.cur;
    wr.hi := wr.cur + (mwr.hi - mwr.lo);
  END SWFlush;

PROCEDURE SWClose(wr: SurrogateWr)
    RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    TerminateWr(wr, Op.StreamClose);
  END SWClose;

PROCEDURE ReleaseWr(wr: Wr.T)
    RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    TYPECASE wr OF
    | SurrogateWr(s) =>
        WrClass.Lock(s);
        TRY
          IF NOT s.closed THEN
            TerminateWr(s, Op.StreamRelease);
          END;
        FINALLY
          s.closed := TRUE;
          s.cur := s.hi;
          s.lo := s.hi;
          s.buff := NIL;
          WrClass.Unlock(s);
        END;
    ELSE
    END;
  END ReleaseWr;

PROCEDURE TerminateWr(wr: SurrogateWr; op: Op)
    RAISES {Wr.Failure, Thread.Alerted} =
  VAR mwr := wr.c.wr;
      ok := FALSE;
  BEGIN
    wr.dataOp := op;
    TRY
      SWFlush(wr);
      mwr.nextMsg();
      VAR err: AtomList.T; BEGIN
        ok := CheckResult(wr.c, err);
        IF err # NIL THEN RAISE Wr.Failure(err); END;
      END;
    FINALLY
      wr.c.loc.free(wr.c, ok);
    END;
  END TerminateWr;


(* utilities *)

PROCEDURE SendOp(c: StubLib.Conn; op: Op) =
  VAR h: UNTRACED REF MsgHeader;
      wr := c.wr;
  BEGIN
    h := LOOPHOLE(ADR(wr.buff[wr.st+wr.cur-wr.lo]), UNTRACED REF MsgHeader);
    INC(wr.cur, BYTESIZE(MsgHeader));
    h.hdr := StubLib.NativeRep;
    h.hdr.private := ORD(op);
  END SendOp;

PROCEDURE RecvOp(c: StubLib.Conn) : StubLib.DataRep
     RAISES {Rd.Failure, Thread.Alerted} =
  VAR h: UNTRACED REF MsgHeader;
      rd := c.rd;
  BEGIN
    IF NOT rd.nextMsg() OR rd.hi - rd.cur < BYTESIZE(MsgHeader) THEN
      RAISE Rd.Failure(AtomList.List1(StubLib.UnmarshalFailure));
    END;
    h := LOOPHOLE(ADR(rd.buff[rd.st+rd.cur-rd.lo]), UNTRACED REF MsgHeader);
    INC(rd.cur, BYTESIZE(MsgHeader));
    RETURN h.hdr;
  END RecvOp;

PROCEDURE CheckResult(c: StubLib.Conn; VAR res: AtomList.T): BOOLEAN
    RAISES {Thread.Alerted} =
  VAR rep: StubLib.DataRep;
  BEGIN
    TRY
      rep := RecvOp(c);
      CASE rep.private OF
      | ORD(Op.StreamOK) => RETURN TRUE;
      | ORD(Op.StreamError) =>
          res := StubLib.InRef(c, rep, TYPECODE(AtomList.T));
          RETURN TRUE;
      ELSE res := NIL;
      END;
    EXCEPT
    | NetObj.Error(x) => res := x;
    | Rd.Failure(x) => res := x;
    END;
    IF res = NIL THEN
      res := AtomList.List1(StubLib.UnmarshalFailure);
    END;
    RETURN FALSE;
  END CheckResult;

PROCEDURE PlugRd(rd: Rd.T; wr: Wr.T)
     RAISES {Rd.Failure, Wr.Failure, Thread.Alerted} =
  VAR dontBlock := rd.intermittent;
  BEGIN
    (* here "wr" is a locked network stream *)
    RdClass.Lock(rd);
    TRY
      IF rd.closed THEN Die(); END;
      REPEAT
        IF rd.cur # rd.hi THEN
          wr.putString(SUBARRAY(rd.buff^, (rd.cur-rd.lo)+rd.st, rd.hi-rd.cur));
          rd.cur := rd.hi;
          dontBlock := rd.intermittent;
        ELSIF dontBlock THEN
          (* flush all data before blocking on an intermittent reader *)
          wr.flush();
          dontBlock := FALSE;
        END;
      UNTIL (rd.seek(rd.cur, dontBlock) = RdClass.SeekResult.Eof);
    FINALLY
      RdClass.Unlock(rd);
    END;
  END PlugRd;
  
PROCEDURE PlugWr(rd: Rd.T; wr: Wr.T)
     RAISES {Rd.Failure, Wr.Failure, Thread.Alerted} =
  BEGIN
    (* here "rd" is a locked network stream *)
    IF rd.closed THEN Die(); END;
    REPEAT
      IF rd.cur # rd.hi THEN
        Wr.PutString(wr,
              SUBARRAY(rd.buff^, (rd.cur-rd.lo)+rd.st, rd.hi-rd.cur));
        rd.cur := rd.hi;
      END;
    UNTIL (rd.seek(rd.cur, FALSE) = RdClass.SeekResult.Eof);
  END PlugWr;
  
PROCEDURE InitVoucherStubs() =
  BEGIN
    StubLib.Register(
      TYPECODE(Voucher.T), StubLib.SystemStubProtocol, TYPECODE(T), Invoke); 
  END InitVoucherStubs;

EXCEPTION FatalError;

PROCEDURE Die() RAISES {} =
  <* FATAL FatalError *>
  BEGIN
    RAISE FatalError;
  END Die;

BEGIN
END VoucherStubs.



(* old comments surrounding "plug" *)

(*
PROCEDURE PlugRd(wr: WrT; rd: Rd.T)
     RAISES {Rd.Failure, Wr.Failure, Thread.Alerted};

       "PlugRd" takes a "Rd.T" argument of unspecified class and
       efficiently copies all data from it into the outbound msg
       writer.  "PlugRd" terminates normally when "rd" reports
       "EndOfFile".  "PlugWr" does not call "Rd.Close" on its
       "rd" argument.  Nor does it invoke the "nextMsg" method to
       terminate the current outbound message.

PROCEDURE PlugWr(rd: RdT; wr: Wr.T)
     RAISES {Rd.Failure, Wr.Failure, Thread.Alerted};
       
       "PlugWr" takes a "Wr.T" argument of unspecified class and
       efficiently copies all data from the inbound msg reader into
       this writer.   "PlugWr" terminates normally when "rd" reports
       "EndOfFile".  "PlugWr" does not call "Wr.Close" on its "wr"
       argument.
*)

