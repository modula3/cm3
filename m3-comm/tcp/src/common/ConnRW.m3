(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Created on Sat Jan 11 15:49:00 PST 1992 by gnelson *)
(* Last modified on Wed Aug 31 11:45:59 PDT 1994 by wobber *)
(*      modified on Tue Aug  4 16:19:31 PDT 1992 by tomr   *)
(*      modified on Tue Jan 28 15:19:47 PST 1992 by muller *)
(*      modified on Sun Jan 12 16:17:03 PST 1992 by meehan *)

MODULE ConnRW;

IMPORT ConnFD, Rd, Wr, RdClass, WrClass, Thread;

TYPE
   RdT = Rd.T BRANDED OBJECT
    fd: ConnFD.T;
  OVERRIDES
    seek := RdSeek;
    close := RdClose
  END;

  WrT = Wr.T BRANDED OBJECT
    fd: ConnFD.T;
  OVERRIDES
    seek := WrSeek;
    flush := WrFlush;
    close := WrClose
  END;

CONST BufferSize = 8192;

PROCEDURE NewRd(fd: ConnFD.T) : Rd.T =
  BEGIN
    RETURN NEW(RdT, fd := fd, 
        buff := NEW(REF ARRAY OF CHAR, BufferSize),
        st := 0,
        lo := 0,
        hi := 0,
        cur := 0,
        intermittent := TRUE,
        seekable := FALSE,
        closed := FALSE);
  END NewRd;

PROCEDURE NewWr(fd: ConnFD.T) : Wr.T =
  BEGIN
    RETURN NEW(WrT, fd := fd,
        buff := NEW(REF ARRAY OF CHAR, BufferSize),
        st := 0,
        lo := 0,
        hi := 0,
        cur := 0,
        buffered := TRUE,
        seekable := FALSE,
        closed := FALSE);
  END NewWr;

PROCEDURE RdSeek(rd: RdT; <*UNUSED*> pos: CARDINAL;
                 dontBlock: BOOLEAN): RdClass.SeekResult
  RAISES {Rd.Failure, Thread.Alerted} =
  VAR 
    len: CARDINAL;  
    timer: LONGREAL;
  BEGIN
    IF dontBlock THEN timer := 0.0D0; ELSE timer := -1.0D0; END;
    TRY
      len := rd.fd.get(rd.buff^, timer);
    EXCEPT
    | ConnFD.TimedOut => RETURN RdClass.SeekResult.WouldBlock;
    (* | ConnFD.Error => RETURN RdClass.SeekResult.Eof; *)
    END;
    IF len > 0 THEN
      rd.lo := rd.cur;
      rd.hi := rd.cur + len;
      RETURN RdClass.SeekResult.Ready
    ELSE
      RETURN RdClass.SeekResult.Eof;
    END;
  END RdSeek;

PROCEDURE RdClose(rd: RdT) RAISES {Rd.Failure} =
  BEGIN
    rd.buff := NIL;
    rd.fd.shutdownIn();
  END RdClose;

PROCEDURE WrSeek(wr: WrT; <*UNUSED*> n: CARDINAL)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN WrFlush(wr) END WrSeek;

PROCEDURE WrFlush(wr: WrT) RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    wr.fd.put(SUBARRAY(wr.buff^, 0, wr.cur-wr.lo));
    wr.lo := wr.cur;
    wr.hi := wr.lo + NUMBER(wr.buff^);
  END WrFlush;
  
PROCEDURE WrClose(wr: WrT) RAISES {Wr.Failure} =
  BEGIN
    wr.buff := NIL;
    wr.fd.shutdownOut();
  END WrClose;
  
BEGIN END ConnRW.
