(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jul  7 08:56:37 PDT 1995 by mhb                      *)
(*      modified on Thu Jan 26 13:59:56 PST 1995 by kalsow                   *)
(*      modified on Tue Jul  6 15:36:02 PDT 1993 by wobber                   *)
(*      created  on ???????????????????????????? by unknown                  *)

MODULE RdCopy;

IMPORT RdClass, WrClass, Rd, Wr, Thread;

PROCEDURE ToWriter (rd: Rd.T; wr: Wr.T; length: LONGINT := LAST(LONGINT)):
  LONGINT RAISES {Rd.Failure, Wr.Failure, Thread.Alerted}<*NOWARN*> =
  (*
  PROCEDURE GetSub (VAR a: ARRAY OF CHAR): CARDINAL
    RAISES {Rd.Failure, Thread.Alerted} =
    BEGIN
      RETURN Rd.GetSub(rd, a);
    END GetSub;
  *)
  PROCEDURE PutString (READONLY a: ARRAY OF CHAR)
    RAISES {Wr.Failure, Thread.Alerted} =
    BEGIN
      Wr.PutString(wr, a);
    END PutString;
  <*FATAL ANY*>
  BEGIN
    RETURN ToProc(rd, PutString, length);
  END ToWriter;

PROCEDURE ToProc (rd  : Rd.T;
                  proc: PROCEDURE (READONLY a: ARRAY OF CHAR) RAISES ANY;
                  length: LONGINT := LAST(LONGINT)): LONGINT
  RAISES ANY (* RAISES(proc) + {Rd.Failure, Thread.Alerted *) =
  VAR i := 0L;
  BEGIN
    RdClass.Lock(rd);
    TRY
      LOOP
        IF Thread.TestAlert() THEN RAISE Thread.Alerted END; (* mhb: 7/7/95 *)
        WITH len = MIN(length - i, rd.hi - rd.cur) DO
          IF len > 0 THEN
            proc(SUBARRAY(rd.buff^, rd.st + ORD(rd.cur - rd.lo), ORD(len)));
            INC(i, len);
            INC(rd.cur, len);
          END;
        END;
        IF i = length OR rd.seek(rd.cur, FALSE) = RdClass.SeekResult.Eof THEN
          EXIT;
        END;
      END;
    FINALLY
      RdClass.Unlock(rd);
    END;
    RETURN i;
  END ToProc;


PROCEDURE FromProc (wr: Wr.T;
                    proc: PROCEDURE (VAR a: ARRAY OF CHAR): CARDINAL
                            RAISES ANY;
                    length: LONGINT := LAST(LONGINT)): LONGINT
  RAISES ANY (* RAISES(proc) + {Rd.Failure, Thread.Alerted *) =
  VAR i := 0L;
  BEGIN
    WrClass.Lock(wr);
    TRY
      LOOP
        WITH len = MIN(length - i, wr.hi - wr.cur) DO
          IF len > 0 THEN
            WITH res = proc(
                         SUBARRAY(wr.buff^, wr.st + ORD(wr.cur - wr.lo), ORD(len))) DO
              INC(i, res);
              INC(wr.cur, res);
              IF res # len THEN EXIT; END;
            END;
          END;
        END;
        IF i = length THEN EXIT; END;
        wr.seek(wr.cur);
      END;
    FINALLY
      WrClass.Unlock(wr);
    END;
    RETURN i;
  END FromProc;


BEGIN
END RdCopy.
