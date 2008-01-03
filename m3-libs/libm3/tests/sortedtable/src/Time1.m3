(* Copyright 1990 Digital Equipment Corporation.            *)
(* Distributed only by permission.                          *)
(* Last modified on Tue Aug  2 15:46:08 PDT 1994 by kalsow  *)
(*      modified on Wed Oct  7 16:19:55 PDT 1992 by johnh   *)
(*      modified on Fri Mar  9 16:18:40 PST 1990 by mcjones *)

MODULE Time1 EXPORTS Main;

IMPORT Fmt, IO, Time;

(* IMPORT SIntIntTbl; *)
IMPORT SortedIntIntTbl AS SIntIntTbl;

VAR
  start : Time.T;
  stop  : Time.T;

PROCEDURE BeginTest (type: TEXT;  N: CARDINAL) =
  BEGIN
    IO.Put (type & " ( N := " & Fmt.Int (N) & " )...\n");
    start := Time.Now ();
  END BeginTest;

PROCEDURE EndTest () =
  BEGIN
    stop := Time.Now ();
    IO.Put ("  "& Fmt.LongReal (stop - start, Fmt.Style.Fix, 2) &" seconds\n");
  END EndTest;

PROCEDURE TimeInts (N: CARDINAL) =
  VAR
    t: SIntIntTbl.T;
    s: SIntIntTbl.Iterator;
    key, v: INTEGER;
  BEGIN
    BeginTest ("Integer", N);

    t := NEW (SIntIntTbl.Default).init ();
    FOR i := 1 TO N DO
      EVAL t.put (2 * i, i);
    END;

    FOR i := 1 TO N DO
      EVAL t.get (2 * i, v);
      EVAL t.get (2 * i + 1, v);
    END;

    s := t.iterateOrdered (up := TRUE);
    s.seek (1 * 2);
    WHILE s.next (key, v) DO END;

    s := t.iterateOrdered (up := FALSE);
    s.seek (2 * N);
    WHILE s.next (key, v) DO END;

    FOR i := 1 TO N DO
      EVAL t.delete (2 * i, v);
    END;

    EndTest ();
  END TimeInts;

BEGIN
  TimeInts (2048);
  TimeInts (4096);
  TimeInts (8192);
  TimeInts (16384);
  TimeInts (32768)
END Time1.
