(* Created on Wed Nov 19 09:46:42 PST 1997 by heydon       *)
(* Last modified on Sun Nov 23 09:47:09 PST 1997 by heydon *)
(* Copyright (C) 1997, Digital Equipment Corporation       *)

MODULE IntPerfTest EXPORTS Main;

(* SYNTAX: IntPerfTest [ numEntries [ numRounds ] ]

   This program times how long it takes to perform various operations
   on the following kinds of tables:

   - IntIntTbl.Default
   - SortedIntIntTbl.Default
   - RedBlackTbl.T
   - SkipListTbl.T

   The type IntIntTbl.Default is included so the performance of
   a hash table implemenation can be compared to implementations
   that also provide ordered iteration (i.e., sorting). Hence, in
   the results printed for IntIntTbl.Default's, the time reported
   for upward iteration ("Iter Up") is simply the time required to
   do an unordered iteration over the table, and the times reported
   for downward iteration ("Iter Dn") and upward and downward
   seeking on an iterator ("Seek Up" and "Seek Dn") are all zero,
   since those operations are not supported on IntIntTbl.Default
   tables.

   Caveat: In the times reported, no effort has been made to factor
   out the loop overhead, which is assumed to be dominated by the
   costs of the table operations themselves.
*)

IMPORT IntIntTbl, SortedIntIntTbl, RedBlackIntIntTbl, SkipListIntIntTbl;
IMPORT FloatMode, Fmt, Lex, Params, Random, Scan, Thread, Time, Wr;
FROM Stdio IMPORT stdout, stderr;

<* FATAL Wr.Failure, Thread.Alerted *>

TYPE
  T = IntIntTbl.T;
  Iter = IntIntTbl.Iterator;
  SortT = SortedIntIntTbl.T;
  SortIter = SortedIntIntTbl.Iterator;

CONST
  Cols = 7;
  ColWidth = 9;
  IterUpCol = 3;
  IterDnCol = 4;

VAR
  NumEntries := 100000;
  NumRounds := 5;
  rnd := NEW(Random.Default).init(fixed := FALSE);
  seed: CARDINAL := rnd.integer(0, 100000);
  totals: ARRAY [1..Cols] OF LONGREAL;
  col: INTEGER;

PROCEDURE InitRandom() =
(* Initialized the random number generator "rnd" using seed "seed". *)
  BEGIN
    EVAL rnd.init(fixed := TRUE);
    FOR i := 1 TO seed DO EVAL rnd.integer() END
  END InitRandom;

PROCEDURE EraseTable(tbl: T) =
  BEGIN
    TYPECASE tbl OF <* NOWARN *>
    | IntIntTbl.Default(tbl) => EVAL tbl.init(sizeHint := NumEntries)
    | SortedIntIntTbl.Default (stbl) => EVAL stbl.init()
    | RedBlackIntIntTbl.T (rbtbl) => EVAL rbtbl.init()
    | SkipListIntIntTbl.T (sltbl) => EVAL sltbl.init(maxSizeHint := NumEntries)
    END
  END EraseTable; 

CONST
  Indent = "    ";
  ColHeader1 = ARRAY OF TEXT{
    "Round", "Insert", "Search", "Iter Up", "Iter Dn",
    "Seek Up", "Seek Dn", "Delete" };
  ColHeader2 = ARRAY OF TEXT{
    "Number", "(put)", "(get)", "(next)", "(next)",
    "(seek)", "(seek)", "(delete)" };

PROCEDURE WriteHeader() =
  BEGIN
    Wr.PutText(stdout, Indent);
    FOR i := 0 TO LAST(ColHeader1) DO
      Wr.PutText(stdout, Fmt.Pad(ColHeader1[i], ColWidth))
    END;
    Wr.PutChar(stdout, '\n');
    Wr.PutText(stdout, Indent);
    FOR i := 0 TO LAST(ColHeader1) DO
      Wr.PutText(stdout, Fmt.Pad(ColHeader2[i], ColWidth))
    END;
    Wr.PutChar(stdout, '\n');
    Wr.PutText(stdout, Indent);
    FOR i := 0 TO LAST(ColHeader1) DO
      Wr.PutChar(stdout, ' ');
      FOR j := 1 TO ColWidth - 1 DO
        Wr.PutChar(stdout, '-')
      END
    END;
    Wr.PutText(stdout, "\n\n");
    Wr.Flush(stdout)
  END WriteHeader;

PROCEDURE WriteValue(v: LONGREAL) =
  VAR t := Fmt.LongReal(v, Fmt.Style.Fix, prec := 3); BEGIN
    Wr.PutText(stdout, Fmt.Pad(t, ColWidth));
    Wr.Flush(stdout);
  END WriteValue;

PROCEDURE NewTest(msg: TEXT) =
  BEGIN
    Wr.PutText(stdout, "  " & msg & " test:\n");
    Wr.Flush(stdout);
    FOR i := 1 TO Cols DO totals[i] := 0.0d0 END;
  END NewTest;

PROCEDURE StartRound(i: CARDINAL) =
  BEGIN
    Wr.PutText(stdout, Indent);
    Wr.PutText(stdout, Fmt.Pad(Fmt.Int(i), ColWidth));
    col := 1;
  END StartRound;

PROCEDURE Finish(start: Time.T) =
  VAR delta := Time.Now() - start; BEGIN
    WriteValue(delta);
    totals[col] := totals[col] + delta;
    INC(col)
  END Finish;

PROCEDURE FinishRound() =
  BEGIN
    Wr.PutText(stdout, "\n");
    Wr.Flush(stdout);
  END FinishRound;

PROCEDURE PrintTotals(tblSz: CARDINAL) =
(* Print the mean time for each test (in seconds) and the mean time
   per operation (in microseconds). The number of operations performed
   is "NumEntries" for the Insert, Search, Seek, and Delete columns, and
   "tblSz" for the Iter columns. *)
  VAR
    nrLong := FLOAT(NumRounds, LONGREAL);
    neLong := FLOAT(NumEntries, LONGREAL);
    szLong := FLOAT(tblSz, LONGREAL);
    opCnt: LONGREAL;
  BEGIN
    Wr.PutText(stdout, Indent);
    Wr.PutText(stdout, Fmt.Pad("Mean", ColWidth));
    FOR i := 1 TO Cols DO
      VAR mean := totals[i] / nrLong; BEGIN
        WriteValue(mean);
      END
    END;
    Wr.PutChar(stdout, '\n');
    Wr.PutText(stdout, Indent);
    Wr.PutText(stdout, Fmt.Pad("Per (us)", ColWidth));
    FOR i := 1 TO Cols DO
      IF i = IterUpCol OR i = IterDnCol
        THEN opCnt := szLong
        ELSE opCnt := neLong
      END;
      VAR per := 1.0d6 * totals[i] / (nrLong * opCnt); BEGIN
        Wr.PutText(stdout, Fmt.Pad(Fmt.Int(ROUND(per)), ColWidth))
      END
    END;
    Wr.PutChar(stdout, '\n');
    Wr.Flush(stdout)
  END PrintTotals;
    
PROCEDURE TestSearch(tbl: T) =
  VAR v: INTEGER; start := Time.Now(); BEGIN
    FOR k := 1 TO NumEntries DO
      EVAL tbl.get(k, (*OUT*) v);
    END;
    Finish(start)
  END TestSearch;

PROCEDURE TestIter(tbl: T) =
  VAR it: Iter; k, v: INTEGER; start: Time.T; BEGIN
    TYPECASE tbl OF
    | SortT (stbl) => it := stbl.iterateOrdered(up := TRUE)
    | T (tbl) => it := tbl.iterate()
    END;
    start := Time.Now();
    WHILE it.next((*OUT*) k, (*OUT*) v) DO (*SKIP*) END;
    Finish(start);
    TYPECASE tbl OF
    | SortT (stbl) =>
    	it := stbl.iterateOrdered(up := FALSE);
    	start := Time.Now();
    	WHILE it.next((*OUT*) k, (*OUT*) v) DO (*SKIP*) END;
    	Finish(start)
    | T => Finish(Time.Now())
    END
  END TestIter;

PROCEDURE TestSeek(tbl: T) =
  VAR it: SortIter; start: Time.T; BEGIN
    TYPECASE tbl OF
    | SortT (stbl) =>
        it := stbl.iterateOrdered(up := TRUE);
    	start := Time.Now();
        FOR i := 1 TO NumEntries DO
          it.seek((i * 23) MOD NumEntries)
        END;
    	Finish(start);
        it := stbl.iterateOrdered(up := FALSE);
    	start := Time.Now();
        FOR i := NumEntries TO 1 BY -1 DO
          it.seek((i * 23) MOD NumEntries)
        END;
    	Finish(start)
    | T =>
        (* unsupported, so simply report zeros for both *)
        Finish(Time.Now());
        Finish(Time.Now())
    END;
  END TestSeek;

PROCEDURE TestRandom(tbl: T) =
  VAR start: Time.T; sz: CARDINAL; BEGIN
    NewTest("Random");
    FOR r := 1 TO NumRounds DO
      EraseTable(tbl);
      InitRandom();
      StartRound(r);
      start := Time.Now();
      FOR i := 1 TO NumEntries DO
    	VAR k := rnd.integer(1, NumEntries); BEGIN
    	  EVAL tbl.put(k, k)
    	END
      END;
      Finish(start);
      sz := tbl.size();
      TestSearch(tbl);
      TestIter(tbl);
      TestSeek(tbl);
      InitRandom();
      start := Time.Now();
      FOR i := 1 TO NumEntries DO
    	VAR k := rnd.integer(1, NumEntries); v: INTEGER; BEGIN
    	  EVAL tbl.delete(k, (*OUT*) v);
    	END
      END;
      Finish(start);
      <* ASSERT tbl.size() = 0 *>
      FinishRound()
    END;
    PrintTotals(sz)
  END TestRandom;

PROCEDURE TestIncreasing(tbl: T) =
  VAR start: Time.T; BEGIN
    NewTest("Increasing");
    FOR r := 1 TO NumRounds DO
      EraseTable(tbl);
      StartRound(r);
      start := Time.Now();
      FOR k := 1 TO NumEntries DO
    	EVAL tbl.put(k, k)
      END;
      Finish(start);
      <* ASSERT tbl.size() = NumEntries *>
      TestSearch(tbl);
      TestIter(tbl);
      TestSeek(tbl);
      start := Time.Now();
      FOR k := 1 TO NumEntries DO
    	VAR v: INTEGER; BEGIN
          EVAL tbl.delete(k, (*OUT*) v);
    	END
      END;
      Finish(start);
      <* ASSERT tbl.size() = 0 *>
      FinishRound()
    END;
    PrintTotals(NumEntries)
  END TestIncreasing;

PROCEDURE TestDecreasing(tbl: T) =
  VAR start: Time.T; BEGIN
    NewTest("Decreasing");
    FOR r := 1 TO NumRounds DO
      EraseTable(tbl);
      StartRound(r);
      start := Time.Now();
      FOR k := NumEntries TO 1 BY -1 DO
    	EVAL tbl.put(k, k)
      END;
      Finish(start);
      <* ASSERT tbl.size() = NumEntries *>
      TestSearch(tbl);
      TestIter(tbl);
      TestSeek(tbl);
      start := Time.Now();
      FOR k := NumEntries TO 1 BY -1 DO
    	VAR v: INTEGER; BEGIN
          EVAL tbl.delete(k, (*OUT*) v)
    	END
      END;
      Finish(start);
      <* ASSERT tbl.size() = 0 *>
      FinishRound()
    END;
    PrintTotals(NumEntries)
  END TestDecreasing;

PROCEDURE TestTbl(tbl: T; name: TEXT) =
  BEGIN
    Wr.PutText(stdout, "*** " & name & " ***\n");
    TestRandom(tbl);
    TestIncreasing(tbl);
    TestDecreasing(tbl);
    Wr.PutText(stdout, "\n");
  END TestTbl;

BEGIN
  TRY
    IF Params.Count > 1 THEN
      (* parse "numEntries" argument *)
      NumEntries := Scan.Int(Params.Get(1))
    END;
    IF Params.Count > 2 THEN
      (* parse "numRounds" argument *)
      NumRounds := Scan.Int(Params.Get(2))
    END;
    Wr.PutText(stdout, "NumEntries = " & Fmt.Int(NumEntries) & "\n");
    Wr.PutText(stdout, "NumRounds  = " & Fmt.Int(NumRounds) & "\n\n");
    WriteHeader();
    TestTbl(NEW(IntIntTbl.Default),       "Table.Default");
    TestTbl(NEW(SortedIntIntTbl.Default), "SortedTable.Default");
    TestTbl(NEW(RedBlackIntIntTbl.T), 	  "RedBlackTbl.T");
    TestTbl(NEW(SkipListIntIntTbl.T), 	  "SkipListTbl.T");
  EXCEPT
    Lex.Error, FloatMode.Trap =>
      Wr.PutText(stderr, "Error: optional argument must be an integer\n")
  END
END IntPerfTest.
