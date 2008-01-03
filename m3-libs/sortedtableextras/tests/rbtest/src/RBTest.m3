(* Created on Wed Nov 19 09:46:42 PST 1997 by heydon       *)
(* Last modified on Sat Nov 22 19:44:16 PST 1997 by heydon *)
(* Copyright (C) 1997, Digital Equipment Corporation       *)

MODULE RBTest EXPORTS Main;

(* This program is a simple test of the red-black sorted table
   implementation.

   SYNTAX: RBTest [ numEntries [ randTrials ] ]
*)

IMPORT RedBlackIntIntTbl;
IMPORT FloatMode, Lex, Params, Random, Scan, Thread, Wr;
FROM Stdio IMPORT stderr;

<* FATAL Wr.Failure, Thread.Alerted *>

CONST UseFixedSeed = FALSE;

VAR (* READONLY after init *)
  NumEntries := 10000;
  RandTrials := 100;
  Rand := NEW(Random.Default).init(fixed := UseFixedSeed);
  Seed: CARDINAL := Rand.integer(0, 100000);

PROCEDURE InitRandom(i: CARDINAL) =
(* Initialized the random number generator "Rand" using seed "Seed"
   offset by "i". *)
  BEGIN
    EVAL Rand.init(fixed := TRUE);
    FOR i := 1 TO Seed + (100 * i) DO EVAL Rand.integer() END
  END InitRandom;

TYPE
  T = RedBlackIntIntTbl.T;
  Iter = RedBlackIntIntTbl.Iterator;

PROCEDURE TestSearch(tbl: T) =
  VAR v: INTEGER; BEGIN
    FOR k := 1 TO NumEntries DO
      IF tbl.get(k, (*OUT*) v) THEN
        <* ASSERT v = k * k *>
      END
    END
  END TestSearch;

PROCEDURE TestIter(tbl: T) =
  VAR it: Iter; lastK, k, v: INTEGER; BEGIN
    it := tbl.iterateOrdered(up := TRUE);
    lastK := FIRST(INTEGER);
    WHILE it.next((*OUT*) k, (*OUT*) v) DO
      <* ASSERT lastK < k *>
      lastK := k
    END;
    it := tbl.iterateOrdered(up := FALSE);
    lastK := LAST(INTEGER);
    WHILE it.next((*OUT*) k, (*OUT*) v) DO
      <* ASSERT lastK > k *>
      lastK := k
    END
  END TestIter;

PROCEDURE TestRandom(tbl: T) =
  BEGIN
    FOR i := 0 TO RandTrials - 1DO
      EVAL tbl.init();
      InitRandom(i);
      FOR i := 1 TO NumEntries DO
    	VAR k := Rand.integer(1, NumEntries); BEGIN
    	  EVAL tbl.put(k, k*k)
    	END
      END;
      TestSearch(tbl);
      TestIter(tbl);
      InitRandom(i);
      FOR i := 1 TO NumEntries DO
    	VAR k := Rand.integer(1, NumEntries); v: INTEGER; BEGIN
    	  IF tbl.delete(k, (*OUT*) v) THEN
    	    <* ASSERT v = k * k *>
    	  END
    	END
      END;
      <* ASSERT tbl.size() = 0 *>
    END
  END TestRandom;

PROCEDURE TestIncreasing(tbl: T) =
  BEGIN
    EVAL tbl.init();
    FOR i := 1 TO NumEntries DO
      VAR inTbl := tbl.put(i, i*i); BEGIN
        <* ASSERT NOT inTbl *>
      END
    END;
    <* ASSERT tbl.size() = NumEntries *>
    TestSearch(tbl);
    TestIter(tbl);
    FOR i := 1 TO NumEntries DO
      VAR v: INTEGER; inTbl := tbl.delete(i, (*OUT*) v); BEGIN
        <* ASSERT inTbl AND v = i * i *>
      END
    END;
    <* ASSERT tbl.size() = 0 *>
  END TestIncreasing;

PROCEDURE TestDecreasing(tbl: T) =
  BEGIN
    EVAL tbl.init();
    FOR i := NumEntries TO 1 BY -1 DO
      VAR inTbl := tbl.put(i, i*i); BEGIN
        <* ASSERT NOT inTbl *>
      END
    END;
    <* ASSERT tbl.size() = NumEntries *>
    TestSearch(tbl);
    TestIter(tbl);
    FOR i := NumEntries TO 1 BY -1 DO
      VAR v: INTEGER; inTbl := tbl.delete(i, (*OUT*) v); BEGIN
        <* ASSERT inTbl AND v = i * i *>
      END
    END;
    <* ASSERT tbl.size() = 0 *>
  END TestDecreasing;

PROCEDURE TestSeek(tbl: T) =
  VAR keys: REF ARRAY OF INTEGER; BEGIN
    EVAL tbl.init();
    (* fill in a random, sparse table *)
    FOR i := 0 TO NumEntries DIV 10 DO
      VAR k := Rand.integer(1, NumEntries); BEGIN
        EVAL tbl.put(k, k);
      END
    END;
    (* read table keys into "keys" array in order *)
    keys := NEW(REF ARRAY OF INTEGER, tbl.size());
    VAR it := tbl.iterate(); k, v: INTEGER; i := 0; BEGIN
      WHILE it.next((*OUT*) k, (*OUT*) v) DO
        keys[i] := k; INC(i)
      END
    END;
    TestSeekUp(tbl, keys);
    TestSeekDown(tbl, keys);
  END TestSeek;

PROCEDURE TestSeekUp(tbl: T; keys: REF ARRAY OF INTEGER) =
  VAR
    it := tbl.iterateOrdered(up := TRUE);
    curr := 0; k, v: INTEGER;
  BEGIN
    FOR i := -1 TO NumEntries + 1 DO
      WHILE curr <= LAST(keys^) AND tbl.keyCompare(i, keys[curr]) > 0 DO
        INC(curr)
      END;
      it.seek(i);
      IF it.next((*OUT*) k, (*OUT*) v) THEN
        <* ASSERT k = keys[curr] *>
      ELSE
        <* ASSERT curr > LAST(keys^) *>
      END
    END
  END TestSeekUp;

PROCEDURE TestSeekDown(tbl: T; keys: REF ARRAY OF INTEGER) =
  VAR
    it := tbl.iterateOrdered(up := FALSE);
    curr := LAST(keys^); k, v: INTEGER;
  BEGIN
    FOR i := NumEntries + 1 TO -1 BY -1 DO
      WHILE curr >= FIRST(keys^) AND tbl.keyCompare(i, keys[curr]) < 0 DO
        DEC(curr)
      END;
      it.seek(i);
      IF it.next((*OUT*) k, (*OUT*) v) THEN
        <* ASSERT k = keys[curr] *>
      ELSE
        <* ASSERT curr < FIRST(keys^) *>
      END
    END
  END TestSeekDown;

PROCEDURE TestTbl(tbl: T) =
  BEGIN
    TestRandom(tbl);
    TestIncreasing(tbl);
    TestDecreasing(tbl);
    TestSeek(tbl);
  END TestTbl;

BEGIN
  TRY
    IF Params.Count > 1 THEN
      (* parse "numEntries" argument *)
      NumEntries := Scan.Int(Params.Get(1))
    END;
    IF Params.Count > 2 THEN
      (* parse "randTrials" argument *)
      RandTrials := Scan.Int(Params.Get(2))
    END;
    TestTbl(NEW(T).init())
  EXCEPT
    Lex.Error, FloatMode.Trap =>
      Wr.PutText(stderr, "Error: optional argument must be an integer\n")
  END
END RBTest.
