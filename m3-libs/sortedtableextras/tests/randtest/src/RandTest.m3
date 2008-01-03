(* Created on Sun Nov 23 04:21:14 PST 1997 by heydon       *)
(* Last modified on Sun Nov 23 04:48:30 PST 1997 by heydon *)
(* Copyright (C) 1997, Digital Equipment Corporation       *)

MODULE RandTest EXPORTS Main;

(* This program tests the implementation of the biased coin
   used in the SkipListTbl implementation. *)

IMPORT Fmt, Random, Thread, Word, Wr;
FROM Stdio IMPORT stdout;
<* FATAL Wr.Failure, Thread.Alerted *>

CONST UseFixedSeed = FALSE;

TYPE
  T <: Public;
  Public = OBJECT METHODS
    init(useFixedSeed := FALSE): T;
    next(): INTEGER;
  END;

REVEAL
  T = Public BRANDED OBJECT
    rand: Random.T := NIL; (* random number generator *)
    randBits: INTEGER;     (* cache of random bits *)
    bitsRem: CARDINAL;     (* number of random bits remaining in "lastRand" *)
  OVERRIDES
    init := Init;
    next := Next;
  END;

PROCEDURE Init(t: T; useFixedSeed: BOOLEAN): T =
  BEGIN
    IF t.rand = NIL OR useFixedSeed THEN
      t.rand := NEW(Random.Default).init(useFixedSeed)
    END;
    t.randBits := t.rand.integer();
    t.bitsRem := Word.Size;
    <* ASSERT (t.bitsRem MOD 4) = 0 *>
    RETURN t
  END Init;

PROCEDURE Next(t: T): INTEGER =
  VAR res := Word.And(t.randBits, 2_11); BEGIN
    IF t.bitsRem > 2 THEN
      t.randBits := Word.RightShift(t.randBits, 2);
      DEC(t.bitsRem, 2)
    ELSE
      t.randBits := t.rand.integer();
      t.bitsRem := Word.Size
    END;
    RETURN res;
  END Next;

CONST InitLevel = 1; Factor = 10;

TYPE Counts = ARRAY [0..3] OF CARDINAL;

CONST IntWidth = 9;

PROCEDURE WriteHeader() =
  BEGIN
    Wr.PutText(stdout, Fmt.Pad("Count", IntWidth));
    FOR i := FIRST(Counts) TO LAST(Counts) DO
      Wr.PutText(stdout, Fmt.Pad(Fmt.Int(i), IntWidth));
      Wr.PutText(stdout, " ( Net %)")
    END;
    Wr.PutChar(stdout, '\n');
    Wr.Flush(stdout)
  END WriteHeader;

PROCEDURE WriteCounts(cnt: INTEGER; cnts: Counts) =
  VAR rcnt := FLOAT(cnt); BEGIN
    Wr.PutText(stdout, Fmt.Pad(Fmt.Int(cnt), IntWidth));
    FOR i := FIRST(Counts) TO LAST(Counts) DO
      Wr.PutText(stdout, Fmt.Pad(Fmt.Int(cnts[i]), IntWidth));
      Wr.PutText(stdout, " (");
      VAR
        pcnt := 100.0 * FLOAT(cnts[i]) / rcnt;
        str := Fmt.Real(pcnt, Fmt.Style.Fix, prec := 1);
      BEGIN
        Wr.PutText(stdout, Fmt.Pad(str, 5))
      END;
      Wr.PutText(stdout, "%)");
    END;
    Wr.PutChar(stdout, '\n');
    Wr.Flush(stdout)
  END WriteCounts;

VAR
  level := InitLevel;
  nextLevel := level * Factor;
  count: CARDINAL := 0;
  counts := Counts{0,..};
  t := NEW(T).init(UseFixedSeed);
BEGIN
  WriteHeader();
  LOOP
    INC(counts[t.next()]);
    INC(count);
    IF count MOD level = 0 THEN
      WriteCounts(count, counts);
      IF count = nextLevel THEN
        level := nextLevel;
        nextLevel := nextLevel * Factor
      END
    END
  END
END RandTest.
