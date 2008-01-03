(* Created on Wed Nov 26 06:02:46 PST 1997 by heydon       *)
(* Last modified on Wed Nov 26 06:20:20 PST 1997 by heydon *)
(* Copyright (C) 1997, Digital Equipment Corporation       *)

MODULE LevelTest EXPORTS Main;

(* This program tests the implementation of the randomized procedure
   for selecting a node's level in the SkipListTbl implementation. *)

IMPORT Fmt, Random, Thread, Word, Wr;
FROM Stdio IMPORT stdout;
<* FATAL Wr.Failure, Thread.Alerted *>

CONST UseFixedSeed = FALSE;

TYPE
  T <: Public;
  Public = OBJECT
    maxLevel: CARDINAL;
  METHODS
    init(maxSizeHint: CARDINAL := 10000; useFixedSeed := FALSE): T;
    randLevel(): CARDINAL;
  END;

REVEAL
  T = Public BRANDED OBJECT
    rand: Random.T := NIL; (* random number generator *)
    randBits: INTEGER;     (* cache of random bits *)
    bitsRem: CARDINAL;     (* number of random bits remaining in "lastRand" *)
  METHODS
    next(): INTEGER := Next;
  OVERRIDES
    init := Init;
    randLevel := RandLevel;
  END;

PROCEDURE Init(t: T; maxSizeHint: CARDINAL; useFixedSeed: BOOLEAN): T =
  BEGIN
    (* compute "t.maxLevel" as "ceiling(log_4(maxSizeHint))" *)
    t.maxLevel := 0;
    WHILE maxSizeHint > 0 DO
      maxSizeHint := Word.RightShift(maxSizeHint, 2);
      INC(t.maxLevel)
    END;
    t.maxLevel := MAX(t.maxLevel, 1);

    (* initialize random number generator *)
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

PROCEDURE RandLevel(t: T): CARDINAL =
  VAR level: CARDINAL := 1; BEGIN
    WHILE level < t.maxLevel AND t.next() = 0 DO
      INC(level);
    END;
    RETURN level
  END RandLevel;

CONST InitLevel = 1; Factor = 10;

TYPE Counts = REF ARRAY OF CARDINAL;

CONST NumColWidth = 9; PcntColWidth = 7;

PROCEDURE WriteHeader(cnts: Counts) =
  BEGIN
    Wr.PutText(stdout, Fmt.Pad("Count", NumColWidth));
    FOR i := FIRST(cnts^) TO LAST(cnts^) DO
      Wr.PutText(stdout, Fmt.Pad(Fmt.Int(i+1), PcntColWidth))
    END;
    Wr.PutChar(stdout, '\n');
    Wr.Flush(stdout)
  END WriteHeader;

PROCEDURE WriteCounts(cnt: INTEGER; cnts: Counts) =
  VAR rcnt := FLOAT(cnt); BEGIN
    Wr.PutText(stdout, Fmt.Pad(Fmt.Int(cnt), NumColWidth));
    FOR i := FIRST(cnts^) TO LAST(cnts^) DO
      VAR
        pcnt := 100.0 * FLOAT(cnts[i]) / rcnt;
        str := Fmt.Real(pcnt, Fmt.Style.Fix, prec := 2);
      BEGIN
        Wr.PutText(stdout, Fmt.Pad(str, PcntColWidth))
      END;
    END;
    Wr.PutChar(stdout, '\n');
    Wr.Flush(stdout)
  END WriteCounts;

VAR
  t := NEW(T).init(100000, UseFixedSeed);
  level := InitLevel;
  nextLevel := level * Factor;
  count: CARDINAL := 0;
  counts := NEW(Counts, t.maxLevel);
BEGIN
  FOR i := 0 TO LAST(counts^) DO counts[i] := 0 END;
  WriteHeader(counts);
  LOOP
    INC(counts[t.randLevel() - 1]);
    INC(count);
    IF count MOD level = 0 THEN
      WriteCounts(count, counts);
      IF count = nextLevel THEN
        level := nextLevel;
        nextLevel := nextLevel * Factor
      END
    END
  END
END LevelTest.
