(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Created September 1989 by Bill Kalsow                       *)
(* Based on Random.mod by Mark R. Brown                        *)
(* Last modified on Fri Jun 17 13:53:45 PDT 1994 by kalsow     *)
(*      modified on Wed Apr 20 13:41:38 PDT 1994 by detlefs    *)
(*      modified on Mon May 31 16:26:54 PDT 1993 by swart      *)
(*      modified on Thu May  6 09:51:58 PDT 1993 by mjordan    *)
(*      modified on Tue Apr 13 11:40:18 PDT 1993 by mcjones    *)
(*      modified on Thu Jan 25 22:46:01 PST 1990 by stolfi     *)
(*      modified on Thu Nov  2 18:28:01 1989 by muller         *)

MODULE Random;

IMPORT Word, Tick, TimeStamp, RandomReal;

CONST
  DefaultSeed = 1202056903;
  (* Note: good seeds must be in [LAST(INTEGER)/10 .. 9*LAST(INTEGER)/10]. *)

REVEAL
  Default = T OBJECT METHODS
    init(fixed := FALSE): Default
  END BRANDED "RandomKnuth3_2_2A.T" OBJECT
    i: [0..56];
    a: ARRAY [1..55] OF INTEGER;
  OVERRIDES
    init     := Init;
    integer  := Integer;
    real     := Real;
    longreal := Longreal;
    extended := Extended;
    boolean  := Boolean;
  END;

PROCEDURE Init (t: Default;  fixed: BOOLEAN): Default =
  VAR seed := DefaultSeed;
  BEGIN
    IF (t = NIL) THEN t := NEW (Default) END;
    IF NOT fixed THEN seed := RandomSeed () END;
    Start (t, seed);
    RETURN t;
  END Init;
    
PROCEDURE Start (t: Default;  seed: INTEGER) =
  VAR j, k: INTEGER;  i2: [1..54];
  BEGIN
    (* For an explanation of this initialization procedure see the Fortran
       program in Section 3.6 of Knuth Volume 2, second edition. *)
    t.a[55] := seed;
    j := seed;
    k := 1;
    FOR i := 1 TO 54 DO
      i2 := (21 * i) MOD 55;
      t.a[i2] := k;
      k := Word.Minus (j, k); (* ignore underflow *)
      j := t.a[i2];
    END;
    FOR i := 1 TO 20 DO Next55 (t) END;
  END Start;

PROCEDURE RandomSeed (): INTEGER =
  VAR
    seed, i: INTEGER;
    j := TimeStamp.Hash(TimeStamp.New());
  BEGIN
    (* XOR everything we can get our hands on. The idea here is to move
       around volitile portions of these quantities so that we will get as
       many random bits as possible.  -- Garret *)

    i := Tick.Now();
    seed := DefaultSeed;
    seed := Word.Xor (seed, Word.Rotate(j, i MOD BITSIZE(INTEGER)));
    seed := Word.Xor (seed, i);

    (* normalize the seed to .1*LAST(CARD) <= seed < .9 LAST(CARD) *)
    IF seed < 0 THEN seed :=  -(seed + 1);  END;
    IF seed > 9 * (LAST (CARDINAL) DIV 10) THEN
      seed := (seed DIV (j MOD 7 + 2)) + (i MOD 23);
    END;
    WHILE seed < LAST (CARDINAL) DIV 10 DO
      seed := seed * (j MOD 7 + 2) + (i MOD 23);
    END;

    RETURN seed;
  END RandomSeed;

PROCEDURE Next55 (t: Default) =
  BEGIN
    FOR j := 55 TO 32 BY  -1 DO
      (* DEC (t.a[j], t.a[j - 31]); (* ignore underflow *) *)
      t.a[j] := Word.Minus (t.a[j], t.a[j - 31]);
    END;
    FOR j := 31 TO 1 BY  -1 DO
      (* DEC (t.a[j], t.a[j + 24]); (* ignore underflow *) *)
      t.a[j] := Word.Minus (t.a[j], t.a[j + 24]);
    END;
    t.i := 56;
  END Next55;

CONST
  halfWordSize = Word.Size DIV 2;
  halfWordRadix = Word.LeftShift(1, halfWordSize);
  halfWordMask = halfWordRadix - 1;

PROCEDURE Integer (t: Default;  min, max: INTEGER): INTEGER =
  VAR x, rem, range: INTEGER;
  BEGIN
    <*ASSERT min <= max *>
    LOOP
      LOOP
        DEC (t.i);
        IF t.i > 0 THEN EXIT END;
        Next55 (t);
      END;
      x := t.a[t.i];

      IF (0 < min) OR (min + LAST (INTEGER) > max) THEN
        (* the range less than LAST (INTEGER) *)
        range := max - min + 1;
        IF range < halfWordRadix THEN
          VAR xl := Word.And(x, halfWordMask);
              xh := Word.RightShift(x, halfWordSize);
              res: INTEGER;
          BEGIN
            res := xl * range;
            res := Word.RightShift(res, halfWordSize);
            res := res + xh * range;
            res := Word.RightShift(res, halfWordSize);
            RETURN min + res;
          END (* BEGIN *)
        ELSE
	  x := Word.And (x, LAST (INTEGER));  (* 0 <= x <= LAST (INTEGER *)
	  rem := x MOD range;                 (* 0 <= rem < range *)
	  IF x - rem <= LAST (INTEGER) - range THEN
	    (* got a full block:  x - rem + range <= LAST (INTEGER) *)
	    RETURN min + rem;
	  END
        END (* IF *)
      ELSE
        (* the range is very large, but not complete *)
        (* so, just keep trying until we find a legal value *)
        IF (min <= x) AND (x <= max) THEN RETURN x END;
      END;
    END;
  END Integer;

PROCEDURE Boolean (t: Default): BOOLEAN =
  BEGIN
    LOOP
      DEC (t.i);
      IF t.i > 0 THEN EXIT END;
      Next55 (t);
    END;
    RETURN VAL (Word.And (1, t.a[t.i]), BOOLEAN);
  END Boolean;

PROCEDURE Real (t: Default;  min, max: REAL): REAL =
  BEGIN
    <*ASSERT min <= max *>
    RETURN (max - min) * RandomReal.Real (t) + min;
  END Real;

PROCEDURE Longreal (t: Default;  min, max: LONGREAL): LONGREAL =
  BEGIN
    <*ASSERT min <= max *>
    RETURN (max - min) * RandomReal.Longreal (t) + min;
  END Longreal;

PROCEDURE Extended (t: Default;  min, max: EXTENDED): EXTENDED =
  BEGIN
    <*ASSERT min <= max *>
    RETURN (max - min) * RandomReal.Extended (t) + min;
  END Extended;

BEGIN
  <*ASSERT Word.Size MOD 2 = 0 *>
END Random.

(*************************************************************
PROCEDURE Subrange (first, last: CARDINAL;  t: Default): CARDINAL =
  VAR card, card1, range: CARDINAL;
  BEGIN
      LOOP
        LOOP
          DEC (t.i);
          IF t.i > 0 THEN EXIT END;
          Next55 (t);
        END;
        card := Word.And (2147483647, t.a[t.i]);
        IF first > last THEN RETURN (card);  END;
        range := last - first;
        IF range = LAST (CARDINAL) THEN RETURN (card);  END;
        INC (range);
        card1 := card - card MOD range;
        IF card1 <= LAST (CARDINAL) - range THEN
          RETURN (first + (card - card1));
        END;
      END;
  END Subrange;
*******************************************************************)


