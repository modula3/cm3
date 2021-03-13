(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Wed Aug 18 17:34:19 PDT 1993 by kalsow  *)
(*      modified on Tue Dec 17  9:05:34 PST 1991 by mcjones *)
(*      modified on Mon Aug  6 20:17:50 PDT 1990 by ellis   *)
(*      modified on Tue May  1 23:11:02 1990 by saxe        *)

UNSAFE MODULE Main;
IMPORT Test, Word;

(* Bit operations *)

TYPE
  Bit = [0 .. 31];
  BitSet = BITS BITSIZE(INTEGER) FOR SET OF Bit;
  (* BitVector = BITS 32 FOR ARRAY Bit OF BITS 1 FOR BOOLEAN;*)

VAR
  i, j, k: INTEGER;
  ar: ARRAY [0 .. 58] OF INTEGER;
  last_check: INTEGER;

PROCEDURE Check (n: INTEGER;  b: BOOLEAN) =
  BEGIN
    last_check := n;
    IF NOT b THEN Test.msgI (n); END;
    Test.check (b);
  END Check;

PROCEDURE CheckI (n: INTEGER;  a, b: INTEGER) =
  BEGIN
    last_check := n;
    IF (a # b) THEN Test.msgI (n); END;
    Test.checkI (a, b);
  END CheckI;

PROCEDURE CheckB (n: INTEGER;  a, b: BOOLEAN) =
  BEGIN
    last_check := n;
    IF (a # b) THEN Test.msgI (n); END;
    Test.checkB (a, b);
  END CheckB;

BEGIN
  FOR ind1 := 0 TO 58 DO
    ar[ind1] := ROUND (FLOAT(16_7fffffff(*LAST(INTEGER)*), LONGREAL)
                         * (FLOAT(ind1, LONGREAL) / 58.0d0));
  END;

  FOR ind1 := 0 TO 56 DO
    j := ar[ind1 + 2] - ar[ind1 + 1];
    k := ar[ind1 + 1] - ar[ind1];
    Check (1, j <= k + 1);
    Check (2, k <= j + 1);
  END;

  FOR ind1 := 0 TO 58 DO
    i := ar[ind1];
    CheckI (3, i + Word.Not(i), Word.Not (0));
    CheckI (4, Word.Shift(i,200), 0);
    CheckI (5, Word.Rotate(i,100), Word.Rotate(i,100 MOD BITSIZE (INTEGER)));
    CheckI (6, Word.Rotate(i,100), Word.Rotate(i,-100*(BITSIZE (INTEGER)-1)));

    FOR bit := 0 TO 30 DO
      CheckI (8, Word.Shift(i,-bit), i DIV Word.Shift(1,bit));
    END;

    FOR bit := 0 TO 31 DO
      CheckI (9, i, 
        Word.Rotate(Word.Shift(i,-bit)
                    +Word.Shift(i,BITSIZE(INTEGER)-bit),bit));
      CheckI (10, Word.Rotate(i,-27*bit),
                  Word.Rotate(i,(BITSIZE(INTEGER)-27)*bit));
    END;

    FOR ind2 := 0 TO 58 DO
      j := ar[ind2];

      k := Word.And (i, j);
      Check (11, k <= i);
      Check (12, k <= j);
      Check (13, LOOPHOLE(k,BitSet) = LOOPHOLE(j,BitSet)*LOOPHOLE(i,BitSet));
      FOR bit := 0 TO 31 DO
        CheckB (14,
          (bit IN LOOPHOLE(k, BitSet)),
         ((bit IN LOOPHOLE(i, BitSet)) AND (bit IN LOOPHOLE(j, BitSet))));
        (************
        CheckB (15,
          LOOPHOLE(k, BitVector)[bit],
         (LOOPHOLE(j, BitVector)[bit] AND LOOPHOLE(i, BitVector)[bit]));
        ************)
      END;

      k := Word.Or (i, j);
      Check (16, k >= i);
      Check (17, k >= j);
      Check (18, LOOPHOLE(k,BitSet) = LOOPHOLE(j,BitSet)+LOOPHOLE(i,BitSet));
      FOR bit := 0 TO 31 DO
        CheckB (19,
          (bit IN LOOPHOLE(k, BitSet)),
         ((bit IN LOOPHOLE(i, BitSet)) OR (bit IN LOOPHOLE(j, BitSet))));
        (************
        CheckB (20,
          LOOPHOLE(k, BitVector)[bit],
         (LOOPHOLE(j, BitVector)[bit] OR LOOPHOLE(i, BitVector)[bit]));
        ************)
      END;

      k := Word.Xor (i, j);
      CheckI (21, k, Word.Or (i,j) - Word.And (i,j));
      Check (22, LOOPHOLE(k,BitSet) = LOOPHOLE(j,BitSet)/LOOPHOLE(i,BitSet));
      FOR bit := 0 TO 31 DO
        CheckB (23,
          (bit IN LOOPHOLE(k, BitSet)),
         ((bit IN LOOPHOLE(i, BitSet)) # (bit IN LOOPHOLE(j, BitSet))));
        (************
        CheckB (24,
          LOOPHOLE(k, BitVector)[bit],
         (LOOPHOLE(j, BitVector)[bit] # LOOPHOLE(i, BitVector)[bit]));
        ************)
      END;
    END;
  END;

  Test.done ();
END Main.
