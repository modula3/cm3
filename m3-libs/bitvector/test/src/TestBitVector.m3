(* Copyright (C) 1997, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Created on Sun Apr  6 22:26:16 PDT 1997 by heydon        *)
(* Last modified on Sat Nov 29 17:29:43 PST 1997 by heydon  *)

MODULE TestBitVector EXPORTS Main;

(* A test of the "BitVector" interface. *)

IMPORT Word, Fmt, IO, Random, BitVector;

VAR rand := NEW(Random.Default).init(fixed := TRUE);

PROCEDURE FailedAt(i: INTEGER; kind := "bit"): BOOLEAN =
  BEGIN
    IO.Put("  Error: failed at " & kind & " " & Fmt.Int(i) & "!\n\n");
    RETURN FALSE
  END FailedAt;

PROCEDURE SetResetTest(): BOOLEAN =
  CONST Step = 3; Num = 200; Lo = 20; Hi = Lo + (2 * Step * Num);
  VAR bv := NEW(BitVector.T); i := Lo; BEGIN
    IO.Put("*** Set/Reset Bit Test ***\n\n");
    IO.Put("Testing upward...\n");
    WHILE i <= Hi DO
      VAR set := bv.set(i); BEGIN <* ASSERT NOT set *> END;
      FOR j := 0 TO bv.size() - 1 DO
        IF bv.read(j) # (Lo <= j AND j <= i AND ((j-Lo) MOD Step) = 0) THEN
          RETURN FailedAt(i)
        END
      END;
      INC(i, Step)
    END;
    IO.Put("  Passed!\n\n");
    IO.Put("Testing downward...\n");
    DEC(i, Step);
    WHILE i >= Lo DO
      VAR set := bv.reset(i); BEGIN <* ASSERT set *> END;
      FOR j := 0 TO bv.size() - 1 DO
        IF bv.read(j) #
          ((Lo <= j AND j < i AND ((j-Lo) MOD Step) = 0) OR
           (i <= j AND (Step+j-Lo) MOD (2 * Step) = 0)) THEN
          RETURN FailedAt(i)
        END
      END;
      DEC(i, 2 * Step)
    END;
    IO.Put("  Passed!\n\n");
    RETURN TRUE
  END SetResetTest;

PROCEDURE IteratorTest(): BOOLEAN =
  CONST Lo = 190; Hi = 210;
  VAR bv := NEW(BitVector.T); i := Lo; BEGIN
    IO.Put("*** Iterator Test ***\n");
    WHILE i < Hi DO
      VAR set := bv.set(i*i); BEGIN <* ASSERT NOT set *> END;
      INC(i)
    END;
    VAR iter := NEW(BitVector.Iterator).init(bv); ix: CARDINAL; BEGIN
      i := Lo;
      WHILE iter.next((*OUT*) ix) DO
        IF ix # (i * i) THEN RETURN FailedAt(ix) END;
        INC(i)
      END
    END;
    IO.Put("  Passed!\n\n");
    RETURN TRUE
  END IteratorTest;

PROCEDURE LeastUnsetTest(): BOOLEAN =
  CONST MaxSqrt = 100;
  VAR bv := NEW(BitVector.T); i := 0; BEGIN
    IO.Put("*** LeastUnset() Test ***\n\n");
    IO.Put("Testing upward...\n");
    WHILE i < MaxSqrt * MaxSqrt DO
      IF bv.leastUnset() # i THEN RETURN FailedAt(i) END;
      INC(i)
    END;
    IO.Put("  Passed!\n\n");
    IO.Put("Testing downward by squares...\n");
    i := MaxSqrt - 1;
    WHILE i >= 0 DO
      VAR sqr := i * i; BEGIN
        VAR set := bv.reset(sqr); BEGIN <* ASSERT set *> END;
        IF bv.leastUnset() # sqr THEN RETURN FailedAt(sqr) END
      END;
      DEC(i)
    END;
    IO.Put("  Passed!\n\n");
    RETURN TRUE;
  END LeastUnsetTest;

PROCEDURE LeastUnsetExceptTest(): BOOLEAN =
  CONST Max = 1000;
  VAR bv1, bv2 := NEW(BitVector.T); BEGIN
    IO.Put("*** LeastUnsetExcept() Test ***\n\n");
    IO.Put("Test 1...\n");
    bv2.setInterval(0, Max - 1);
    IF bv2.leastUnset() # Max THEN RETURN FailedAt(Max) END;
    FOR i := 1 TO Max - 1 DO
      IF bv1.leastUnsetExcept(bv2) # (Max + i) THEN
        RETURN FailedAt(Max + i)
      END
    END;
    VAR set := bv2.reset(Max DIV 2); BEGIN <* ASSERT set *> END;
    IF bv1.leastUnsetExcept(bv2) # (Max DIV 2) THEN
      RETURN FailedAt(Max DIV 2)
    END;
    IO.Put("  Passed!\n\n");
    IO.Put("Test 2...\n");
    EVAL bv2.init();
    (* set "bv2" so its only unset bits are multiples of 7 *)
    FOR i := 0 TO Max - 1 BY 7 DO
      FOR j := 1 TO 6 DO
        VAR set := bv2.set(i+j); BEGIN <* ASSERT NOT set *> END
      END
    END;
    FOR i := 0 TO ((Max DIV 7) - 1) DO
      IF bv1.leastUnsetExcept(bv2) # (i * 7) THEN RETURN FailedAt(i * 7) END
    END;
    IO.Put("  Passed!\n\n");
    RETURN TRUE
  END LeastUnsetExceptTest;

PROCEDURE RdWrIntvlTest(): BOOLEAN =
  CONST WdSize = BITSIZE(Word.T);
  VAR bv := NEW(BitVector.T); BEGIN
    IO.Put("*** Read/Write Interval Test ***\n\n");

    (* test writing/reading within word boundaries *)
    IO.Put("Testing intra-words...\n");
    VAR mask := Word.Not(0); BEGIN
      FOR i := 0 TO WdSize - 1 DO
        bv.writeSub(i * WdSize, WdSize, mask)
      END
    END;
    VAR mask := 1; BEGIN
      FOR i := 0 TO WdSize - 1 DO
        bv.writeSub(i * WdSize, WdSize, mask);
        mask := Word.LeftShift(mask, 1)
      END
    END;
    VAR len := WdSize; BEGIN
      FOR i := 0 TO WdSize - 1 DO
        IF bv.readSub((i * WdSize) + i, len) # 1 THEN
          RETURN FailedAt(i, "word")
        END;
        DEC(len)
      END
    END;
    IO.Put("  Passed!\n\n");

    (* test writing/reading across word boundaries *)
    IO.Put("Testing inter-words...\n");
    VAR len := WdSize - 1; val := Word.Or(Word.LeftShift(1, len), 1); BEGIN
      FOR i := 0 TO (WdSize * WdSize) - 1 DO
        bv.writeSub(i * len, len, val);
        INC(val)
      END;
      val := 1;
      FOR i := 0 TO (WdSize * WdSize) - 1 DO
        IF bv.readSub(i * len, len) # val THEN
          RETURN FailedAt(i, "sub-word")
        END;
        INC(val)
      END;
      val := bv.readSub((WdSize * WdSize * WdSize) + (WdSize DIV 2), WdSize);
      IF val # 0 THEN
        IO.Put("  Error: failed to read 0 past end of bit vector!\n\n");
        RETURN FALSE
      END
    END;
    IO.Put("  Passed!\n\n");
    RETURN TRUE
  END RdWrIntvlTest;

PROCEDURE SetIntvlTest(): BOOLEAN =
  CONST WdSize = BITSIZE(Word.T);
  VAR bv := NEW(BitVector.T); BEGIN
    IO.Put("*** Set Interval Test ***\n\n");
    IO.Put("All intra-word intervals:\n");
    FOR i := 1 TO WdSize DO
      FOR j := 0 TO WdSize - i - 1 DO
        VAR lo := WdSize + j; hi := lo + i - 1; BEGIN
          EVAL bv.init();
          bv.setInterval(lo, hi);
          FOR k := 0 TO bv.size() + WdSize - 1 DO
            IF bv.read(k) # (lo <= k AND k <= hi) THEN
              IO.Put("  Error: failed on interval [" & Fmt.Int(lo) &
                ", " & Fmt.Int(hi) & "]!\n\n");
              RETURN FALSE
            END
          END
        END
      END
    END;
    IO.Put("  Passed!\n\n");
    IO.Put("Inter-word intervals:\n");
    FOR i := 0 TO 1000 - 1 DO
      VAR
        lo := rand.integer(0, 1000);
        hi := lo + rand.integer(0, 4 * WdSize);
      BEGIN
        EVAL bv.init();
        bv.setInterval(lo, hi);
        FOR k := 0 TO bv.size() - 1 DO
          IF bv.read(k) # (lo <= k AND k <= hi) THEN
            IO.Put("  Error: failed on interval [" & Fmt.Int(lo) &
              ", " & Fmt.Int(hi) & "]!\n\n");
            RETURN FALSE
          END
        END
      END
    END;
    IO.Put("  Pased!\n\n");
    RETURN TRUE
  END SetIntvlTest;

PROCEDURE CopyTest(): BOOLEAN =
  VAR
    bv0 := NEW(BitVector.T).init(sizeHint := 30);
    bv1: BitVector.T;
  BEGIN
    IO.Put("*** Copy Test ***\n");
    (* set bits with indexes that are perfect squares in [0,100) *)
    FOR i := 0 TO 10 - 1 DO
      VAR set := bv0.set(i*i); BEGIN <* ASSERT NOT set *> END
    END;

    (* copy "bv0" to "bv1" *)
    bv1 := bv0.copy();

    (* test that they are identical *)
    FOR i := 0 TO 10 - 1 DO
      VAR j := i * i; BEGIN
        IF NOT bv1.read(j) THEN
          IO.Put("  Error: bit " & Fmt.Int(j) & " was not set!\n\n");
          RETURN FALSE
        END;
        VAR set := bv1.reset(j); BEGIN <* ASSERT set *> END
      END
    END;

    (* "bv1" should now be empty *)
    VAR iter := NEW(BitVector.Iterator).init(bv1); i: CARDINAL; BEGIN
      IF iter.next((*OUT*) i) THEN
        IO.Put("  Error: bit " & Fmt.Int(i) & " should not be set!\n\n");
        RETURN FALSE
      END
    END;
    IO.Put("  Passed!\n\n");
    RETURN TRUE
  END CopyTest;

PROCEDURE CardinalityTest(): BOOLEAN =
  CONST IterCnt = 1000; Size = 5000; BitCnt = Size DIV 5;
  VAR bv1, bv2 := NEW(BitVector.T).init(Size); BEGIN
    IO.Put("*** Cardinality Test ***\n");
    FOR i := 1 TO IterCnt DO
      VAR setCnt := 0; resetCnt := 0; BEGIN
      	EVAL bv1.init(freeMem := FALSE);
      	EVAL bv2.init(freeMem := FALSE);
      	bv2.setInterval(0, Size - 1);
      	FOR j := 1 TO BitCnt DO
      	  VAR k := rand.integer(0, Size - 1); BEGIN
      	    IF NOT bv1.set(k) THEN INC(setCnt) END;
      	    IF bv2.reset(k) THEN INC(resetCnt) END;
      	  END
      	END;
      	IF bv1.cardinality() # setCnt THEN
      	  IO.Put("  Error: got " & Fmt.Int(bv1.cardinality()) &
      	    " set bits; expected " & Fmt.Int(setCnt) & "!\n\n");
      	  RETURN FALSE
      	END;
      	IF Size - bv2.cardinality() # resetCnt THEN
      	  IO.Put("  Error: got " & Fmt.Int(Size - bv2.cardinality()) &
      	    " reset bits; expected " & Fmt.Int(resetCnt) & "!\n\n");
      	  RETURN FALSE
        END
      END
    END;
    IO.Put("  Passed!\n\n");
    RETURN TRUE
  END CardinalityTest;

PROCEDURE BitwiseTest1(): BOOLEAN =
  CONST Max = 1000;
  VAR
    empty := NEW(BitVector.T);
    bv3 := NEW(BitVector.T);
    bv4 := NEW(BitVector.T).init(sizeHint := 200);
    bv5 := NEW(BitVector.T).init(sizeHint := 500);
    bv67 := NEW(BitVector.T);
    bv3xor5 := NEW(BitVector.T);
    bv, bvp: BitVector.T;
  BEGIN
    IO.Put("*** Bitwise Test 1 ***\n\n");

    (* initialize bit vectors *)
    FOR i := 0 TO Max - 1 DO
      IF i MOD 3 = 0 THEN EVAL bv3.set(i) END;
      IF i MOD 4 = 0 THEN EVAL bv4.set(i) END;
      IF i MOD 5 = 0 THEN EVAL bv5.set(i) END;
      IF i MOD 67 = 0 THEN EVAL bv67.set(i) END;
      IF (i MOD 3 = 0) # (i MOD 5 = 0) THEN EVAL bv3xor5.set(i) END
    END;

    (* tests against empty bit vectors *)
    IO.Put("Testing against empty vectors...\n");
    bv := bv3.copy();
    IF NOT (BitVector.And(empty, bv).empty() AND
            empty.and(bv).empty() AND bv.and(empty).empty()) THEN
      IO.Put("  AND operation failed!\n\n"); RETURN FALSE
    END;
    bv := empty.copy(); bvp := bv3.copy();
    IF NOT (BitVector.Equal(BitVector.Or(empty, bv3), bv3) AND
            BitVector.Equal(bv.or(bv3), bv3) AND
            BitVector.Equal(bvp.or(empty), bv3)) THEN
      IO.Put("  OR operation failed!\n\n"); RETURN FALSE
    END;
    bv := empty.copy(); bvp := bv3.copy();
    IF NOT (BitVector.Equal(BitVector.Xor(bv3, empty), bv3) AND
            BitVector.Equal(bv.xor(bv3), bv3) AND
            BitVector.Equal(bvp.xor(empty), bv3)) THEN
      IO.Put("  XOR operation failed!\n\n"); RETURN FALSE
    END;
    bv := empty.copy(); bvp := bv3.copy();
    IF NOT (BitVector.Equal(BitVector.Minus(bv3, empty), bv3) AND
            BitVector.Equal(BitVector.Minus(empty, bv3), empty) AND
            BitVector.Equal(bv.minus(bv3), empty) AND
            BitVector.Equal(bvp.minus(empty), bv3)) THEN
      IO.Put("  Minus operation failed!\n\n"); RETURN FALSE
    END;
    IO.Put("  Passed!\n\n");
    RETURN TRUE
  END BitwiseTest1;

PROCEDURE MakeBV(gap, len: INTEGER; sense: BOOLEAN): BitVector.T =
(* Return a new bit vector of length "len" in which every "gap"th bit
   is either set or unset as "sense" is TRUE or FALSE, respectively. *)
  VAR res := NEW(BitVector.T).init(sizeHint := len); BEGIN
    IF NOT sense THEN res.setInterval(0, len-1) END;
    FOR i := gap TO len - 1 BY gap DO
      VAR was := res.write(i, sense); BEGIN
        <* ASSERT was # sense *>
      END
    END;
    RETURN res;
  END MakeBV;

PROCEDURE BitwiseTest2b(bv1, bv2: BitVector.T): BOOLEAN =
(* Test all of the bit-wise operations on "bv1" and "bv2" for
   correctness, returning TRUE if and only if all tests pass. *)
  VAR
    and := BitVector.And(bv1, bv2);
    or := BitVector.Or(bv1, bv2);
    xor := BitVector.Xor(bv1, bv2);
    minus := BitVector.Minus(bv1, bv2);
    bigger := MAX(bv1.size(), bv2.size());
  BEGIN
    (* first, test non-destructive ops *)
    FOR i := 0 TO bigger - 1 DO
  	VAR b1 := bv1.read(i); b2 := bv2.read(i); BEGIN
  	  IF (and.read(i) # (b1 AND b2)) OR
  	     (or.read(i) # (b1 OR b2)) OR
  	     (xor.read(i) # (b1 # b2)) OR
  	     (minus.read(i) # (b1 AND NOT b2)) THEN
  	    RETURN FALSE
  	  END
  	END
    END;

    (* second, test destructive ops *)
    VAR
      andD := bv1.copy().and(bv2);
      orD := bv1.copy().or(bv2);
      xorD := bv1.copy().xor(bv2);
      minusD := bv1.copy().minus(bv2);
    BEGIN
      RETURN
        BitVector.Equal(and, andD) AND BitVector.Equal(andD, and) AND
        BitVector.Equal(or, orD) AND BitVector.Equal(orD, or) AND
        BitVector.Equal(xor, xorD) AND BitVector.Equal(xorD, xor) AND
        BitVector.Equal(minus, minusD) AND BitVector.Equal(minusD, minus)
    END
  END BitwiseTest2b;

PROCEDURE BitwiseTest2(): BOOLEAN =
  CONST Num = 30; Len1 = 300; Len2 = 500;
  TYPE T = ARRAY [0..Num-1] OF BitVector.T;
  VAR on, off: T; BEGIN
    IO.Put("*** Bitwise Test 2 ***\n");
    FOR i := 0 TO Num - 1 DO
      on[i] := MakeBV(i+1, Len1, TRUE);
      off[i] := MakeBV(i+1, Len2, FALSE)
    END;
    FOR i := 0 TO Num - 1 DO
      FOR j := 0 TO Num - 1 DO
        IF NOT BitwiseTest2b(on[i], on[j]) OR
           NOT BitwiseTest2b(on[i], off[j]) OR
           NOT BitwiseTest2b(off[i], on[j]) OR
           NOT BitwiseTest2b(off[i], off[j]) THEN
          IO.Put("  Failed at i = " & Fmt.Int(i)
                 & ", j = " & Fmt.Int(j) & "\n");
          RETURN FALSE
        END
      END
    END;
    IO.Put("  Passed!\n\n");
    RETURN TRUE
  END BitwiseTest2;

BEGIN
  IF SetResetTest() AND IteratorTest() AND LeastUnsetTest() AND
     LeastUnsetExceptTest() AND RdWrIntvlTest() AND
     SetIntvlTest() AND CopyTest() AND CardinalityTest() AND
     BitwiseTest1() AND BitwiseTest2() THEN
    IO.Put("All tests passed!\n");
  ELSE
    IO.Put("Test failed!\n");
  END
END TestBitVector.
