(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE M3SetVal;

IMPORT Word;

REVEAL T = BRANDED "M3SetVal.T" REF ARRAY OF INTEGER;

PROCEDURE NewEmpty (n_elts: INTEGER): T =
  VAR n_words := (n_elts + BITSIZE (INTEGER) - 1) DIV BITSIZE (INTEGER);
  BEGIN
    IF (n_elts < 0) THEN RETURN NIL; END;
    RETURN NEW (T, n_words);
  END NewEmpty;

PROCEDURE Compare (a, b: T): INTEGER =
  VAR le, eq, ge := TRUE;  ax, bx: INTEGER;
  BEGIN
    IF (a = NIL) OR (b = NIL) THEN RETURN -99; END;
    IF NUMBER (a^) # NUMBER (b^) THEN RETURN -99; END;
    FOR i := 0 TO LAST (a^) DO
      ax := a[i];  bx := b[i];
      IF (ax # bx)             THEN eq := FALSE; END;
      IF Word.Or (ax, bx) # bx THEN le := FALSE; END;
      IF Word.Or (ax, bx) # ax THEN ge := FALSE; END;
    END;
    IF    (le AND NOT eq) THEN RETURN -1
    ELSIF (ge AND NOT eq) THEN RETURN +1
    ELSIF (eq)            THEN RETURN  0
    ELSE                       RETURN -99;
    END;
  END Compare;

PROCEDURE Union (a, b: T): T =
  VAR c: T;
  BEGIN
    IF (a = NIL) OR (b = NIL) THEN RETURN NIL; END;
    IF NUMBER (a^) # NUMBER (b^) THEN RETURN NIL; END;
    c := NEW (T, NUMBER (a^));
    FOR i := 0 TO LAST (a^) DO
      c[i] := Word.Or (a[i], b[i]);
    END;
    RETURN c;
  END Union;

PROCEDURE Intersection (a, b: T): T =
  VAR c: T;
  BEGIN
    IF (a = NIL) OR (b = NIL) THEN RETURN NIL; END;
    IF NUMBER (a^) # NUMBER (b^) THEN RETURN NIL; END;
    c := NEW (T, NUMBER (a^));
    FOR i := 0 TO LAST (a^) DO
      c[i] := Word.And (a[i], b[i]);
    END;
    RETURN c;
  END Intersection;

PROCEDURE Difference (a, b: T): T =
  VAR c: T;
  BEGIN
    IF (a = NIL) OR (b = NIL) THEN RETURN NIL; END;
    IF NUMBER (a^) # NUMBER (b^) THEN RETURN NIL; END;
    c := NEW (T, NUMBER (a^));
    FOR i := 0 TO LAST (a^) DO
      c[i] := Word.And (a[i], Word.Not (b[i]));
    END;
    RETURN c;
  END Difference;

PROCEDURE SymDifference (a, b: T): T =
  VAR c: T;
  BEGIN
    IF (a = NIL) OR (b = NIL) THEN RETURN NIL; END;
    IF NUMBER (a^) # NUMBER (b^) THEN RETURN NIL; END;
    c := NEW (T, NUMBER (a^));
    FOR i := 0 TO LAST (a^) DO
      c[i] := Word.Xor (a[i], b[i]);
    END;
    RETURN c;
  END SymDifference;

PROCEDURE Include (a: T;  elt: INTEGER): T =
  VAR
    c: T;
    elt_word := elt DIV BITSIZE (INTEGER);
    elt_bit  := elt - elt_word * BITSIZE (INTEGER);
  BEGIN
    IF (a = NIL) THEN RETURN NIL; END;
    IF (elt < 0) OR (elt_word >= NUMBER (a^)) THEN RETURN NIL; END;
    c := NEW (T, NUMBER (a^));
    c^ := a^;
    c [elt_word] := Word.Or (c[elt_word], Word.LeftShift (1, elt_bit));
    RETURN c;
  END Include;

PROCEDURE Exclude (a: T;  elt: INTEGER): T =
  VAR
    c: T;
    elt_word := elt DIV BITSIZE (INTEGER);
    elt_bit  := elt - elt_word * BITSIZE (INTEGER);
  BEGIN
    IF (a = NIL) THEN RETURN NIL; END;
    IF (elt < 0) OR (elt_word >= NUMBER (a^)) THEN RETURN NIL; END;
    c := NEW (T, NUMBER (a^));
    c^ := a^;
    c [elt_word] := Word.And (c[elt_word], Word.Not (Word.LeftShift (1, elt_bit)));
    RETURN c;
  END Exclude;

PROCEDURE IsMember (a: T;  elt: INTEGER): BOOLEAN =
  VAR
    elt_word := elt DIV BITSIZE (INTEGER);
    elt_bit  := elt - elt_word * BITSIZE (INTEGER);
  BEGIN
    IF (a = NIL) THEN RETURN FALSE; END;
    IF (elt < 0) OR (elt_word >= NUMBER (a^)) THEN RETURN FALSE; END;
    RETURN Word.And (a[elt_word], Word.LeftShift (1, elt_bit)) # 0;
  END IsMember;
   
BEGIN
END M3SetVal.
