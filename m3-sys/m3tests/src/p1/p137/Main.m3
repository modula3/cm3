(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Mon Oct 26 11:17:11 PST 1992 by kalsow *)
(*      modified on Mon Aug  6 20:18:02 PDT 1990 by ellis  *)
(*      modified on Tue May  1 23:06:15 1990 by saxe       *)

MODULE Main;
IMPORT Test, Word, Long;
FROM Ctypes IMPORT int;

(* BITINSERT and BITEXTRACT *)

VAR i, j, k, l, m: INTEGER;
VAR I, J, K, L, M: LONGINT;
CONST I32 = (BITSIZE(INTEGER) = 32);
CONST I64 = (BITSIZE(INTEGER) = 64);
CONST L64 = (BITSIZE(LONGINT) = 64);

BEGIN
  <* ASSERT L64 *> (* longint is 64 bits *)
  <* ASSERT I32 # I64 *> (* integer is 32bits xor 64bits *)

  j := 16_7FFFFFFF;
  J := 16_7FFFFFFFFFFFFFFFL;
  IF I32 THEN
    Test.checkI (j, LAST(INTEGER));
  ELSE
    Test.check (j < LAST(INTEGER));
  END;
  Test.checkI (j, LAST(int));

  Test.checkN(J, LAST(LONGINT));
  Test.checkN(16_8000000000000000L, FIRST(LONGINT));

  i := ((-(8_20000000000 - 1)) - 1);
  I := -8_20000000000L;
  IF I32 THEN
    Test.checkI (i, FIRST(INTEGER));
  ELSE
    Test.check (i > FIRST(INTEGER));
  END;
  Test.checkI (j, LAST(int));

  IF I32 THEN
    Test.checkI (8_17777777777, LAST(INTEGER));
  END;
  Test.checkI (8_17777777777, LAST(int));

  Test.check  (ABS(FIRST(INTEGER) + LAST(INTEGER)) < 2);

  k := 8_12740756571;
  K := 8_12740756571L;
  Test.check (k > i);
  Test.check (K > I);
  Test.check (k <= j);
  Test.check (K <= J);

  Test.checkI (Word.Extract (k, 0, 0), 0);
  Test.checkI (Word.Extract (k, 0, 2), 1);
  Test.checkI (Word.Extract (k, 0, 3), 1);
  Test.checkI (Word.Extract (k, 0, 4), 9);
  Test.checkI (Word.Extract (k, 0, 5), 25);
  Test.checkI (Word.Extract (k, 3, 15), 8_75657);
  Test.checkI (Word.Extract (k, 13, 8), 30);
  Test.checkI (Word.Extract (k, 28, 3), 5);

  Test.checkN (Long.Extract (K, 0, 0), 0L);
  Test.checkN (Long.Extract (K, 0, 2), 1L);
  Test.checkN (Long.Extract (K, 0, 3), 1L);
  Test.checkN (Long.Extract (K, 0, 4), 9L);
  Test.checkN (Long.Extract (K, 0, 5), 25L);
  Test.checkN (Long.Extract (K, 3, 15), 8_75657L);
  Test.checkN (Long.Extract (K, 13, 8), 30L);
  Test.checkN (Long.Extract (K, 28, 3), 5L);


  FOR n := 0 TO 32 DO
    FOR o := 0 TO 32 - n DO
      m := Word.Extract (k, o, n);
      IF n < 32 THEN
        Test.checkI (m + Word.Extract (Word.Not (k), o, n),
                     Word.Extract (j, 0, n));
      ELSE
        Test.checkI (m, k);
      END;
    END;
  END;

  FOR n := 0 TO 64 DO
    FOR o := 0 TO 64 - n DO
      M := Long.Extract (K, o, n);
      IF n < 64 THEN
        Test.checkN (M + Long.Extract (Long.Not (K), o, n),
                     Long.Extract (J, 0, n));
      ELSE
        Test.checkN (M, K);
      END;
    END;
  END;

  l := 0;
  l := Word.Insert (l, Word.Extract (k, 0, 4), 0, 3);
  l := Word.Insert (l, Word.Extract (k, 3, 7), 3, 2);
  l := Word.Insert (l, Word.Extract (k, 5, 5), 5, 5);
  l := Word.Insert (l, Word.Extract (k, 10, 0), 10, 0);
  l := Word.Insert (l, Word.Extract (k, 10, 11), 10, 11);
  l := Word.Insert (l, Word.Extract (k, 10, 11), 10, 11);
  l := Word.Insert (l, Word.Extract (k, 21, 10), 21, 10);
  Test.checkI (l, k);

  L := 0L;
  L := Long.Insert (L, Long.Extract (K, 0, 4), 0, 3);
  L := Long.Insert (L, Long.Extract (K, 3, 7), 3, 2);
  L := Long.Insert (L, Long.Extract (K, 5, 5), 5, 5);
  L := Long.Insert (L, Long.Extract (K, 10, 0), 10, 0);
  L := Long.Insert (L, Long.Extract (K, 10, 11), 10, 11);
  L := Long.Insert (L, Long.Extract (K, 10, 11), 10, 11);
  L := Long.Insert (L, Long.Extract (K, 21, 10), 21, 10);
  Test.checkN (L, K);

  Test.done ();
END Main.
