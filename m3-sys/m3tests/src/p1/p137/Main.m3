(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Mon Oct 26 11:17:11 PST 1992 by kalsow *)
(*      modified on Mon Aug  6 20:18:02 PDT 1990 by ellis  *)
(*      modified on Tue May  1 23:06:15 1990 by saxe       *)

MODULE Main;
IMPORT Test, Word;

(* BITINSERT and BITEXTRACT *)

VAR i, j, k, l, m: INTEGER;

BEGIN
  j := 16_7FFFFFFF;
  Test.checkI (j, LAST(INTEGER));

  i := -8_20000000000;
  Test.checkI (i, FIRST(INTEGER));

  Test.checkI (8_17777777777, LAST(INTEGER));

  Test.check  (ABS(FIRST(INTEGER) + LAST(INTEGER)) < 2);

  k := 8_12740756571;
  Test.check (k > i);
  Test.check (k <= j);

  Test.checkI (Word.Extract (k, 0, 0), 0);
  Test.checkI (Word.Extract (k, 0, 2), 1);
  Test.checkI (Word.Extract (k, 0, 3), 1);
  Test.checkI (Word.Extract (k, 0, 4), 9);
  Test.checkI (Word.Extract (k, 0, 5), 25);
  Test.checkI (Word.Extract (k, 3, 15), 8_75657);
  Test.checkI (Word.Extract (k, 13, 8), 30);
  Test.checkI (Word.Extract (k, 28, 3), 5);

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

  l := 0;
  l := Word.Insert (l, Word.Extract (k, 0, 4), 0, 3);
  l := Word.Insert (l, Word.Extract (k, 3, 7), 3, 2);
  l := Word.Insert (l, Word.Extract (k, 5, 5), 5, 5);
  l := Word.Insert (l, Word.Extract (k, 10, 0), 10, 0);
  l := Word.Insert (l, Word.Extract (k, 10, 11), 10, 11);
  l := Word.Insert (l, Word.Extract (k, 10, 11), 10, 11);
  l := Word.Insert (l, Word.Extract (k, 21, 10), 21, 10);
  Test.checkI (l, k);

  Test.done ();
END Main.
