(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Mon Oct 26 09:03:14 PST 1992 by kalsow *)
(*      modified on Tue Sep 25 14:49:26 1990 by saxe       *)
(*      modified on Mon Aug  6 19:55:11 PDT 1990 by ellis  *)

MODULE Main;

IMPORT Test;

(* Simple arithmetic expressions. *)

VAR
  intA, intB, intC: INTEGER;
  realA, realB, realC: REAL;
  longA, longB, longC: LONGREAL;
  extA, extB, extC: EXTENDED;
  cardA, cardB, cardC: CARDINAL;

PROCEDURE DoIt () =
  BEGIN
    Test.checkI (FIRST (CARDINAL) ,  0);
    Test.checkI (LAST (CARDINAL) ,  LAST (INTEGER));
    Test.checkB (FIRST (INTEGER) < LAST (INTEGER), TRUE);
    Test.checkB (FIRST (CARDINAL) < LAST (CARDINAL), TRUE);

    Test.checkI (MIN (FIRST (INTEGER), LAST (INTEGER)) ,  FIRST (INTEGER));
    Test.checkI (MIN (LAST (INTEGER), FIRST (INTEGER)) ,  FIRST (INTEGER));
    Test.checkI (MAX (FIRST (INTEGER), LAST (INTEGER)) ,  LAST (INTEGER));
    Test.checkI (MAX (LAST (INTEGER), FIRST (INTEGER)) ,  LAST (INTEGER));

    Test.checkR (MIN (FIRST (REAL), LAST (REAL)) ,  FIRST (REAL));
    Test.checkR (MIN (LAST (REAL), FIRST (REAL)) ,  FIRST (REAL));
    Test.checkR (MAX (FIRST (REAL), LAST (REAL)) ,  LAST (REAL));
    Test.checkR (MAX (LAST (REAL), FIRST (REAL)) ,  LAST (REAL));
    Test.checkL (MIN (FIRST (LONGREAL), LAST (LONGREAL)) ,  FIRST (LONGREAL));
    Test.checkL (MIN (LAST (LONGREAL), FIRST (LONGREAL)) ,  FIRST (LONGREAL));
    Test.checkL (MAX (FIRST (LONGREAL), LAST (LONGREAL)) ,  LAST (LONGREAL));
    Test.checkL (MAX (LAST (LONGREAL), FIRST (LONGREAL)) ,  LAST (LONGREAL));
    Test.checkX (MIN (FIRST (EXTENDED), LAST (EXTENDED)) ,  FIRST (EXTENDED));
    Test.checkX (MIN (LAST (EXTENDED), FIRST (EXTENDED)) ,  FIRST (EXTENDED));
    Test.checkX (MAX (FIRST (EXTENDED), LAST (EXTENDED)) ,  LAST (EXTENDED));
    Test.checkX (MAX (LAST (EXTENDED), FIRST (EXTENDED)) ,  LAST (EXTENDED));
    Test.checkI (MIN (FIRST (CARDINAL), LAST (CARDINAL)) ,  FIRST (CARDINAL));
    Test.checkI (MIN (LAST (CARDINAL), FIRST (CARDINAL)) ,  FIRST (CARDINAL));
    Test.checkI (MAX (FIRST (CARDINAL), LAST (CARDINAL)) ,  LAST (CARDINAL));
    Test.checkI (MAX (LAST (CARDINAL), FIRST (CARDINAL)) ,  LAST (CARDINAL));

    intA := 9097;
    Test.checkI (23 + 37 ,  60);
    Test.checkI (23 - 37 ,  -14);
    Test.checkI (- (-14) ,  14);
    Test.checkI (25 * -20 ,  -500);
    Test.checkI (9097 DIV 100 ,  90);
    Test.checkI (9097 MOD 100 ,  97);
    Test.checkI (intA DIV 100 ,  90);
    Test.checkI (intA MOD 100 ,  97);
    Test.checkI (9097 DIV -100 ,  -91);
    Test.checkI (9097 MOD -100 ,  -3);
    Test.checkI (intA DIV -100 ,  -91);
    Test.checkI (intA MOD -100 ,  -3);
    Test.checkI (-9097 DIV 100 ,  -91);
    Test.checkI (-9097 MOD 100 ,  3);
    Test.checkI (-intA DIV 100 ,  -91);
    Test.checkI (-intA MOD 100 ,  3);
    Test.checkI ((-9097) DIV -100 ,  90);
    Test.checkI ((-9097) MOD -100 ,  -97);
    Test.checkI ((-intA) DIV -100 ,  90);
    Test.checkI ((-intA) MOD -100 ,  -97);
    Test.checkI (ABS (-321) + ABS (321) ,  642);
    Test.checkI (TRUNC (FLOAT (4321)) ,  4321);
    Test.checkI (ROUND (FLOAT (-6789)) ,  -6789);

    intA := 7;
    intB := 17;
    intC := (intA + intA * intB) DIV 3 + 33 MOD (intA - intB);
    Test.checkI (intC ,  35);

    realA := 45.73;
    realB := -29.441;
    realC := (realB - realA) * (realA + realB);
    Test.checkB (realC < -1224.46, TRUE);
    Test.checkB (-1224.461 < realC, TRUE);
    Test.checkI (FLOOR (realC / 0.29) ,  -4223);
    Test.checkR (FLOAT (FLOAT (realC, LONGREAL)) ,  realC);

    longA := 45.7366D2;
    longB := -29.441D-5;
    longC := (longB - longA) * (longA + longB);
    Test.checkB (ABS (longC) < 20918365.7956d0, TRUE);
    Test.checkB (ABS (longC) > 0.0020918365795599d10, TRUE);

    extA := 45.7366x2;
    extB := -29.441x-5;
    extC := (extB - extA) * (extA + extB);
    Test.checkB (ABS (extC) < 20918365.7956x0, TRUE);
    Test.checkB (ABS (extC) > 0.0020918365795599x10, TRUE);

    cardA := 26;
    cardB := 38;
    cardC := (cardA + cardA * cardB DIV 3) + 33 MOD (cardB - cardA);
    Test.checkI (cardC ,  364);
  END DoIt;

BEGIN
  DoIt ();
  Test.done ();
END Main.
