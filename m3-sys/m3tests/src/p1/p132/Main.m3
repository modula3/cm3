(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Mon Oct 26 10:23:08 PST 1992 by kalsow *)
(*      modified on Mon Aug  6 20:16:55 PDT 1990 by ellis  *)
(*      modified on Mon Feb 12 14:49:58 1990 by saxe       *)

UNSAFE MODULE Main;
IMPORT Test;

(* Assignment statements on built-in types. *)

TYPE
  Short = BITS 8 FOR [0..255];
  BYTE = Short;
  WORD = INTEGER;

VAR
  intA, intB, intC: INTEGER;
  (*unsA, unsB, unsC: UNSIGNED;*)
  realA, realB, realC: REAL;
  longA, longB, longC: LONGREAL;
  byteA, byteB, byteC: BYTE;
  wordA, wordB, wordC: WORD;
  boolA, boolB, boolC: BOOLEAN;
  cardA, cardB, cardC: CARDINAL;
  charA, charB, charC: CHAR;
  
BEGIN

intA := 1234567;
intB := -7654321;
intC := intA;
intA := intB;
intB := intC;
Test.checkI (intA, -7654321);
Test.checkI (intB, 1234567);
Test.checkI (intC, intB);
Test.check (intC # intA);

(*********
unsA := LAST(UNSIGNED);
unsB := 87654321;
unsC := unsA;
unsA := unsB;
unsB := unsC;
Test.checkI (unsA, 87654321);
Test.checkI (unsB, 4294967295); (* 2^32 -1 *)
Test.checkI (unsC, unsB);
Test.check (unsC # unsA);
************)

realA := 123.4567;
realB := -7654.321;
realC := realA;
realA := realB;
realB := realC;
Test.checkR (realA, -76543.21E-1);
Test.checkR (realB, 1.234567e2);
Test.checkR (realC, realB);
Test.check (realC # realA);

longA := -1234567.0d0;
longB := 76543.21d20;
longC := longA;
longA := longB;
longB := longC;
Test.checkL (longA, 7654321000.0d15);
Test.checkL (longB, -1234567.0d-0);
Test.checkL (longC, longB);
Test.check (longC # longA);

byteA := LOOPHOLE('A', BYTE);
byteB := LOOPHOLE('b', BYTE);
byteC := byteA;
byteA := byteB;
byteB := byteC;
Test.checkC (LOOPHOLE(byteA, CHAR), 'b');
Test.checkC (LOOPHOLE(byteB, CHAR), 'A');
Test.checkI (LOOPHOLE(byteC, Short), LOOPHOLE(byteB, Short));
Test.check (LOOPHOLE(byteC, Short) # LOOPHOLE(byteA, Short));

wordA := LOOPHOLE(12345678, WORD);
wordB := LOOPHOLE(87654321, WORD);
wordC := wordA;
wordA := wordB;
wordB := wordC;
Test.checkI (LOOPHOLE(wordA, CARDINAL), 87654321);
Test.checkI (LOOPHOLE(wordB, INTEGER), 12345678);
Test.checkI (LOOPHOLE(wordC, INTEGER), LOOPHOLE(wordB, INTEGER));
Test.check (LOOPHOLE(wordC, INTEGER) # LOOPHOLE(wordA, INTEGER));

boolA := TRUE;
boolB := FALSE;
boolC := boolA;
boolA := boolB;
boolB := boolC;
Test.checkB (NOT (boolA), TRUE);
Test.checkB (boolB, TRUE);
Test.checkB (boolC, boolB);
Test.check (boolC # boolA);

cardA := 234567;
cardB := 765432;
cardC := cardA;
cardA := cardB;
cardB := cardC;
Test.checkI (cardA, 765432);
Test.checkI (cardB, 234567);
Test.checkI (cardC, cardB);
Test.check (cardC # cardA);

charA := 'p';
charB := 'P';
charC := charA;
charA := charB;
charB := charC;
Test.checkC (charA, 'P');
Test.checkC (charB, 'p');
Test.checkC (charC, charB);
Test.check (charC # charA);

Test.done ();

END Main.
