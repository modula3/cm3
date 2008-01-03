(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Tue Oct 27 15:58:34 PST 1992 by kalsow *)
(*      modified on Mon Aug  6 20:19:12 PDT 1990 by ellis  *)
(*      modified on Tue Feb 13 15:56:07 1990 by saxe       *)

(* Tests of different kinds of statements. *)

MODULE Main;
IMPORT Test;

VAR
  intA, intB, intC: INTEGER;
  charA, charB: CHAR;

BEGIN

  (* Assignment statements *)
  charA := 'B';
  Test.checkC (charA, 'B');
  intA := 1 - 2;
  Test.checkI (intA, -1);

  (* IF statements *)
  IF intA < 0 THEN intA := 20; ELSE intA := 30; END;
  Test.checkI (intA, 20);
  IF intA < 0 THEN intA := 20; ELSE intA := 30; END;
  Test.checkI (intA, 30);

  (* WHILE statements *)
  intA := 1;
  intB := 20;
  WHILE intA < intB DO intA := intA * 2; END;
  Test.checkI (intA, 32);
  WHILE intA < intB DO intA := intA * 2; END;
  Test.checkI (intA, 32);

  (* REPEAT statements *)
  intA := 1;
  intB := 20;
  REPEAT intA := intA * 2; intA := intA * 2 UNTIL intA >= intB;
  Test.checkI (intA, 64);
  REPEAT intA := intA * 2; intA := intA * 2 UNTIL intA >= intB;
  Test.checkI (intA, 256);

  (* LOOP statement *)
  intA := 0;
  LOOP
    intA := intA + 11;
    IF intA > 73 THEN EXIT; END;
    intA := intA - 1;
  END;
  Test.checkI (intA, 81);

  (* FOR statements *)
  intA := 0;
  intB := 10;
  intC := 0;
  FOR intA := intA TO intB DO intB := intB + intA; END;
  Test.checkI (intB, 65);
  FOR intA := intB TO intA DO intC := 300; END;
  Test.checkI (intC, 0);

  (* CASE statements *)
  charA := 'x';
  charB := 'y';
  CASE charA OF
  | 'A' .. 'Z' => charB := 'q'; charA := charB;
  | 'q'        => charB := 'w'; charA := charB;
  ELSE            charA := 'D';
  END;
  Test.checkC (charA, 'D');
  CASE charA OF
  | 'A' .. 'Z' => charB := 'q'; charA := charB;
  | 'q'        => charB := 'w'; charA := charB;
  ELSE            charA := 'q';
  END;
  Test.checkC (charA, 'q');
  CASE charA OF <*NOWARN*>
  | 'A' .. 'Z' => charB := 'q'; charA := charB;
  | 'q'        => charB := 'w'; charA := charB;
  END;
  Test.checkC (charA, 'w');
  CASE charA OF
  | 'A' .. 'Z' => charB := 'q'; charA := charB;
  | 'q'        => charB := 'w'; charA := charB;
  ELSE
  END;
  Test.checkC (charA, 'w');

  Test.done ();
END Main.
