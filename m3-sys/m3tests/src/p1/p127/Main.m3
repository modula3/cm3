(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Mon Oct 26 10:00:20 PST 1992 by kalsow *)
(*      modified on Mon Aug  6 20:15:20 PDT 1990 by ellis  *)
(*      modified on Wed Apr 11 15:45:27 1990 by saxe       *)

MODULE Main;
IMPORT Test;

VAR i: INTEGER;
BEGIN
  i := TRUNC(127.3);
  Test.checkI (i, 127);
  i := ROUND(127.3);
  Test.checkI (i, 127);

  i := TRUNC(127.7);
  Test.checkI (i, 127);
  i := ROUND(127.7);
  Test.checkI (i, 128);

  i := TRUNC(-127.3);
  Test.checkI (i, -127);
  i := ROUND(-127.3);
  Test.checkI (i, -127);

  i := TRUNC(-127.7);
  Test.checkI (i, -127);
  i := ROUND(-127.7);
  Test.checkI (i, -128);

  i := CEILING(127.3); 
  Test.checkI (i, 128);	
  i := FLOOR(127.3);
  Test.checkI (i, 127);

  Test.checkI (FLOOR(123.7), 123);
  Test.checkI (FLOOR(-123.7), -124);
  Test.checkI (FLOOR(123.0), 123);
  Test.checkI (FLOOR(-123.0), -123);
  Test.checkI (FLOOR(0.0), 0);
  Test.checkI (FLOOR(0.54), 0);
  Test.checkI (FLOOR(1.0 - 1.6), -1);

 Test.checkR (100.0e6, 1.0e8);

 Test.done ();
END Main.
