(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Mon Oct 26 09:08:26 PST 1992 by kalsow *)
(*      modified on Mon Aug  6 21:07:45 PDT 1990 by ellis  *)
(*      modified on Wed Apr  4 18:23:35 1990 by saxe       *)

MODULE Main;
IMPORT Test;

TYPE
  Short = [0 .. 63];

VAR
  p, q, r: Short;

BEGIN
  p := 40;
  q := 27;
  Test.checkI (p + q, 67);
  r := (p*3) MOD 64;
  Test.checkI (r, 56);
  Test.done ();
END Main.
