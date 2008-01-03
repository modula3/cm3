(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Mon Oct 26 10:38:04 PST 1992 by kalsow *)
(*      modified on Mon Aug  6 20:17:31 PDT 1990 by ellis  *)
(*      modified on Thu Mar 22 13:48:43 1990 by saxe       *)

MODULE Main;
IMPORT Test;

(* More BITS FOR data types *)

TYPE
  TenBit = BITS 10 FOR [15 .. 511];


VAR
  a: ARRAY TenBit OF TenBit;

BEGIN
  FOR i := FIRST(a) TO LAST(TenBit) DO a[i] := 526 - i; END;
  FOR j := 15 TO 511 DO Test.checkI (a[526 - j], j); END;
  Test.done ();
END Main.
