(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Tue Oct 27 13:35:15 PST 1992 by kalsow *)
(*      modified on Wed Dec  5 12:40:26 1990 by saxe       *)

(* Check that REF variables are initialized to NIL and that
   non-rep-complete variables are initialized to legal values *)

MODULE Main;
IMPORT Test;

TYPE
  Short = BITS 8 FOR [-30 .. -12];
  ShortArray = (*BITS 152 FOR*) ARRAY Short OF Short;
  Record = RECORD
    a: Short;
    b: REF INTEGER;
    c: Short;
    d: REF Short;
    e: ShortArray;
  END;

VAR
  ri: REF INTEGER;
  s: Short;
  rs: REF Short;
  r: Record;
  rr: REF Record;

PROCEDURE Check(VAR arg: Record)=
  VAR index: Short;
  BEGIN
    Test.check (index >= -30);
    Test.check (index <= -12);
    Test.check (arg.a >= -30);
    Test.check (arg.a <= -12);
    Test.check (arg.b = NIL);
    Test.check (arg.c >= -30);
    Test.check (arg.c <= -12);
    Test.check (arg.d = NIL);
    FOR i := FIRST(arg.e) TO LAST(arg.e) DO
      Test.check (arg.e[i] >= -30);
      Test.check (arg.e[i] <= -12);
    END;
    Test.check (index >= -30);
    Test.check (index <= -12);
  END Check;

PROCEDURE Proc() =
  VAR y: Record;  z: ARRAY [1 .. 5] OF Record;
  BEGIN
    Check(y);
    FOR i := FIRST(z) TO LAST(z) DO Check(z[i]) END;
  END Proc;

BEGIN
  Test.check (ri = NIL);
  Test.check (s >= -30);
  Test.check (s <= -12);
  Test.check (rs = NIL);
  Check(r);
  Test.check (rr = NIL);
  rr := NEW (REF Record);
  Check(rr^);
  Proc();
  Test.done ();
END Main.
