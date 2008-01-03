(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Tue Oct 27 13:38:11 PST 1992 by kalsow *)
(*      modified on Wed Mar 28 15:38:53 1990 by saxe       *)

MODULE Main;
IMPORT Test, Foo;

BEGIN
  Foo.Assign(Foo.intA, 30);
  Test.checkI (Foo.intA, 30);
  Foo.Assign(Foo.intB, 20);
  Test.checkI (Foo.intB, 20);
  Foo.Assign(Foo.intC, Foo.Increment(Foo.intB));
  Test.checkI (Foo.intC, 21);
  Test.checkI (Foo.intB, 21);
  Foo.Assign(Foo.intC,
    Foo.Plus(Foo.intA, Foo.intB));
  Foo.Assign(Foo.intC,
    Foo.Plus(Foo.intC, Foo.intC));
  Test.checkI (Foo.intC, Foo.Plus(100, 2));
  Test.checkI (Foo.Increment(Foo.intB), 22);
  Foo.Assign(Foo.intB,
    Foo.Plus(Foo.Increment(Foo.intB),
      Foo.Plus(Foo.Increment(Foo.intB),
        Foo.intA)));
  (* for any legal order of evaluation *)
  Test.checkI (Foo.intB, 77);
  Test.done ();
END Main.
