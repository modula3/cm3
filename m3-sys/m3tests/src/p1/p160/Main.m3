(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Tue Oct 27 15:44:13 PST 1992 by kalsow *)
(*      modified on Thu Oct 18 16:38:47 1990 by saxe       *)

(* Very big sets on the heap, passed as VAR parameters. *)

MODULE Main;
IMPORT Test;

CONST
  Big = 10000;

TYPE
  Element = [1 .. Big];
  Set = SET OF Element;
  RefSet = REF Set;

PROCEDURE Foo() =
  VAR t := NEW (RefSet);  u := NEW (RefSet);
  BEGIN
    t^ := Set{1 .. Big};
    u^ := Set{};
    Test.check (t^ = Set{1 .. Big});
    Test.check (u^ = Set{});

    u^ := Set{1 .. Big};
    t^ := Set{};
    Test.check (u^ = Set{1 .. Big});
    Test.check (t^ = Set{});
  END Foo;

BEGIN
  Foo();
  Test.done ();
END Main.


