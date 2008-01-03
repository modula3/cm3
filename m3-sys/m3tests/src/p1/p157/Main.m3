(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Tue Oct 27 15:00:03 PST 1992 by kalsow *)
(*      modified on Wed Oct 10 17:00:07 1990 by saxe       *)

(* Big sets on the heap, passed as VAR parameters. *)

MODULE Main;
IMPORT Test;

CONST
  Big = 100;

TYPE
  Element = [1 .. Big];
  Set = SET OF Element;
  RefSet = REF Set;

VAR
  x, y, z: RefSet;

PROCEDURE Sieve (VAR s: Set) =
  VAR j: INTEGER; t: Set;
  BEGIN
    s := Set{1..Big};
    FOR i := 1 TO Big DO
      j := i;
      WHILE j <= Big DO
        t := Set{};
        t := t + Set {j}; (* INCL(t, j); *)
        s := s / t;
        j := j + i;
      END;
    END;
  END Sieve;

PROCEDURE Squares (VAR t: Set) =
  VAR i: Element;
  BEGIN
(*    t := -Set{1 .. 100 (* Big *)}; *)
    t := Set{};
    i := 1;
    WHILE i * i <= Big DO
      t := t + Set {i * i};
      INC (i);
    END;
  END Squares;

BEGIN
  x := NEW (RefSet);
  Sieve(x^);
  Test.check (10 IN x^);
  Test.check (NOT (100 IN x^));
  y := NEW (RefSet);
  Squares(y^);
  Test.check (NOT (10 IN y^));
  Test.check (100 IN y^);
  z := NEW (RefSet);
  z^ := Set{1 .. Big};
  Test.check (x^ * y^ = Set{});
  Test.check (x^ + y^ = z^);
  Test.done ();
END Main.
