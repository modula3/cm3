(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Tue Oct 27 15:29:27 PST 1992 by kalsow *)
(*      modified on Wed Oct 17 16:39:35 1990 by saxe       *)

(* Very big sets on the heap, passed as VAR parameters. *)

MODULE Main;
IMPORT Test;

CONST
  Big = 1000000;

TYPE
  Element = [1 .. Big];
  Set = SET OF Element;
  RefSet = REF Set;

VAR
  x, y, z: RefSet;
  a, b: INTEGER;

PROCEDURE Sieve(VAR s: Set) =
  VAR i, j: INTEGER;  t := NEW (RefSet);
  BEGIN
    i := 1;  j := Big;  s := Set {i..j};  (** s := Set{1..Big}; **)
    FOR i := 1 TO Big DO
      j := i;
      WHILE j <= Big DO
        t^ := Set{};
        t^ := t^ + Set {j};  (* INCL(t^, j); *)
        s := s / t^;
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
      t := t + Set {i * i};  (* INCL(t, i * i); *)
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
  a := 1;  b := Big;  z^ := Set{a .. b};  (* z^ := Set{1 .. Big}; *)
  Test.check (x^ * y^ = Set{});
  Test.check (x^ + y^ = z^);
  Test.done ();
END Main.
