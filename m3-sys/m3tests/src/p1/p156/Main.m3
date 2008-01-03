(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Tue Oct 27 14:55:48 PST 1992 by kalsow *)
(*      modified on Wed Oct 10 17:44:50 1990 by saxe       *)

(* Set operators on a set type that doesn't fit in a word. *)

MODULE Main;
IMPORT Test;

TYPE
  E =
    {a, b, c, d, e, f, g, h, i, j, a1, b1, c1, d1, e1, f1, g1, h1, i1,
     j1, a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, a3, b3, c3, d3, e3, f3,
     g3, h3, i3, j3, a4, b4, c4, d4, e4, f4, g4, h4, i4, j4, a5, b5, c5,
     d5, e5, f5, g5, h5, i5, j5, a6, b6, c6, d6, e6, f6, g6, h6, i6, j6,
     a7, b7, c7, d7, e7, f7, g7, h7, i7, j7, a8, b8, c8, d8, e8, f8, g8,
     h8, i8, j8, a9, b9, c9, d9, e9, f9, g9, h9, i9, j9};

  Set = SET OF E;

VAR
  x, y: Set;

CONST
  p = Set{E.a2, E.c4, E.e6};
  q = Set{};
  r = Set{E.a2 .. E.b3, E.a2 .. E.a2, E.a4 .. E.a4, E.d5 .. E.b1};

BEGIN
  Test.check (E.a2 IN p);
  Test.check (NOT (E.b3 IN p));
  Test.check (E.c4 IN p);
  Test.check (NOT (E.d5 IN p));
  Test.check (E.e6 IN p);

  Test.check (NOT (E.a IN q));
  Test.check (NOT (E.b1 IN q));
  Test.check (NOT (E.c2 IN q));
  Test.check (NOT (E.d3 IN q));
  Test.check (NOT (E.e4 IN q));

  Test.check (E.a4 IN r);
  Test.check (E.g2 IN r);
  Test.check (NOT (E.c IN r));
  Test.check (NOT (E.d5 IN r));
  Test.check (NOT (E.e IN r));

  Test.check (r = Set{E.a4, E.a2, E.b2, E.c2, E.d2, E.e2,
                      E.f2, E.g2, E.h2, E.i2, E.j2, E.a3, E.b3});

(*  Test.check (
    -p =
      Set{E.b, E.d, E.f, E.g, E.h, E.i, E.j, E.a1, E.b1,
          E.c1, E.d1, E.e1, E.f1, E.g1, E.h1, E.i1, E.j1,
          E.a2, E.b2, E.c2, E.d2, E.e2, E.f2, E.g2, E.h2,
          E.i2, E.j2, E.a3, E.b3, E.c3, E.d3, E.e3,
          E.f3, E.g3, E.h3, E.i3, E.j3, E.a4, E.b4, E.c4,
          E.d4, E.e4, E.f4, E.g4, E.h4, E.i4, E.j4,
          E.a5, E.b5, E.c5, E.d5, E.e5, E.f5, E.g5, E.h5,
          E.i5, E.j5, E.a6, E.b6, E.c6, E.d6, E.e6,
          E.f6, E.g6, E.h6, E.i6, E.j6, E.a7, E.b7, E.c7,
          E.d7, E.e7, E.f7, E.g7, E.h7, E.i7, E.j7,
          E.a8, E.b8, E.c8, E.d8, E.e8, E.f8, E.g8, E.h8,
          E.i8, E.j8, E.a9, E.b9, E.c9, E.d9, E.e9,
          E.f9, E.g9, E.h9, E.i9, E.j9});
*)

  (** Test.check (+p = p); **)
  Test.check (p - p = q);
  Test.check ((p - r) * (r - p) = q);
  Test.check (p - r = Set{E.c4, E.e6});
  Test.check (p + p = p);
  Test.check (
    p + r =
      Set{E.a4, E.c4, E.e6..E.e6, E.a2, E.b2, E.c2, E.d2,
          E.e2, E.f2, E.g2, E.h2, E.i2, E.j2, E.a3, E.b3});
  (** Test.check (-(-(r)) = r); **)
  Test.check (p * r = Set{E.a2});
  Test.check (p * q = Set{});
  Test.check (p # q);
  Test.check (q < p);
  Test.check (NOT (p < r));
  Test.check (NOT (p > r));
  Test.check (NOT (p = r));
  Test.check (
    p / r =
      Set{E.a4, E.c4, E.e6, E.b2, E.c2, E.d2, E.e2,
          E.f2, E.g2, E.h2, E.i2, E.j2, E.a3, E.b3});
  Test.check (r / q = q / r);
  Test.check (r / p / r / p / q / r = r);
  Test.check (p / r - r / p = q);
  Test.check (p / r + r / p = p / r / q);
  (** Test.check (-q = Set{a .. E.j9}); **)
  x := p;
  x := x + Set{E.b7};  (* INCL(x, E.b7); *)
  Test.check (x > p);
  Test.check (x >= p);
  Test.check (p <= x);
  Test.check (p # x);
  x := x - Set{E.c4};  (* EXCL(x, E.c4); *)
  Test.check (NOT (p <= x));
  Test.check (NOT (p >= x));
  y := p;
  Test.check (x / y = Set{E.b7, E.c4});
  Test.done ();
END Main.
