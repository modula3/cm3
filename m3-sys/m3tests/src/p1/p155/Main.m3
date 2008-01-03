(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Tue Oct 27 14:49:19 PST 1992 by kalsow *)
(*      modified on Wed Oct 10 13:15:01 1990 by saxe       *)

(* Set operators on a small set type. *)

MODULE Main;
IMPORT Test;

TYPE
  Elt = {a, b, c, d, e};
  Set = SET OF Elt;

VAR
  x: Set;

CONST
  p = Set{Elt.a, Elt.c, Elt.e};
  q = Set{};
  r = Set{Elt.a .. Elt.b, Elt.a .. Elt.a (* , Elt.d .. Elt.b *)};

BEGIN
  Test.check (Elt.a IN p);
  Test.check (NOT (Elt.b IN p));
  Test.check (Elt.c IN p);
  Test.check (NOT (Elt.d IN p));
  Test.check (Elt.e IN p);

  Test.check (NOT (Elt.a IN q));
  Test.check (NOT (Elt.b IN q));
  Test.check (NOT (Elt.c IN q));
  Test.check (NOT (Elt.d IN q));
  Test.check (NOT (Elt.e IN q));

  Test.check (Elt.a IN r);
  Test.check (Elt.b IN r);
  Test.check (NOT (Elt.c IN r));
  Test.check (NOT (Elt.d IN r));
  Test.check (NOT (Elt.e IN r));

  Test.check (r = Set{Elt.b, Elt.a});

  (** Test.check (-p = Set{Elt.d, Elt.b}); **)
  (** Test.check (+p = p); **)
  Test.check (p - p = q);
  Test.check ((p - r) * (r - p) = q);
  Test.check (p - r = Set{Elt.c, Elt.e});
  Test.check (p + p = p);
  Test.check (p + r = Set{Elt.a, Elt.b, Elt.c, Elt.e});
  (**  Test.check (-(-(r)) = r);  **)
  Test.check (p * r = Set{Elt.a});
  Test.check (p * q = Set{});
  Test.check (p # q);
  Test.check (q < p);
  Test.check (NOT (p < r));
  Test.check (NOT (p > r));
  Test.check (NOT (p = r));
  Test.check ((p / r) = (Set{Elt.b, Elt.c, Elt.e}));
  Test.check (r / q = q / r);
  Test.check (r / p / r / p / q / r = r);
  Test.check (p / r - r / p = q);
  x := p;
  x := x + Set{Elt.b}; (*INCL(x, Elt.b);*)
  Test.check (x > p);
  Test.check (x >= p);
  Test.check (p <= x);
  Test.check (p # x);
  x := x - Set{Elt.c}; (*EXCL(x, Elt.c);*)
  Test.check (NOT (p <= x));
  Test.check (NOT (p >= x));
  Test.check (x = r + Set{Elt.e});
  Test.done ();
END Main.
