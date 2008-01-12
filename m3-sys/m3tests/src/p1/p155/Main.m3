(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Tue Oct 27 14:49:19 PST 1992 by kalsow *)
(*      modified on Wed Oct 10 13:15:01 1990 by saxe       *)

(* Set operators on a small set type. *)

MODULE Main;
IMPORT Test, Wr;
FROM Stdio IMPORT stderr;

TYPE
  Elt = {a, b, c, d, e};
  Set = SET OF Elt;

VAR
  x: Set;

CONST
  p = Set{Elt.a, Elt.c, Elt.e};
  q = Set{};
  r = Set{Elt.a .. Elt.b, Elt.a .. Elt.a (* , Elt.d .. Elt.b *)};

PROCEDURE m( s: TEXT ) =
  BEGIN
    TRY
      Wr.PutText (stderr, s & "\n");
      Wr.Flush (stderr);
    EXCEPT ELSE END;
  END m;

BEGIN
  m ("check set p = {a, c, e}");
  Test.checkM (Elt.a IN p, "Elt.a IN p");
  Test.checkM (NOT (Elt.b IN p), "NOT (Elt.b IN p)");
  Test.checkM (Elt.c IN p, "Elt.c IN p");
  Test.checkM (NOT (Elt.d IN p), "NOT (Elt.d IN p)");
  Test.checkM (Elt.e IN p, "(Elt.e IN p");

  m ("check set q = {}");
  Test.checkM (NOT (Elt.a IN q), "NOT (Elt.a IN q)");
  Test.checkM (NOT (Elt.b IN q), "NOT (Elt.b IN q)");
  Test.checkM (NOT (Elt.c IN q), "NOT (Elt.c IN q)");
  Test.checkM (NOT (Elt.d IN q), "NOT (Elt.d IN q)");
  Test.checkM (NOT (Elt.e IN q), "NOT (Elt.e IN q)");

  m ("check set r = {a, b}");
  Test.checkM (Elt.a IN r, "Elt.a IN r");
  Test.checkM (Elt.b IN r, "Elt.b IN r");
  Test.checkM (NOT (Elt.c IN r), "NOT (Elt.c IN r)");
  Test.checkM (NOT (Elt.d IN r), "NOT (Elt.d IN r)");
  Test.checkM (NOT (Elt.e IN r), "NOT (Elt.e IN r)");

  Test.checkM (r = Set{Elt.b, Elt.a}, "r = Set{Elt.b, Elt.a}");

  (** Test.checkM (-p = Set{Elt.d, Elt.b}); **)
  (** Test.checkM (+p = p); **)
  Test.checkM (p - p = q, "check (p - p = q)");
  Test.checkM ((p - r) * (r - p) = q, "check ((p - r) * (r - p) = q)");
  Test.checkM (p - r = Set{Elt.c, Elt.e}, "check (p - r = Set{Elt.c, Elt.e})");
  Test.checkM (p + p = p, "check (p + p = p)");
  Test.checkM (p + r = Set{Elt.a, Elt.b, Elt.c, Elt.e},
               "check (p + r = Set{Elt.a, Elt.b, Elt.c, Elt.e})");
  (**  Test.checkM (-(-(r)) = r);  **)
  Test.checkM (p * r = Set{Elt.a}, "check (p * r = Set{Elt.a})");
  Test.checkM (p * q = Set{}, "check (p * q = Set{})");
  Test.checkM (p # q, "check (p # q)");
  Test.checkM (q < p, "check (q < p)");
  Test.checkM (NOT (p < r), "check (NOT (p < r))");
  Test.checkM (NOT (p > r), "check (NOT (p > r))");
  Test.checkM (NOT (p = r), "check (NOT (p = r))");
  Test.checkM ((p / r) = (Set{Elt.b, Elt.c, Elt.e}),
               "check ((p / r) = (Set{Elt.b, Elt.c, Elt.e}))");
  Test.checkM (r / q = q / r, "check (r / q = q / r)");
  Test.checkM (r / p / r / p / q / r = r, "check (r / p / r / p / q / r = r)");
  Test.checkM (p / r - r / p = q, "check (p / r - r / p = q)");
  x := p;
  x := x + Set{Elt.b}; (*INCL(x, Elt.b);*)
  (* x = { a, b, c, e } *)
  Test.checkM (x > p, "check (x > p)");
  Test.checkM (x >= p, "check (x >= p)");
  Test.checkM (p <= x, "check (p <= x)");
  Test.checkM (p # x, "check (p # x)");
  x := x - Set{Elt.c}; (*EXCL(x, Elt.c);*)
  (* x = { a, b, e } *)
  m ("check set x = {a, b, e}");
  Test.checkM (Elt.a IN x, "Elt.a IN x");
  Test.checkM (Elt.b IN x, "Elt.b IN x");
  Test.checkM (NOT (Elt.c IN x), "NOT (Elt.c IN x)");
  Test.checkM (NOT (Elt.d IN x), "NOT (Elt.d IN x)");
  Test.checkM (Elt.e IN x, "Elt.e IN x");
  Test.checkM (NOT (p <= x), "check (NOT (p <= x))");
  Test.checkM (NOT (p >= x), "check (NOT (p >= x))");
  Test.checkM (x = r + Set{Elt.e}, "check (x = r + Set{Elt.e})");
  Test.done ();
END Main.
