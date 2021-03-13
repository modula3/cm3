(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Tue Oct 27 15:20:03 PST 1992 by kalsow *)
(*      modified on Wed Oct 10 13:16:01 1990 by saxe       *)

(* Set operators on a small set type in a packed record. *)

MODULE Main;
IMPORT Test;

TYPE
  Elt    = {a, b, c, d, e};
  Set    = BITS 5 FOR SET OF Elt;
  Record = BITS 16 FOR RECORD p, q, r: Set END;

VAR
  g, h: Record;


BEGIN
  g.p := Set{Elt.a, Elt.c, Elt.e};
  g.q := Set{};
  g.r := Set{Elt.a .. Elt.b, Elt.a .. Elt.a, Elt.d .. Elt.b};

  Test.checkI (BITSIZE (Set), 5);
  Test.checkI (BITSIZE (g.p), 5);

  Test.checkI (BITSIZE (Record), 16);
  Test.checkI (BITSIZE (g), 16);

  Test.check (Elt.a IN g.p);
  Test.check (NOT (Elt.b IN g.p));
  Test.check (Elt.c IN g.p);
  Test.check (NOT (Elt.d IN g.p));
  Test.check (Elt.e IN g.p);

  Test.check (NOT (Elt.a IN g.q));
  Test.check (NOT (Elt.b IN g.q));
  Test.check (NOT (Elt.c IN g.q));
  Test.check (NOT (Elt.d IN g.q));
  Test.check (NOT (Elt.e IN g.q));

  Test.check (Elt.a IN g.r);
  Test.check (Elt.b IN g.r);
  Test.check (NOT (Elt.c IN g.r));
  Test.check (NOT (Elt.d IN g.r));
  Test.check (NOT (Elt.e IN g.r));

  Test.check (g.r = Set{Elt.b, Elt.a});

  (** Test.check (-g.p = Set{Elt.d, Elt.b}); **)
  (** Test.check (+g.p = g.p); **)
  Test.check (g.p - g.p = g.q);
  Test.check ((g.p - g.r) * (g.r - g.p) = g.q);
  Test.check (g.p - g.r = Set{Elt.c, Elt.e});
  Test.check (g.p + g.p = g.p);
  Test.check (g.p + g.r = Set{Elt.a, Elt.b, Elt.c, Elt.e});
  (** Test.check (-(-(g.r)) = g.r); **)
  Test.check (g.p * g.r = Set{Elt.a});
  Test.check (g.p * g.q = Set{});
  Test.check (g.p # g.q);
  Test.check (g.q < g.p);
  Test.check (NOT (g.p < g.r));
  Test.check (NOT (g.p > g.r));
  Test.check (NOT (g.p = g.r));
  Test.check ((g.p / g.r) = (Set{Elt.b, Elt.c, Elt.e}));
  Test.check (g.r / g.q = g.q / g.r);
  Test.check (g.r / g.p / g.r / g.p / g.q / g.r = g.r);
  Test.check (g.p / g.r - g.r / g.p = g.q);

  h.q := g.p;
  h.q := h.q + Set {Elt.b};  (* INCL(h.q, Elt.b); *)
  Test.check (h.q > g.p);
  Test.check (h.q >= g.p);
  Test.check (g.p <= h.q);
  Test.check (g.p # h.q);
  h.q := h.q - Set {Elt.c};  (* EXCL(h.q, Elt.c); *)
  Test.check (NOT (g.p <= h.q));
  Test.check (NOT (g.p >= h.q));
  Test.check (h.q = g.r + Set{Elt.e});
  Test.done ();
END Main.
