(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE  Main;

IMPORT Wr, Stdio, Thread;
<* FATAL Wr.Failure, Thread.Alerted *>

TYPE
  A = OBJECT METHODS f() := f; END;
  B = A OBJECT OVERRIDES f := g; END;
  C = A OBJECT METHODS f() := g; END;

PROCEDURE f (<*UNUSED*> self: A) =
  BEGIN
    Wr.PutText (Stdio.stdout, "f\n");
  END f;

PROCEDURE g (<*UNUSED*> self: A) =
  BEGIN
    Wr.PutText (Stdio.stdout, "g\n");
  END g;

VAR
  a: A := NEW (A);
  b: B := NEW (B);
  c: C := NEW (C);

  ba: A := NEW (B);
  ca: A := NEW (C);

BEGIN
  a.f ();  (* => f *)
  b.f ();  (* => g *)
  c.f ();  (* => g *)

  ba.f (); (* => g *)
  ca.f (); (* => f *)

  NARROW (a, A).f ();  (* => f *)
  NARROW (b, A).f ();  (* => g *)
  NARROW (c, A).f ();  (* => f *)

  NARROW (ba, A).f (); (* => g *)
  NARROW (ca, A).f (); (* => f *)
END Main.


