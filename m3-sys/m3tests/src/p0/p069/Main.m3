(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

FROM Test IMPORT msg, checkI, done;

TYPE
  T = OBJECT METHODS m () := tm; END;
  U = T OBJECT OVERRIDES m  := um; END;
  V = T OBJECT OVERRIDES m  := vm; END;

PROCEDURE tm (<*UNUSED*>t: T) = 
  BEGIN
    msg ("t");
    INC (tmmark);
  END tm;

PROCEDURE um (<*UNUSED*>u: T) = 
  BEGIN
    msg ("u");
    INC (ummark);
  END um;

PROCEDURE vm (<*UNUSED*>v: T) = 
  BEGIN
    msg ("v");
    INC (vmmark);
  END vm;

VAR
  t: T; u: U; v: V; tmmark, ummark, vmmark: INTEGER := 0;

BEGIN
  t := NEW (T);
  t.m ();
  u := NEW (U);
  u.m ();
  v := NEW (V);
  v.m ();
  
  checkI (tmmark, 1);
  checkI (ummark, 1);
  checkI (vmmark, 1);

  done ();
END Main.

