(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

TYPE T = OBJECT METHODS apply (): REFANY RAISES {} END;

VAR t: T;

PROCEDURE B (self: T): REFANY RAISES ANY = 
  BEGIN
    RETURN self;
  END B;

BEGIN
  t := NEW (T, apply := B);
END Main.
