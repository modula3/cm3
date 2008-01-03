(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Mon Jun 13 15:12:58 PDT 1994 by najork                   *)


MODULE Clock;

FROM Time IMPORT Now;

REVEAL 
  T = Public BRANDED OBJECT
  OVERRIDES
    init := Init;
    time := Time;
  END;


PROCEDURE Init (self : T) : T =
  BEGIN
    RETURN self;
  END Init;


PROCEDURE Time (<* UNUSED *> self : T) : LONGREAL =
  BEGIN
    RETURN Now () - clockstart;
  END Time;

VAR 
  clockstart := Now ();

BEGIN
END Clock.
