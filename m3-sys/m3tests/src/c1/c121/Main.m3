(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* File created by Bauers, IBM Rochester. *)

MODULE Main;
TYPE
    T <: T1;
REVEAL
    T <: T2;
TYPE    
    T1 = OBJECT
    END;
    T2 = T1 OBJECT
        f : CHAR;
    END;

VAR v := v2.f;
    v2 : T;

REVEAL
    (* Causes a second initialization of 'Main__T__versionstamp' to be output.  *)
    T <: T2;    
BEGIN
  EVAL v;
END Main.

