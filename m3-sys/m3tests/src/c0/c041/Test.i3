(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: FIRST and LAST as constants *)

INTERFACE Test;

TYPE  Enum = [0..127]; 

CONST Low = FIRST (Enum); High = LAST (Enum);

END Test.
