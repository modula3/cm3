(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
INTERFACE A;

IMPORT Text;

TYPE
  T = Text.T;

<*OBSOLETE*> PROCEDURE P (t: T): T;

END A.

