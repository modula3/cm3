(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
INTERFACE A;

EXCEPTION A; B; C;

PROCEDURE Foo (i : INTEGER) RAISES {A};

END A.
 
