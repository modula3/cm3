(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
GENERIC INTERFACE List (Elt);


TYPE
  T = REF RECORD
        first: Elt.T;
        rest:  T; END;

  Closure = OBJECT METHODS apply (t: Elt.T) RAISES ANY; END; 

PROCEDURE Push (VAR t: T; x: Elt.T);
PROCEDURE Equal (t, u: T): BOOLEAN;
PROCEDURE Length (t: T): CARDINAL;
PROCEDURE Copy (t: T): T;
PROCEDURE Map (f: Closure; t: T) RAISES ANY;

END List.


