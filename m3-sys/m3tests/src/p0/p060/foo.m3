(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE foo;

REVEAL u = t OBJECT a: INTEGER; METHODS END
              BRANDED "aaa" OBJECT b:CHAR; METHODS END;

REVEAL t = BRANDED "bbb"  OBJECT c: BOOLEAN; METHODS END;

BEGIN

END foo.
