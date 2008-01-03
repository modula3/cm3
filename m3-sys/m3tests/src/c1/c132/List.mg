(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
GENERIC MODULE List (Elt);

PROCEDURE Push (VAR t: T; x: Elt.T) =
  BEGIN
    t := NEW (T, first := x, rest := t);
  END Push;
    
PROCEDURE Equal (t, u: T): BOOLEAN =
  BEGIN
    IF t = NIL AND u = NIL THEN
      RETURN TRUE;
    ELSIF t = NIL OR u = NIL THEN
      RETURN FALSE;
    ELSIF Elt.Equal (t.first, u.first) THEN
      RETURN Equal (t.rest, u.rest);
    ELSE  
      RETURN FALSE; END;
  END Equal;

      
PROCEDURE Length (t: T): CARDINAL =
  BEGIN
    IF t = NIL THEN 
       RETURN 0;
    ELSE 
       RETURN 1 + Length (t.rest); END;
  END Length;
 
PROCEDURE Copy (t: T): T =
  BEGIN
    IF t = NIL THEN 
      RETURN NIL;
    ELSE
      RETURN NEW (T, first := Elt.Copy (t.first), rest := Copy (t.rest)); END;
  END Copy;

PROCEDURE Map (f: Closure; t: T) RAISES ANY =
  BEGIN
    WHILE (t # NIL) DO
      f.apply (t.first);
      t := t.rest;
    END;
  END Map;

BEGIN
END List.


