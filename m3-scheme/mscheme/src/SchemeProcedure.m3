(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

MODULE SchemeProcedure;
IMPORT SchemeProcedureClass;
FROM SchemeUtils IMPORT Stringify, Error, List1, List2;
FROM Scheme IMPORT Object, E;
IMPORT Scheme;

REVEAL
  T = SchemeProcedureClass.Private BRANDED Brand OBJECT
  OVERRIDES
    format := Format;
    apply1 := Apply1;
    apply2 := Apply2;
  END;

PROCEDURE Apply1(t : T; interp : Scheme.T; a1 : Object) : Object RAISES { E }=
  BEGIN RETURN t.apply(interp, List1(a1)) END Apply1;

PROCEDURE Apply2(t : T; interp : Scheme.T; a1, a2 : Object) : Object 
  RAISES { E }=
  BEGIN RETURN t.apply(interp, List2(a1,a2)) END Apply2;

PROCEDURE Format(t : T) : TEXT =
  BEGIN RETURN "{" & t.name & "}" END Format;

PROCEDURE Proc(x : Object) : T RAISES { E } =
  BEGIN
    IF x # NIL AND ISTYPE(x,T) THEN RETURN x 
    ELSE RETURN Proc(Error("Not a procedure: " & Stringify(x))) 
    END
  END Proc;

BEGIN END SchemeProcedure.
