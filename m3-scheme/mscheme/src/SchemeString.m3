(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

MODULE SchemeString;
IMPORT Text, Scheme;
FROM SchemeUtils IMPORT Error, StringifyT;

PROCEDURE FromText(txt : TEXT) : T =
  BEGIN
    IF txt = NIL THEN RETURN NIL END;

    VAR     str := NEW(T, Text.Length(txt));
    BEGIN
      FOR i := FIRST(str^) TO LAST(str^) DO
        str[i] := Text.GetChar(txt,i)
      END;
      RETURN str
    END
  END FromText;

PROCEDURE ToText(t : Scheme.Object) : TEXT RAISES { Scheme.E } =
  BEGIN 
    IF t =  NIL THEN
      RETURN NIL
    ELSIF ISTYPE(t, T) THEN 
      RETURN Text.FromChars(NARROW(t,T)^) 
    ELSE RETURN ToText(Error("expected a string, got: " & StringifyT(t)))
    END

  END ToText;

BEGIN END SchemeString.
