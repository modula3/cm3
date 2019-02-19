(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

MODULE SchemePair;
IMPORT Wx;
IMPORT SchemeObject, SchemeUtils, SchemeSymbol;
FROM Scheme IMPORT E;
FROM SchemeUtils IMPORT Error, StringifyT;
IMPORT RefSeq;

(*
PROCEDURE Init(t : T; first, rest : SchemeObject.T) : T =
  BEGIN t.first := first; t.rest := rest; RETURN t END Init;

PROCEDURE Equals(t : T; x : SchemeObject.T) : BOOLEAN =
  BEGIN
    IF    t = x THEN
      RETURN TRUE
    ELSIF NOT ISTYPE(x,T) OR x = NIL THEN
      RETURN FALSE
    ELSE
      WITH that = NARROW(x,T) DO
        RETURN  SchemeUtils.Equal(t.first, that.first) AND
                SchemeUtils.Equal(t.rest, that.rest) 
      END       
    END
  END Equals;

PROCEDURE Format(t : T) : TEXT  RAISES { E } =
  BEGIN RETURN SchemeUtils.StringifyQ(t, TRUE) END Format;
*)

PROCEDURE StringifyPair(t : T; quoted : BOOLEAN; buf : Wx.T; seen : RefSeq.T)  RAISES { E } =

  CONST SymEq      = SchemeSymbol.SymEq;
        StringifyB = SchemeUtils.StringifyB;
        Rest       = SchemeUtils.Rest;
        Second     = SchemeUtils.Second;

  VAR special : TEXT := NIL;

  BEGIN
    IF t.rest # NIL AND ISTYPE(t.rest,T) AND Rest(t.rest) = NIL THEN

      IF    SymEq(t.first, "quote") THEN             special := "'"
      ELSIF SymEq(t.first, "quasiquote") THEN        special := "`"
      ELSIF SymEq(t.first, "unquote") THEN           special := ","
      ELSIF SymEq(t.first, "unquote-splicing") THEN  special := ",@"
      END

    END;

    IF special # NIL THEN
      Wx.PutText(buf, special);
      StringifyB(Second(t), quoted, buf, seen)
    ELSE
      Wx.PutChar(buf, '(');
      StringifyB(t.first, quoted, buf, seen);
      VAR tail := t.rest; 
      BEGIN
        WHILE tail # NIL AND ISTYPE(tail,T) DO
          Wx.PutChar(buf, ' ');
          StringifyB(NARROW(tail,T).first, quoted, buf, seen);
          tail := NARROW(tail,T).rest
        END;
        IF tail # NIL THEN
          Wx.PutText(buf, " . ");
          StringifyB(tail, quoted, buf, seen)
        END;
        Wx.PutChar(buf, ')')
      END
    END
  END StringifyPair;

PROCEDURE Pair(x : SchemeObject.T) : T RAISES { E } = 
  BEGIN
    IF ISTYPE(x,T) THEN RETURN x (* NIL is OK for Pair! *)
    ELSE RETURN Pair(Error("expected a pair, got: " & StringifyT(x)))
    END
  END Pair;

BEGIN END SchemePair.
