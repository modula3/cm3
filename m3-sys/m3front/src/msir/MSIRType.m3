MODULE MSIRType;

IMPORT MSIR, Type, Int, LInt, Bool, Target;

PROCEDURE Translate(t: Type.T): MSIR.T =
  VAR base: Type.T;
  BEGIN
    IF t = NIL THEN RETURN NIL END;
    base := Type.Base(t);
    IF base = Int.T THEN
      RETURN MSIR.TI(Target.Integer.size);
    ELSIF base = LInt.T THEN
      RETURN MSIR.TI(Target.Longint.size);
    ELSIF base = Bool.T THEN
      RETURN MSIR.TI1();
    ELSE
      RETURN NIL;
    END;
  END Translate;

PROCEDURE TranslateResult(t: Type.T): MSIR.T =
  BEGIN
    IF t = NIL THEN RETURN MSIR.TVoid() END;
    RETURN Translate(t);
  END TranslateResult;

BEGIN
END MSIRType.
