(* $Id$ *)

MODULE SchemeConvertHooks;
IMPORT RefList;
IMPORT RT0, RTType;
IMPORT Scheme;

VAR
  toScheme, toModula : RefList.T := NIL;

TYPE
  Rec = OBJECT
    from, to : RT0.Typecode;
    c : T;
  END;

PROCEDURE RegisterToScheme(fromTC : RT0.Typecode; converter : T) =
  BEGIN Register(toScheme, fromTC, 0, converter) END RegisterToScheme;

PROCEDURE RegisterToModula(fromTC, toTC : RT0.Typecode; converter : T) =
  BEGIN Register(toModula, fromTC, toTC, converter) END RegisterToModula;

PROCEDURE Register(VAR lst : RefList.T; 
                   from, to : RT0.Typecode; converter : T) =
  BEGIN
    WITH rec = NEW(Rec, from := from, to := to, c := converter) DO
      lst := RefList.Cons(rec, lst)
    END
  END Register;

PROCEDURE AttemptConvertToScheme(VAR x : REFANY) : BOOLEAN
  RAISES { Scheme.E } = 
  BEGIN RETURN Attempt(toScheme, 0, x) END AttemptConvertToScheme;

PROCEDURE AttemptConvertToModula(tgt : RT0.Typecode; VAR x : REFANY) : BOOLEAN
  RAISES { Scheme.E } =
  BEGIN RETURN Attempt(toModula, tgt, x) END AttemptConvertToModula;

PROCEDURE Attempt(p : RefList.T; tgt : RT0.Typecode; VAR x : REFANY) : BOOLEAN
  RAISES { Scheme.E } =
  VAR 
    fromTc := TYPECODE(x);
  BEGIN
    IF x = NIL THEN RETURN FALSE END;
    WHILE p # NIL DO
      WITH t = NARROW(p.head, Rec) DO
        IF (t.to = tgt AND RTType.IsSubtype(t.to  , tgt)) AND 
           RTType.IsSubtype(fromTc, t.from) THEN
          x := t.c.convert(x); RETURN TRUE
        END
      END;
      p := p.tail
    END;
    RETURN FALSE
  END Attempt;

BEGIN END SchemeConvertHooks.
