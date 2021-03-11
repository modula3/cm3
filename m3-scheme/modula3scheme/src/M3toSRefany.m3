(* $Id$ *)

MODULE M3toSRefany;
IMPORT CardRefTbl;
IMPORT RT0, Scheme, RTType, SchemeUtils;

TYPE
  RP = REF RECORD toScheme : ToSchemeProc END;

VAR tbl := NEW(CardRefTbl.Default).init();

PROCEDURE Register(tc : RT0.Typecode; 
                   toScheme : ToSchemeProc) =
  BEGIN
    WITH rp = NEW(RP, toScheme := toScheme) DO
      EVAL tbl.put(tc,rp)
    END
  END Register;

PROCEDURE ToScheme(r : REFANY) : Scheme.Object RAISES { Scheme.E } =
  VAR ref : REFANY;
      tc : RT0.Typecode;
  BEGIN
    tc := TYPECODE(r);
    REPEAT
      IF tbl.get(tc, ref) THEN
        WITH p = NARROW(ref, RP).toScheme DO
          IF p = NIL THEN
            RAISE Scheme.E("Attempt to call NIL procedure")
          ELSE
            RETURN p(r)
          END
        END
      END;
      tc := RTType.Supertype(tc)
    UNTIL tc = RTType.NoSuchType;

    RAISE Scheme.E("No mapping for ToScheme for object " & 
          SchemeUtils.DebugFormat(r)) 
  END ToScheme;

BEGIN END M3toSRefany.
