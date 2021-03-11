(* $Id$ *)

MODULE SchemeM3TableOps;
IMPORT CardAtomPair, CARefTbl;
IMPORT Scheme, RT0, RTType;
IMPORT SchemeUtils, SchemeSymbol;

REVEAL 
  T = Public BRANDED Brand OBJECT
    tbl : CARefTbl.T;
  METHODS 
    init() : T := Init;
  OVERRIDES
    apply := Apply;
  END;

TYPE
  RP = REF RECORD proc : Proc END;

PROCEDURE Init(t : T) : T = 
  BEGIN t.tbl := NEW(CARefTbl.Default).init(); RETURN t END Init;

PROCEDURE Apply(t : T; args : Scheme.Object) : Scheme.Object 
  RAISES { Scheme.E } =
  VAR ref : REFANY;
      tc : RT0.Typecode;
  BEGIN
    WITH obj = SchemeUtils.First(args),
         sym = SchemeUtils.Second(args),
         arg = SchemeUtils.Third(args),

         ctc = TYPECODE(obj) DO

      tc := ctc;
      REPEAT

        WITH haveProc = t.tbl.get(CardAtomPair.T { tc, sym }, ref) DO

          IF haveProc THEN
            WITH p = NARROW(ref, RP).proc DO
              IF p = NIL THEN
                RAISE Scheme.E("Attempt to call NIL procedure")
              ELSE
                RETURN p(obj, arg, sym)
              END
            END
          END
        END;
        
        tc := RTType.Supertype(tc)
      UNTIL tc = RTType.NoSuchType;
      
      RAISE Scheme.E("No mapping for routine " & SchemeSymbol.ToText(sym) &
            " for object " & SchemeUtils.DebugFormat(obj))
    END
  END Apply;

VAR default := NEW(T).init();

PROCEDURE Register(tc       :  RT0.Typecode; 
                   opName   :  Scheme.Symbol; 
                   proc     :  Proc) =
  BEGIN
    EVAL default.tbl.put(CardAtomPair.T { tc, opName }, NEW(RP, proc := proc))
  END Register;

PROCEDURE DefaultOps() : T = BEGIN RETURN default END DefaultOps;

BEGIN END SchemeM3TableOps.
