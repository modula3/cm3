(* $Id$ *)

MODULE SchemeNavigatorEnvironment;
IMPORT SchemeEnvironment, SchemeObject, SchemePair, SchemeSymbol;
IMPORT SchemeUtils, SchemePrimitive;
IMPORT SchemeEnvironmentClass;
IMPORT Scheme, SchemeProcedure;
FROM Scheme IMPORT E, Object;
FROM SchemeBoolean IMPORT True;
FROM SchemeUtils IMPORT Stringify, Error;

REVEAL
  T = Public BRANDED Brand OBJECT
    mu : MUTEX;
    children : SchemePair.T := NIL;
  OVERRIDES
    attachChildEnvironment := AttachChildEnvironment;
    put                    := Put;
    get                    := Get;
    initEmpty              := InitEmpty;
  END;

TYPE
  Super = SchemeEnvironment.Safe;
  Pair = SchemePair.T;

PROCEDURE AttachChildEnvironment(t : T; 
                                 env : SchemeEnvironment.T; 
                                 tag : SchemeObject.T) =
  BEGIN
    LOCK t.mu DO
      t.children := SchemeUtils.Cons(
                        NEW(SchemePair.T, first := tag, rest := env),
                        t.children)
    END
  END AttachChildEnvironment;

PROCEDURE InitEmpty(t      : T; 
                    parent : SchemeEnvironment.T) : SchemeEnvironment.Instance =
  BEGIN
    EVAL Super.initEmpty(t,parent);

    t.mu := NEW(MUTEX);
    RETURN t
  END InitEmpty;

(* do we really need the IF statements in the below two? *)
PROCEDURE Put(t : T; var : SchemeSymbol.T; READONLY val : SchemeObject.T) =
  BEGIN
    IF var = MagicSymbol THEN
      LOCK t.mu DO
        IF ISTYPE(val, SchemePair.T) THEN
          t.children := val
        END
      END
    ELSIF var = ThisSymbol THEN
      (* skip *)
    ELSE
      Super.put(t,var,val)
    END
  END Put;

PROCEDURE Get(t : T; var : SchemeSymbol.T; 
              VAR val : SchemeObject.T) : BOOLEAN =
  BEGIN
    IF var = MagicSymbol THEN
      LOCK t.mu DO
        val := t.children; RETURN TRUE
      END
    ELSIF var = ThisSymbol THEN
      val := t; RETURN TRUE
    ELSE
      RETURN Super.get(t,var,val)
    END
  END Get;

VAR (* CONST *) MagicSymbol := SchemeSymbol.Symbol(MagicEnvironmentVariable);
                ThisSymbol := SchemeSymbol.Symbol(ThisEnvironmentVariable);

PROCEDURE ExtendWithNavigator(prims : SchemePrimitive.ExtDefiner) : SchemePrimitive.ExtDefiner =
  BEGIN
    prims.addPrim("up-environment",
                  NEW(SchemeProcedure.T, apply := UpEnvApply), 0, 0);

    prims.addPrim("down-environment",
                  NEW(SchemeProcedure.T, apply := DownEnvApply), 1, 1);
    RETURN prims
  END ExtendWithNavigator;

PROCEDURE UpEnvApply(<*UNUSED*>p : SchemeProcedure.T;
                     interp : Scheme.T;
                     <*UNUSED*>args : Object) : Object =
  BEGIN
    WITH env = NARROW(interp.getGlobalEnvironment(),
                      SchemeEnvironment.T).getParent() DO
      IF env # NIL THEN
        interp.changeGlobalEnvironment(env)
      END
    END;
    RETURN True()
  END UpEnvApply;

PROCEDURE DownEnvApply(<*UNUSED*>p : SchemeProcedure.T;
                       interp : Scheme.T;
                       args : Object) : Object RAISES { E } =
  BEGIN
    IF args # NIL AND 
       ISTYPE(args, Pair) AND
       NARROW(args,Pair).first # NIL AND
       ISTYPE(NARROW(args,Pair).first,SchemeEnvironment.T) THEN
      interp.changeGlobalEnvironment(NARROW(args,Pair).first);
      RETURN True()
    ELSE
      RETURN Error("Expected SchemeEnvironment, got " & Stringify(args))
    END;
  END DownEnvApply;

BEGIN END SchemeNavigatorEnvironment.
