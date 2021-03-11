(* $Id$ *)

MODULE SchemeApply;

IMPORT Scheme, SchemeObject, SchemeUtils;

PROCEDURE OneArg(interp : Scheme.T; 
                 closure : SchemeObject.T; 
                 arg : SchemeObject.T) : SchemeObject.T RAISES { Scheme.E } =
  BEGIN
    WITH toRun = SchemeUtils.List2(closure,
                                   arg) DO
      RETURN interp.evalInGlobalEnv(toRun)
    END
  END OneArg;

BEGIN END SchemeApply.
