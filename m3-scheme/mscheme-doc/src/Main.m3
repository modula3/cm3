(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)


MODULE Main;
IMPORT Pathname, Params, Scheme, Debug, OSError, ReadLineError, NetObj;
IMPORT AL, IP, ReadLine;
FROM SchemeReadLine IMPORT MainLoop;
IMPORT Thread;
IMPORT SchemeM3;
IMPORT SchemeNavigatorEnvironment;
FROM SchemePrimitive IMPORT Definer;
IMPORT SchemePrimitive, SchemeEnvironment;
IMPORT SchemePair, SchemeSymbol;

<*FATAL Thread.Alerted*>

TYPE 
  Pair = SchemePair.T;

TYPE
  DR = RECORD
    d : Definer;
    n : TEXT;
  END;

PROCEDURE DefineEnvironments(scm : Scheme.T) =
  BEGIN

    WITH  
      a = ARRAY OF DR {
    DR { NEW(SchemePrimitive.SandboxDefiner), "sandbox" },
    DR { NEW(SchemePrimitive.DefaultDefiner), "default" },
    DR { SchemeNavigatorEnvironment.ExtendWithNavigator(NEW(SchemePrimitive.ExtDefiner).init()),                             "navigator" },
    DR { SchemeM3.ExtendWithM3(NEW(SchemePrimitive.ExtDefiner).init()),
                                              "modula3scheme" }}
      DO

    FOR i := FIRST(a) TO LAST(a) DO
      WITH env = NEW(SchemeEnvironment.Safe).initEmpty() DO
        EVAL a[i].d.installPrimitives(env);
        scm.bind(a[i].n & "-environment", env)
      END
    END;

    VAR 
      lst : Pair := NIL;
    BEGIN
      FOR i := LAST(a) TO FIRST(a) BY -1 DO
        lst := NEW(Pair, 
                   first := SchemeSymbol.Symbol(a[i].n),
                   rest := lst)
      END;
      scm.bind("environment-lst", lst)
    END
  END
  END DefineEnvironments;

BEGIN 

  WITH arr = NEW(REF ARRAY OF Pathname.T, Params.Count) DO
    arr[0] := "require";
    FOR i := 1 TO Params.Count-1 DO arr[i] := Params.Get(i) END;
    TRY
      WITH scm = NEW(SchemeM3.T) DO
        EVAL scm.init(arr^, 
                      globalEnv := 
                          NEW(SchemeNavigatorEnvironment.T).initEmpty());

        DefineEnvironments(scm);

        MainLoop(NEW(ReadLine.Default).init(), scm)
      END
    EXCEPT
      Scheme.E(err) => Debug.Error("Caught Scheme.E : " & err)
    |
      IP.Error(err) => Debug.Error("Caught IP.Error : " & AL.Format(err))
    |
      OSError.E(err) => 
        Debug.Error("Caught NetObj.Error : " & AL.Format(err))
    |
      ReadLineError.E(err) => 
        Debug.Error("Caught ReadLineError.E : " & AL.Format(err))
    |
      NetObj.Error(err) => Debug.Error("Caught NetObj.Error : " & 
                                        AL.Format(err))
    END
  END

END Main.
