(* $Id: Main.m3,v 1.1 2010/11/05 21:59:10 mika Exp $ *)

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
IMPORT SchemeStubs;

<*FATAL Thread.Alerted*>

BEGIN 

  SchemeStubs.RegisterStubs();

  WITH arr = NEW(REF ARRAY OF Pathname.T, Params.Count) DO
    arr[0] := "require";
    FOR i := 1 TO Params.Count-1 DO arr[i] := Params.Get(i) END;
    TRY
      WITH scm = NEW(SchemeM3.T).init(arr^, 
                                      globalEnv := 
                                NEW(SchemeNavigatorEnvironment.T).initEmpty()) DO
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

