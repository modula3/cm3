(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)


MODULE Main;
IMPORT Pathname, Scheme, Debug, OSError, ReadLineError, NetObj;
IMPORT AL, IP, ReadLine;
FROM SchemeReadLine IMPORT MainLoop, ReturningMainLoop;
IMPORT Thread;
IMPORT SchemeM3;
IMPORT SchemeNavigatorEnvironment;
IMPORT ParseParams, Stdio;
IMPORT SchemeInteraction;

<*FATAL Thread.Alerted*>

VAR files : REF ARRAY OF Pathname.T;
    readLine : ReadLine.T;
    doReturn : BOOLEAN;
BEGIN 
  TRY
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
      doReturn := pp.keywordPresent("-return");
      IF pp.keywordPresent("-noreadline") THEN
        readLine := NIL
      ELSE
        readLine := NEW(ReadLine.Default).init()
      END;

      pp.skipParsed();
      WITH nFiles = NUMBER(pp.arg^) - pp.next DO
        files := NEW(REF ARRAY OF Pathname.T, nFiles+1);
        files[0] := "require";
        FOR i := 0 TO nFiles-1 DO
          files[i+1] := pp.getNext()
        END
      END;

      pp.finish()
    END
  EXCEPT
    ParseParams.Error => Debug.Error("Couldn't parse cmd-line params")
  END;

  Scheme.SetInteractionHook(SchemeInteraction.Hook);

  TRY
    WITH scm = NEW(SchemeM3.T).init(files^, 
                                    globalEnv := 
                                        NEW(SchemeNavigatorEnvironment.T).initEmpty()) DO
      IF doReturn THEN EVAL ReturningMainLoop(readLine, scm)
      ELSE MainLoop(readLine, scm)
      END
    END
  EXCEPT
    Scheme.E(err) => Debug.Error("Caught Scheme.E : " & err)
  |
    IP.Error(err) => Debug.Error("Caught IP.Error : " & AL.Format(err))
  |
    ReadLineError.E(err) => 
    Debug.Error("Caught ReadLineError.E : " & AL.Format(err))
  |
    NetObj.Error(err) => Debug.Error("Caught NetObj.Error : " & 
      AL.Format(err))
  END
END Main.

