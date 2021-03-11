(* $Id$ *)

MODULE SchemeInteraction;
IMPORT SchemeM3, Pathname;
FROM Scheme IMPORT E, Object;
IMPORT Scheme;
IMPORT ReadLine;
FROM SchemeReadLine IMPORT ReturningMainLoop;
IMPORT IP, NetObj, Thread, ReadLineError, AL;
IMPORT SchemeBoolean;
IMPORT SchemeUtils, Debug;

PROCEDURE Hook(env : REFANY; do : Object) : Object RAISES { E } =
  BEGIN
    IF Debug.GetLevel() >= Debug.DefaultLevel THEN
      Debug.Out("do = " & SchemeUtils.Stringify(do))
    END;
    IF NOT SchemeBoolean.TruthO(do) THEN RETURN do END;
    TRY
      WITH interp   = NEW(SchemeM3.T).init(ARRAY OF Pathname.T {},
                                         globalEnv := env),
           readLine = NEW(ReadLine.Default).init() DO
        RETURN ReturningMainLoop(readLine, interp)
      END
    EXCEPT
      IP.Error(e) => RAISE E ("IP.Error: " & AL.Format(e))
    |
      NetObj.Error => RAISE E ("NetObj.Error")
    |
      Thread.Alerted => RAISE E ("Thread.Alerted")
    |
      ReadLineError.E(x) => RAISE E ("ReadLineError.E: " & AL.Format(x))
    END
  END Hook;

BEGIN END SchemeInteraction.
