UNSAFE MODULE ExceptionInfo;
IMPORT Text;
IMPORT RT0;
FROM Fmt IMPORT F, Int;

PROCEDURE Fmt(a : ADDRESS) : TEXT =
  VAR
    module : TEXT;
  BEGIN
    WITH act    = LOOPHOLE(a,RT0.ActivationPtr)^,
         ex     = act.exception^,
         exName = ex.name DO
      IF act.module = NIL THEN
        module := "NIL"
      ELSE
        module := StringToText(act.module.file)
      END;
      RETURN F("exception %s mod %s line %s",
               StringToText(exName),
               module,
               Int(act.line))
    END
  END Fmt;

PROCEDURE StringToText(str : RT0.String) : TEXT =
  VAR
    p := str;
    res := "";
  BEGIN
    WHILE p^ # VAL(0,CHAR) DO
      res := res & Text.FromChar(p^); (* super inefficient *)
      INC(p)
    END;
    RETURN res
  END StringToText;

BEGIN END ExceptionInfo.
