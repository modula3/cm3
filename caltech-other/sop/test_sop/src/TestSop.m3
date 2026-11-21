MODULE TestSop;
IMPORT Debug, Fmt;
IMPORT Sop, Bool, BoolTextTbl;
IMPORT RTProcess;

PROCEDURE Run() =
  BEGIN 
    Bool.Init();
    VAR 
      boolTab := NEW(BoolTextTbl.Default).init();
      a, b, c := Bool.New();
      name : TEXT;
      xa := NEW(Sop.T).init(a);
      xb := NEW(Sop.T).init(b);
      xc := NEW(Sop.T).init(c);
    BEGIN 
      Debug.Out("TestSop ready.");
      EVAL boolTab.put(a, "a");
      EVAL boolTab.put(b, "b");
      EVAL boolTab.put(c, "c");

      EVAL boolTab.get(a,name);
      Debug.Out("a = \"" & name & "\"");
      Debug.Out("xa = " & xa.format(boolTab));
      Debug.Out("xb = " & xb.format(boolTab));
      Debug.Out("xc = " & xc.format(boolTab));

      VAR
        sopArr := ARRAY [0..3] OF Sop.T 
                 { Sop.Not(xa), Sop.Or(xa,xb), Sop.And(xa,xb), Sop.Or(xa,xa) };
      BEGIN
        FOR i := FIRST(sopArr) TO LAST(sopArr) DO
          Debug.Out("sopArr[" & Fmt.Int(i) & "] = " & 
            sopArr[i].format(boolTab))
        END
      END
    END;
    RTProcess.Exit(0) (* exit program *)
  END Run;

BEGIN END TestSop.
