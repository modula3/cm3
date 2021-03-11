(* $Id$ *)

MODULE LineMatcher;
IMPORT RegEx, Rd, AL, Debug;
IMPORT Thread;

<* FATAL Thread.Alerted *>

REVEAL
  Default = PubDefault BRANDED Brand & " Default" OBJECT
    lNo := 0;
  OVERRIDES
    execute := DExecute;
    linesRead := DLinesRead;
  END;

PROCEDURE DExecute(def : Default; 
                   pat : RegEx.Pattern; visitor : Visitor) : BOOLEAN =
  VAR line : TEXT; BEGIN
    LOOP
      IF NOT def.nextLine(line,visitor) THEN RETURN FALSE END;
      INC(def.lNo);

      IF RegEx.Execute(pat, line) # -1 THEN
        IF NOT visitor.visit(line) THEN RETURN TRUE END
      END
    END
  END DExecute;

PROCEDURE DLinesRead(def : Default) : CARDINAL = 
  BEGIN RETURN def.lNo END DLinesRead;

REVEAL
  Reader = PubReader BRANDED Brand & " Rd" OBJECT
    rd : Rd.T;
  OVERRIDES
    init := InitRd;
    nextLine := NextLineRd;
  END;

PROCEDURE InitRd(r : Reader; rd : Rd.T) : Reader = 
  BEGIN 
    r.rd := rd;
    RETURN r
  END InitRd;

PROCEDURE NextLineRd(r : Reader; 
                     VAR line : TEXT; visitor : Visitor) : BOOLEAN =
  BEGIN
    TRY
      line := Rd.GetLine(r.rd); RETURN TRUE
    EXCEPT
      Rd.EndOfFile => RETURN FALSE
    |
      Rd.Failure(err) =>
      TYPECASE visitor OF
        RdVisitor(rv) => rv.rdFailure(err); RETURN FALSE (* ??? *)
      ELSE
        Debug.Error("LineMather.NextLineRd: Rd.Failure: " & AL.Format(err));
        <* ASSERT FALSE *>
      END
    END
  END NextLineRd;

BEGIN END LineMatcher.
