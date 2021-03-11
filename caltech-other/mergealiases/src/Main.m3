(* $Id$ *)

MODULE Main;
IMPORT Text_TextSet_Tbl;
IMPORT TextSet, TextSetDef;
IMPORT TextReader, Rd;
IMPORT Stdio, FileRd, Params, Process;
IMPORT Wr;

IMPORT OSError, Thread;

<* FATAL OSError.E, Thread.Alerted, Rd.Failure, Wr.Failure *>

VAR
  stuff := NEW(Text_TextSet_Tbl.Default).init();
  rd := Stdio.stdin;
BEGIN 
  IF Params.Count = 2 THEN
    rd := FileRd.Open(Params.Get(1))
  ELSIF Params.Count > 2 THEN
    Process.Crash("Too many command-line arguments!")
  END;
    
  TRY
    LOOP
      VAR
        line := Rd.GetLine(rd);
        reader := NEW(TextReader.T).init(line);
        lp := reader.shatter(" ", "", TRUE);
        set := NEW(TextSetDef.T).init();
        old : TextSet.T;
      BEGIN
        WHILE lp # NIL DO
          EVAL set.insert(lp.head);
          
          IF stuff.get(lp.head,old) THEN
            EVAL set.unionD(old)
          END;
          
          lp := lp.tail
        END;

        VAR
          iter := set.iterate();
          nam : TEXT;
        BEGIN
          WHILE iter.next(nam) DO
            EVAL stuff.put(nam,set)
          END
        END

      END;

    END
  EXCEPT
    Rd.EndOfFile => (* skip *)
  END;

  VAR
    iter := stuff.iterate();
    done := NEW(TextSetDef.T).init();
    set : TextSet.T;
    nam : TEXT;
  BEGIN
    WHILE iter.next(nam,set) DO
      IF NOT done.member(nam) THEN
        VAR
          jter := set.iterate();
          x : BOOLEAN;
        BEGIN
          WHILE jter.next(nam) DO
            x := done.insert(nam);
            <* ASSERT NOT x *>

            Wr.PutText(Stdio.stdout,nam);
            Wr.PutChar(Stdio.stdout,' ')
          END;
          Wr.PutChar(Stdio.stdout,'\n')
        END
      END;

    END
  END;


END Main.
