MODULE Main;
(* FROM Stdio IMPORT stderr; *)
IMPORT TextReader;
IMPORT Rd;
IMPORT Text;
IMPORT Wr;
IMPORT TextUtils;
IMPORT TextList;
IMPORT Args;
IMPORT FileRd;
IMPORT FileWr;
IMPORT Thread;
IMPORT OSError;

<* FATAL Thread.Alerted, Wr.Failure, Rd.Failure, OSError.E *>

PROCEDURE DoLine(wr: Wr.T; libname: TEXT; idents: TextList.T) =
  BEGIN
    IF idents # NIL THEN
      (* Wr.PutText(stderr, "idents.head = " & idents.head & "\n"); *)
      IF TextUtils.HavePrefix(idents.head, "_map_add") THEN
        (* Wr.PutText(stderr, "prefix.\n"); *)
        Wr.PutText(wr, idents.head & "(" &
          idents.tail.head & ", \"" & libname & "\"");
        VAR
          cur := idents.tail.tail.tail;
        BEGIN
          WHILE cur # NIL DO
            Wr.PutText(wr, ", " & cur.head);
            cur := cur.tail;
          END;
          Wr.PutText(wr, ")\n");
        END;
      ELSIF TextUtils.HavePrefix(idents.head, "_define") THEN
        Wr.PutText(wr, idents.head & "(\"" & libname & "\")\n");
      END;
    END;
  END DoLine;

PROCEDURE Main() =
  VAR
    args := Args.CommandLine();
    rd := FileRd.Open(args[0]);
    targ := FileWr.Open(args[1]);
    libname := args[2];
    line: TEXT;
  BEGIN
    TRY
      LOOP
        line := Rd.GetLine(rd);
        IF Text.Length(line) # 0 AND Text.GetChar(line, 0) # '%' THEN
          (* Wr.PutText(stderr, "line = " & line & "\n"); *)
          DoLine(targ, libname,
                 NEW(TextReader.T).init(line).shatter("(), \t", "", TRUE));
        END;
      END;
    EXCEPT Rd.EndOfFile =>
    END;
    Wr.Close(targ);
    Rd.Close(rd);
  END Main;

BEGIN
  Main();
END Main.
