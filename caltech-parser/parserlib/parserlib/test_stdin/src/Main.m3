MODULE Main;
IMPORT Wr, Fmt, Thread;
IMPORT SeekRd, Rd;
IMPORT Text, Scan;
IMPORT Stdio;
FROM Stdio IMPORT stdout;
IMPORT FloatMode, Lex;
<* FATAL Wr.Failure, Thread.Alerted, Rd.Failure, FloatMode.Trap, Lex.Error *>

PROCEDURE TestStdin()= 
  VAR
    rd: Rd.T;
    line: TEXT;
    i: INTEGER;
    c: CHAR;
    seen := SET OF CHAR{};
  BEGIN
    LOOP
      rd := SeekRd.Stdin();
      TRY
        LOOP
          line := Rd.GetLine(rd);
          Wr.PutText(stdout, "got line: " & line & "\n");
          IF Text.Equal(line,"") THEN
          ELSIF Text.GetChar(line, 0) = 's' THEN
            c := Text.GetChar(line, 1);
            IF c IN seen THEN
              Wr.PutText(stdout, "skipping\n");
            ELSE
              seen := seen + SET OF CHAR{c};
              i := Scan.Int(Text.Sub(line, 2, Text.Length(line)-2));
              Wr.PutText(stdout, "will seek to: " & Fmt.Int(i) & "\n");
              Rd.Seek(rd, i);
            END;
          END;
          IF Text.Equal(line, "d") THEN
            SeekRd.DiscardPrevious(rd);
            Wr.PutText(stdout, "discarding previous: \n");
          END;
          Wr.Flush(stdout);
        END;
      EXCEPT
      | Rd.EndOfFile =>
        Wr.PutText(stdout, "end of file.\n");
        Wr.Flush(stdout);
      END;
    END;
  END TestStdin;

BEGIN
  TestStdin();
END Main.
