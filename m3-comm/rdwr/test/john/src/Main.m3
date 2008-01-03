(* John Polstra <jdp@polstra.com> wrote this on January 3, 1996. *)

MODULE Main;

IMPORT FileRd, Fmt, IO, OSError, Params, Process, Rd, RdWrPipe, Stdio,
  Text, Thread, Wr;

<* FATAL Rd.Failure, Thread.Alerted, Wr.Failure *>

TYPE
  CopyClosure = Thread.Closure OBJECT
      rd: Rd.T;
      wr: Wr.T;
      name: TEXT;
      delay: LONGREAL;
    OVERRIDES
      apply := DoCopy;
    END;

CONST
  SourceName = "TestData";

PROCEDURE DoCopy(cl: CopyClosure): REFANY =
  VAR
    line: TEXT;
    numBytes, numLines: CARDINAL := 0;
  BEGIN
    IO.Put(cl.name & " begins\n", Stdio.stderr);

    LOOP
      IF cl.delay > 0.0d0 THEN Thread.Pause(cl.delay) END;
      TRY
	line := Rd.GetLine(cl.rd);
      EXCEPT Rd.EndOfFile =>
	EXIT;
      END;
      Wr.PutText(cl.wr, line & "\n");
      INC(numLines);
      INC(numBytes, Text.Length(line) + 1);
    END;
    IO.Put(cl.name & " gets EOF after copying " &
      Fmt.Int(numLines) & " lines (" & Fmt.Int(numBytes) & " bytes)\n",
      Stdio.stderr);

    Rd.Close(cl.rd);
    Wr.Close(cl.wr);
    IO.Put(cl.name & " ends\n", Stdio.stderr);
    RETURN NIL;
  END DoCopy;

VAR
  source: Rd.T;
  dest: Wr.T;
  pipeRd: Rd.T;
  pipeWr: Wr.T;
  toPipe, fromPipe: Thread.T;
  rdDelay, wrDelay: LONGREAL;
  arg: TEXT;

BEGIN
  IF Params.Count = 2 THEN arg := Params.Get(1) ELSE arg := "x" END;
  IF Text.Length(arg) # 1 THEN arg := "x" END;

  CASE Text.GetChar(arg, 0) OF
  | '1' => wrDelay := 0.0d0;  rdDelay := 0.0d0;
  | '2' => wrDelay := 0.1d0;  rdDelay := 0.0d0;
  | '3' => wrDelay := 0.0d0;  rdDelay := 0.1d0;
  | '4' => wrDelay := 0.1d0;  rdDelay := 0.1d0;
  ELSE
    IO.Put("Usage: " & Params.Get(0) & " mode\n", Stdio.stderr);
    IO.Put("  mode 1: write fast, read fast\n", Stdio.stderr);
    IO.Put("  mode 2: write slow, read fast\n", Stdio.stderr);
    IO.Put("  mode 3: write fast, read slow\n", Stdio.stderr);
    IO.Put("  mode 4: write slow, read slow\n", Stdio.stderr);
    Process.Exit(1);
  END;

  TRY
    source := FileRd.Open(SourceName);
  EXCEPT OSError.E =>
    IO.Put("Please create a text file \"" & SourceName &
      "\" in your current working directory.\n", Stdio.stderr);
    IO.Put("I suggest using the command \"head -100 /etc/termcap >" &
      SourceName & "\".\n", Stdio.stderr);
    Process.Exit(1);
  END;

  dest := Stdio.stdout;
  RdWrPipe.New(pipeRd, pipeWr);

  fromPipe := Thread.Fork(NEW(CopyClosure, rd := pipeRd, wr := dest,
    name := "FROM pipe", delay := rdDelay));
  toPipe := Thread.Fork(NEW(CopyClosure, rd := source, wr := pipeWr,
    name := "TO   pipe", delay := wrDelay));

  EVAL Thread.Join(fromPipe);
END Main.
