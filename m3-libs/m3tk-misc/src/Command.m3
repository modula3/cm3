(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)
(* *)
(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE Command;

IMPORT Text, TextExtras, ASCII, CITextRefTbl, Fmt, Convert, Thread;
IMPORT Wr, Rd, RdExtras, FileRd, FileWr, TextRd, Stdio, Err, OSError;

<* FATAL Thread.Alerted, Wr.Failure *>

TYPE
  Command = OBJECT
    next: Command;
    closure: Closure;
    name, help: Text.T;
  END;

CONST
  PromptTail = "> ";

VAR
  commandNames_g := NEW(CITextRefTbl.Default).init();
  commands_g: Command := NIL;
  prompt_g := "--" & PromptTail;

TYPE OpenMode = {Read, Write};

PROCEDURE SortedAdd(new: Command; VAR list: Command) RAISES {}=
  BEGIN
    IF (list = NIL) OR (Text.Compare(new.name, list.name) < 0) THEN
      new.next := list;
      list := new;
    ELSE
      SortedAdd(new, list.next);
    END;
  END SortedAdd;


(*PUBLIC*)
PROCEDURE BindClosure(name: Text.T; c: Closure; help: Text.T := NIL) RAISES {}=
  VAR
    command: Command;
    l, index, lindex: CARDINAL;
    shortFormArray: REF ARRAY OF CHAR;
    shortForm: TEXT;
    ch: CHAR;
  BEGIN
    l := Text.Length(name);
    shortFormArray := NEW(REF ARRAY OF CHAR, l);
    index := 0; lindex := 0;
    WHILE index < l DO
      ch := Text.GetChar(name, index);
      IF ch IN ASCII.Uppers THEN
        shortFormArray[lindex] := ASCII.Lower[ch];
        INC(lindex);
      END;
      INC(index);
    END; (* while *)
    shortForm := Text.FromChars(SUBARRAY(shortFormArray^, 0, lindex));

    command := NEW(Command);
    command.closure := c;
    command.name := name;
    IF help = NIL THEN help := "" END;
    command.help := help;
    IF NOT commandNames_g.put(name, command) THEN
      SortedAdd(command, commands_g);
      IF Text.Length(shortForm) > 0 AND NOT Text.Equal(name, shortForm) THEN
        IF NOT commandNames_g.put(shortForm, command) THEN
        ELSE
          Err.Print(Fmt.F("Duplicated (short form of) command: \'%s\'\n",
	      shortForm),
              Err.Severity.Warning);
        END; (* if *)
      END; (* if *)
    ELSE
      Err.Print(Fmt.F("Duplicated command: \'%s\'\n", name),
          Err.Severity.Warning);
    END;
  END BindClosure;

PROCEDURE SetPrompt(p: TEXT) RAISES {}=
  BEGIN
    prompt_g := p & PromptTail;
  END SetPrompt;


TYPE
  SimpleClosure = Closure OBJECT
    proc: PROCEDURE() RAISES {}
  OVERRIDES
    apply := CallProc;
  END;


PROCEDURE CallProc(sc: SimpleClosure) RAISES {}=
  BEGIN
    sc.proc();
  END CallProc;


(* PUBLIC *)
PROCEDURE Bind(
    name: Text.T;
    proc: PROCEDURE() RAISES{};
    help: Text.T := NIL)
    RAISES {}=
  VAR
    sc: SimpleClosure;
  BEGIN
    sc := NEW(SimpleClosure);
    sc.proc := proc;
    BindClosure(name, sc, help);
  END Bind;


VAR
  quit_g: BOOLEAN;


PROCEDURE Help() RAISES {}=
  VAR
    command := commands_g;
  BEGIN
    IF command # NIL THEN
      WHILE command # NIL DO
        PutF("%-24s %s\n", command.name, command.help);
        command := command.next;
      END;
    ELSE
      Put("No commands available!\n");
    END;
  END Help;


PROCEDURE Quit() RAISES {}=
  BEGIN
    quit_g := TRUE;
  END Quit;


TYPE
  StreamStack = OBJECT
    name: TEXT;
    next: StreamStack := NIL;
    rd: Rd.T := NIL; wr: Wr.T := NIL;
  END;


VAR
  inStack_g, logStack_g: StreamStack := NIL;
  dontLog_g := FALSE;


PROCEDURE Open(
    name: Text.T;
    mode: OpenMode;
    VAR ss: StreamStack)
    RAISES {}=
  VAR
    new: StreamStack;
    
  BEGIN
    TRY
      new := NEW(StreamStack, next := ss, name := name);
      ss := new;
      IF mode =OpenMode.Read THEN
        ss.rd := FileRd.Open(name)
      ELSE
        ss.wr := FileWr.Open(name)
      END;             
    EXCEPT
    | OSError.E =>
        PutF("Open failed on '%s'\n", name);
    END;
  END Open;


PROCEDURE Close(VAR ss: StreamStack) RAISES {}=
  BEGIN
    TRY
      IF ss.rd # NIL THEN Rd.Close(ss.rd); END;
      IF ss.wr # NIL THEN Wr.Close(ss.wr); END; 
      ss := ss.next;
    EXCEPT
    | Rd.Failure, Wr.Failure =>
        PutF("Close failed on '%s'\n", ss.name);
    END; (* try *)
  END Close;


PROCEDURE Indirect() RAISES {}=
  VAR
    arg: Text.T;
  BEGIN
    dontLog_g := TRUE;
    IF GetArg(arg) THEN Open(arg, OpenMode.Read, inStack_g) END;
  END Indirect;


PROCEDURE Log() RAISES {}=
  VAR
    arg: Text.T;
  BEGIN
    dontLog_g := TRUE;
    IF GetArg(arg) THEN Open(arg, OpenMode.Write, logStack_g) END;
  END Log;


PROCEDURE EndLog() RAISES {}=
  BEGIN
    dontLog_g := TRUE;
    IF logStack_g = NIL THEN
      Put("Not logging\n");
    ELSE
      WITH name = logStack_g.name DO
        IF name # NIL THEN
          PutF("Closing log \'%s\'\n", name);
        ELSE
          Put("Closing log\n");
        END;
      END;
      Close(logStack_g);
    END;
  END EndLog;

PROCEDURE Last() RAISES {}=
  BEGIN
    IF lastLine_g # NIL THEN
      WITH new = NEW(StreamStack, next := inStack_g,
                      rd := TextRd.New(lastLine_g), name := "") DO
        inStack_g := new;
      END;
    END; (* if *)
  END Last;

PROCEDURE GetLine(): Text.T RAISES {Rd.Failure, Thread.Alerted}=
  BEGIN
    LOOP
      VAR
        stdIn := inStack_g = NIL;
        in: Rd.T;
      BEGIN
        IF stdIn THEN in := Stdio.stdin ELSE in := inStack_g.rd END;
        TRY
          WITH text = RdExtras.GetText(
              in, terminate := ASCII.Set{'\n', ';'}, unget := FALSE) DO
             (* reflect input, if not from Stdio.in *)
            IF NOT stdIn THEN PutF("%s\n", text) END;
            RETURN text;
          END;
        EXCEPT
        | Rd.EndOfFile =>
            IF stdIn THEN
              quit_g := TRUE;
              RETURN "";
            ELSE
              Close(inStack_g);
            END;
        END;
      END;
    END;
  END GetLine;

VAR
  line_g, lastLine_g: Text.T := NIL;
  linePos_g: CARDINAL := 0;

(*PUBLIC*)
PROCEDURE Argument(VAR arg: Text.T): BOOLEAN RAISES {}=
  TYPE
    State = {Initial, InNormalArg, InQuotedArg};
  VAR
    length := Text.Length(line_g);
    state := State.Initial;
    start: CARDINAL;
  BEGIN
    LOOP
      IF linePos_g >= length THEN
        IF state = State.Initial THEN RETURN FALSE ELSE EXIT END;
      ELSE
        WITH ch = Text.GetChar(line_g, linePos_g) DO
          IF ch IN ASCII.Spaces THEN
            IF state = State.InNormalArg THEN EXIT END;
            (* loop *)
          ELSIF ch = '\"' THEN
            IF state = State.Initial THEN
              start := linePos_g + 1;
              state := State.InQuotedArg;
            ELSE
              EXIT;
            END;
          ELSE
            IF state = State.Initial THEN
              start := linePos_g;
              state := State.InNormalArg;
            END;
          END;
          INC(linePos_g);
        END;
      END;
    END;
    arg := TextExtras.Extract(line_g, start, linePos_g);
    IF state = State.InQuotedArg THEN INC(linePos_g) END;
    RETURN TRUE;
  END Argument;


(*PUBLIC*)
PROCEDURE CardinalArgument(VAR card: CARDINAL): BOOLEAN RAISES {}=
  VAR
    arg: Text.T;
    used, argl: INTEGER;
    t: REF ARRAY OF CHAR;
  BEGIN
    IF Argument(arg) THEN
      argl := Text.Length(arg);
      t := NEW(REF ARRAY OF CHAR, argl);
      Text.SetChars(t^, arg);
      card := Convert.ToUnsigned(t^, used);
      RETURN used = argl;
    ELSE
      RETURN FALSE;
    END;
  END CardinalArgument;


(*PUBLIC*)
PROCEDURE IntegerArgument(VAR integer: INTEGER): BOOLEAN RAISES {}=
  VAR
    arg: Text.T;
    used, argl: INTEGER;
    t: REF ARRAY OF CHAR;
  BEGIN
    IF Argument(arg) THEN
      argl := Text.Length(arg);
      t := NEW(REF ARRAY OF CHAR, argl);
      integer := Convert.ToInt(t^, used);
      RETURN used = argl;
    ELSE
      RETURN FALSE;
    END;
  END IntegerArgument;


(*PUBLIC*)
PROCEDURE RestOfLine(): Text.T RAISES {}=
  BEGIN
    RETURN TextExtras.Extract(line_g, linePos_g, Text.Length(line_g));
  END RestOfLine;


PROCEDURE LogLine() RAISES {}=
  VAR
    log := logStack_g;
  BEGIN
    IF log # NIL AND NOT dontLog_g THEN
      WITH line = Fmt.F("%s\n", line_g) DO
        TRY
          WHILE log # NIL DO Wr.PutText(log.wr, line); log := log.next END;
        EXCEPT
        | Wr.Failure => 
            PutF("Error writing to log file '%s'", log.name);
        END; (* try *)
      END;
    END;
  END LogLine;


PROCEDURE TidyUp() RAISES {}=
  BEGIN
    WHILE logStack_g # NIL DO Close(logStack_g) END;
    WHILE inStack_g # NIL DO Close(inStack_g) END;
    Wr.Flush(Stdio.stdout);
  END TidyUp;


(*PUBLIC*)
PROCEDURE Interact(s: Rd.T := NIL) RAISES {Rd.Failure, Wr.Failure}=
  VAR
    t: Text.T;
    ref: REFANY;
    command: Command;
  BEGIN
    quit_g := FALSE;
    IF s # NIL THEN inStack_g := NEW(StreamStack, rd := s); END;
    REPEAT
      Put(prompt_g);
      Wr.Flush(Stdio.stdout);
      lastLine_g := line_g;
      line_g := GetLine();
      linePos_g := 0;
      dontLog_g := FALSE;
      IF Argument(t) THEN
        IF commandNames_g.get(t, ref) THEN
          command := NARROW(ref, Command);
          command.closure.apply();
          LogLine();
        ELSE
          Put("Bad command: \'?\' to list commands\n");
        END;
      ELSE
        (* no command *)
      END;
    UNTIL quit_g;
    TidyUp();
  END Interact;


(*PUBLIC*)
PROCEDURE GetArg(VAR a: Text.T): BOOLEAN RAISES {}=
  BEGIN
    IF Argument(a) THEN RETURN TRUE; END;
    Put("Bad args\n");
    RETURN FALSE;
  END GetArg;


(*PUBLIC*)
PROCEDURE CardGetArg(VAR card: CARDINAL): BOOLEAN RAISES {}=
  BEGIN
    IF CardinalArgument(card) THEN RETURN TRUE; END;
    Put("Bad args\n");
    RETURN FALSE;
  END CardGetArg;


(*PUBLIC*)
PROCEDURE IntGetArg(VAR int: INTEGER): BOOLEAN RAISES {}=
  BEGIN
    IF IntegerArgument(int) THEN RETURN TRUE; END;
    Put("Bad args\n");
    RETURN FALSE;
  END IntGetArg;


(*PUBLIC*)
PROCEDURE Put(t: Text.T) RAISES {}=
  BEGIN
    Wr.PutText(Stdio.stdout, t); Wr.Flush(Stdio.stdout);
  END Put;


(*PUBLIC*)
PROCEDURE PutF(fmt: Text.T; t1, t2, t3, t4, t5: Text.T := NIL) RAISES {}=
  BEGIN
    Wr.PutText(Stdio.stdout, Fmt.F(fmt, t1, t2, t3, t4, t5));
    Wr.Flush(Stdio.stdout);
  END PutF;


(*PUBLIC*)
PROCEDURE PutFN(fmt: Text.T; READONLY array: ARRAY OF TEXT) RAISES {}=
  BEGIN
    Wr.PutText(Stdio.stdout, Fmt.FN(fmt, array));
    Wr.Flush(Stdio.stdout);
  END PutFN;

BEGIN
  Bind("?", Help, "give help information");
  Bind("Quit", Quit, "quit the program");
  Bind("Help", Help, "give help information");
  Bind("@", Indirect, "read commands from named file");
  Bind("Last", Last, "redo last command");
  Bind("StartLog", Log, "save all commands in named log file");
  Bind("EndLog", EndLog, "stop logging");
END Command.
