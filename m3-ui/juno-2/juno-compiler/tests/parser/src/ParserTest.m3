(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Aug 16 10:56:30 PDT 1993 by heydon                   *)

(* This program implements a simple test of the Juno parser. The command-line
   syntax of the program is:

       ParserTest [-exprs|-cmds|-mod] [infile]

   By default, the program reads from standard input. If you specify "infile",
   it will read from that file. Also by default, it expects you to be entering
   Juno expressions. You can use the "-cmds" or "-mod" switches to tell the
   program you will be entering Juno commands or Juno modules, respectively.
*)

MODULE ParserTest EXPORTS Main;

IMPORT JunoAST, JunoParse, JunoLex, JunoToken, JunoUnparse;
IMPORT Rd, Wr, TextRd, FileRd, Params, Process, OSError, Text;
FROM Stdio IMPORT stdin, stdout, stderr;
FROM Thread IMPORT Alerted;

<* FATAL Rd.Failure, Wr.Failure, Alerted *>
<* FATAL Rd.EndOfFile *>

TYPE
  Kind = {Mod, Cmds, Exprs};

CONST
  Width = 40;

VAR
  block: JunoAST.Block;			 (* current block *)
  cmd: JunoAST.Cmd;			 (* current command *)
  expr: JunoAST.Expr;			 (* current expr *)
  line: TEXT;				 (* current input line *)
  file: TEXT;				 (* input file name *)
  in := stdin;				 (* input stream *)
  kind := Kind.Exprs;			 (* what to parse *)
  tokenCnt: CARDINAL;			 (* total number of tokens parsed *)
  arg := 1;				 (* command-line argument index *)

VAR (* for parsing modules only *)
  txt: TEXT;				 (* module text *)
  ip: JunoParse.IterativeParse := NIL;
  error := FALSE;			 (* did a parse error occur? *)

EXCEPTION BadFormat;
EXCEPTION UnhandledParseError;		 (* should not be raised *)

<* FATAL UnhandledParseError *>

BEGIN
  (* Parse command-line *)
  TRY
    IF arg < Params.Count AND Text.GetChar(Params.Get(arg), 0) = '-' THEN
      IF    Text.Equal("-mod",  Params.Get(arg)) THEN kind := Kind.Mod
      ELSIF Text.Equal("-cmds",  Params.Get(arg)) THEN kind := Kind.Cmds
      ELSIF Text.Equal("-exprs", Params.Get(arg)) THEN kind := Kind.Exprs
      ELSE RAISE BadFormat
      END;
      INC(arg)
    END;
    IF arg < Params.Count THEN in := FileRd.Open(Params.Get(arg)) END
  EXCEPT
    BadFormat =>
      Wr.PutText(stderr, "Usage: ParserTest [-exprs|-cmds|-mod] [infile]\n");
      Process.Exit(1);
  | OSError.E =>
      Wr.PutText(stderr, "Unable to open '" & file & "' for reading.\n");
      Process.Exit(2);
  END;

  (* Read AST's *)
  IF kind = Kind.Mod
    THEN Wr.PutText(stdout, "The entire file you type")
    ELSE Wr.PutText(stdout, "Each line you enter")
  END;
  Wr.PutText(stdout, " will be parsed as ");
  CASE kind OF
  | Kind.Mod  => Wr.PutText(stdout, "a module")
  | Kind.Cmds  => Wr.PutText(stdout, "a command")
  | Kind.Exprs => Wr.PutText(stdout, "an expression")
  END;
  Wr.PutText(stdout, ".\n\n");
  Wr.Flush(stdout);
  IF kind = Kind.Mod THEN
    txt := Rd.GetText(in, LAST(CARDINAL));
    IF NOT Rd.Intermittent(in) THEN Wr.PutText(stdout, txt & "^D") END;
    Wr.PutChar(stdout, '\n')
  ELSE
    Wr.PutText(stdout, "> ")
  END;
  Wr.Flush(stdout);
  LOOP
    TRY
      IF kind # Kind.Mod THEN
        IF Rd.EOF(in) THEN EXIT END;
        line := Rd.GetLine(in);
        IF NOT Rd.Intermittent(in) THEN
          Wr.PutText(stdout, line & "\n");
          Wr.Flush(stdout)
        END
      ELSIF ip = NIL THEN
        ip := JunoParse.StartIterativeParse(TextRd.New(txt))
      END;

      (* parse *)
      CASE kind OF
      | Kind.Mod  => JunoParse.Block(ip, block, tokenCnt)
      | Kind.Cmds  => JunoParse.Command(TextRd.New(line), cmd, tokenCnt);
      | Kind.Exprs => JunoParse.Expression(TextRd.New(line), expr, tokenCnt);
      END
    EXCEPT
    | JunoLex.Error (err) =>
	Wr.PutText(stdout, "*** Lex error: " & JunoLex.ErrorText(err.kind));
        Wr.PutChar(stdout, '\n'); Wr.Flush(stdout);
        error := TRUE
    | JunoParse.Error (err) =>
	VAR t := err.found; BEGIN
          Wr.PutText(stdout, "*** Parse error: ");
          Wr.PutText(stdout, "Found '" &  JunoToken.ToText(t) & "'");
          IF err.expected # JunoToken.Kind.Unknown THEN
            VAR e := NEW(JunoToken.T, kind := err.expected); BEGIN
              Wr.PutText(stdout, "; Expected '" & JunoToken.ToText(e) & "'");
            END
          END;
          Wr.PutChar(stdout, '\n'); Wr.Flush(stdout)
	END;
        error := TRUE
    | Rd.Failure =>
        Wr.PutText(stderr, "*** Read failure!\n");
        Wr.Flush(stderr);
        EXIT
    END;

    (* unparse *)
    CASE kind OF
    | Kind.Mod  =>
        IF block = NIL THEN JunoParse.FinishIterativeParse(ip); EXIT END;
        JunoUnparse.Block(stdout, block, tokenCnt, indent := 0, width := Width)
    | Kind.Cmds  => JunoUnparse.Cmd(stdout, cmd, tokenCnt, width := Width);
    | Kind.Exprs => JunoUnparse.Expr(stdout, expr, tokenCnt, width := Width);
    END;
    Wr.PutText(stdout, "\n\n");
    IF kind = Kind.Mod
      THEN IF error THEN Wr.Flush(stdout); EXIT END
      ELSE Wr.PutText(stdout, "> ")
    END;
    Wr.Flush(stdout);
  END;
  IF in # stdin THEN Rd.Close(in) END;
  IF kind # Kind.Mod THEN
    IF NOT Rd.Intermittent(in) THEN Wr.PutText(stdout, "^D") END;
    Wr.PutChar(stdout, '\n')
  END;
  Wr.Flush(stdout)
END ParserTest.
