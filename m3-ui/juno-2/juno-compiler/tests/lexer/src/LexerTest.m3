(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Mar 21 13:48:53 PST 1994 by heydon                   *)

(* This program implements a simple test of the Juno lexer. The command-line
   syntax of the program is:

       LexerTest [infile]

   By default, the program reads from standard input. If you specify "infile",
   it will read from that file. The program breaks each line of input into a
   stream of tokens, and echos its understanding of those tokens back to the
   standard output. Each token is written in the following way: "kind(value)".
   The "kind" indicates the type of the token, such as "Id" for an identifier,
   "Op" for an operator, etc. The "value" indicates the associated value, for
   example the actual name of the identifier, value of a number, or operator.
*)

MODULE LexerTest EXPORTS Main;

IMPORT JunoLex, JunoToken;
IMPORT Rd, Wr, TextRd, Thread, Params, Process, FileRd, OSError;
FROM Stdio IMPORT stdin, stdout, stderr;

<* FATAL Rd.Failure, Wr.Failure, Thread.Alerted *>
<* FATAL Rd.EndOfFile *>

VAR
  s: JunoLex.Stream;			 (* stream of tokens *)
  t: JunoToken.T;			 (* current token *)
  line: TEXT;				 (* current input line *)
  file: TEXT;				 (* input file name *)
  in := stdin;				 (* input stream *)
  rd := NEW(TextRd.T);

EXCEPTION BadFormat;

BEGIN
  (* Parse command-line *)
  TRY
    CASE Params.Count OF
      1 => (* SKIP -- read from stdin *)
    | 2 => in := FileRd.Open(Params.Get(1))
    ELSE RAISE BadFormat
    END
  EXCEPT
    BadFormat =>
      Wr.PutText(stderr, "Usage: LexerTest [infile]\n");
      Process.Exit(1);
   | OSError.E =>
      Wr.PutText(stderr, "Unable to open '" & file & "' for reading.\n");
      Process.Exit(2);
  END;

  (* Read tokens *)
  Wr.PutText(stdout, "Each line you enter will be parsed into tokens.\n");
  Wr.PutText(stdout, "\n> "); Wr.Flush(stdout);
  WHILE NOT Rd.EOF(in) DO
    line := Rd.GetLine(in);
    IF NOT Rd.Intermittent(in) THEN
      Wr.PutText(stdout, line & "\n"); Wr.Flush(stdout)
    END;
    s := JunoLex.New(rd.init(line));
    TRY
      LOOP
        t := s.next();
        Wr.PutText(stdout, JunoToken.ToName(t) & "\n"); Wr.Flush(stdout);
        IF t.kind = JunoToken.Kind.EndMarker THEN EXIT END
      END
    EXCEPT
      JunoLex.Error (e) =>
        Wr.PutText(stdout, "Bad Token! (" & JunoLex.ErrorText(e.kind) & ")\n");
        Wr.PutText(stdout, "-> Read = <" & e.initialChars & ">\n");
        Wr.PutText(stdout, "-> Left = <");
        WHILE NOT Rd.EOF(rd) DO Wr.PutChar(stdout, Rd.GetChar(rd)) END;
        Wr.PutText(stdout, ">\n");
        Wr.Flush(stdout)
    END;
    EVAL JunoLex.Close(s);
    Wr.PutText(stdout, "\n> "); Wr.Flush(stdout);
  END;
  IF in # stdin THEN Rd.Close(in) END;
  IF NOT Rd.Intermittent(in) THEN Wr.PutText(stdout, "^D") END;
  Wr.PutChar(stdout, '\n'); Wr.Flush(stdout)
END LexerTest.
