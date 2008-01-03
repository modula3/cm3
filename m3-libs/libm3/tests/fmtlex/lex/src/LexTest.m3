(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Mar 15 11:36:52 PST 1994 by heydon                   *)

MODULE LexTest EXPORTS Main;

(* SYNTAX: LexTest [infile]

   This program tests the "Lex" interface by reading lines from the file named
   "infile" (or standard input if no filename argument is specified) and
   writing parsed values to standard output. The first character of each line
   indicates which procedure in Lex to test:

|    's' => Lex.Scan   'i', 'I' => Lex.Int        'r' => Lex.Real
|    'm' => Lex.Match  'u', 'U' => Lex.Unsigned   'l' => Lex.LongReal
|    'b' => Lex.Bool                              'e' => Lex.Extended

   The program consumes the first character of each input line, and then
   converts the remainder of the line. There are some exceptions:

|    's': first skips blanks, then scans remainder of line
|    'm': skips blanks, scans text, skips blanks, matches remainder of line
|    'I': skips blanks, reads base-10 base, converts remainder of line w/ base
|    'U': skips blanks, reads base-10 base, converts remainder of line w/ base

   For the  'I' and 'U' variants, the base must be between 2 and 16 (by
   default, the bases in the 'i' and 'u' cases are 10 and 16, respectively).
   The program prints the converted value followed by the unconsumed
   characters on the line.

   Blank lines are ignored. Comments start at a '#' character. All whitespace
   immediately before the first '#' character and everything from that '#'
   character to the end of the line is ignored. *)

IMPORT Lex, Fmt, Params, OSError;
IMPORT Wr, Rd, FileRd, Text, TextRd, Thread, FloatMode, Word;
FROM Stdio IMPORT stdin, stdout;

<* FATAL Thread.Alerted, Rd.EndOfFile, Rd.Failure, Wr.Failure, OSError.E *>

CONST FlagName = ARRAY OF TEXT {
  "Invalid Operation", "Inexact Operation", "Floating-point Overflow",
  "Floating-point Underflow", "Division by Zero", "Integer Overflow",
  "Integer Division by Zero"};

CONST Prefix = "-> ";

EXCEPTION Error(TEXT);

PROCEDURE WriteUnread(rd: Rd.T) =
  BEGIN
    Wr.PutText(stdout, Prefix & "\"");
    Wr.PutText(stdout, Rd.GetText(rd, LAST(CARDINAL)) & "\"\n\n");
    Wr.Flush(stdout)
  END WriteUnread;

TYPE
  Kind = {Scan, Match, Bool, Int, Unsigned, Real, LongReal, Extended};

CONST
  CommentChar = '#';

VAR
  line: TEXT;
  kind: Kind;
  base: CARDINAL;
  signed: INTEGER;
  unsigned: Word.T;
  rd := NEW(TextRd.T);
  inFile: Rd.T;

BEGIN
  (* open file if one is specified *)
  IF Params.Count > 1
    THEN inFile := FileRd.Open(Params.Get(1))
    ELSE inFile := stdin
  END;

  WHILE NOT Rd.EOF(inFile) DO
    line := Rd.GetLine(inFile);
    IF inFile # stdin AND Text.Length(line) > 0
       AND Text.GetChar(line, 0) # CommentChar THEN
      Wr.PutText(stdout, line & "\n")
    END;
    (* remove optional trailing comment *)
    VAR comment := Text.FindChar(line, CommentChar); BEGIN
      IF comment # -1 THEN
        REPEAT
          DEC(comment)
        UNTIL comment < 0 OR NOT Text.GetChar(line, comment) IN Lex.Blanks;
        line := Text.Sub(line, 0, comment+1)
      END
    END;
    IF Text.Length(line) > 0 THEN
      EVAL rd.init(line);
      TRY
	CASE Rd.GetChar(rd) OF <* NOWARN *>
	  's' => kind := Kind.Scan
        | 'm' => kind := Kind.Match
	| 'b' => kind := Kind.Bool
	| 'i' => kind := Kind.Int; base := 10;
	| 'I' => kind := Kind.Int; base := Lex.Int(rd)
	| 'u' => kind := Kind.Unsigned; base := 16;
	| 'U' => kind := Kind.Unsigned; base := Lex.Int(rd)
	| 'r' => kind := Kind.Real
	| 'l' => kind := Kind.LongReal
	| 'e' => kind := Kind.Extended
        ELSE RAISE Error("Unrecognized command character")
	END;
	CASE kind OF
	  Kind.Scan =>
            Lex.Skip(rd, cs := Lex.Blanks);
	    Wr.PutText(stdout, Prefix & Lex.Scan(rd, cs := Lex.NonBlanks))
        | Kind.Match =>
            Lex.Skip(rd, cs := Lex.Blanks);
            VAR t := Lex.Scan(rd, cs := Lex.NonBlanks); BEGIN
              Lex.Skip(rd, cs := Lex.Blanks);
              Lex.Match(rd, t);
              Wr.PutText(stdout, Prefix & t)
            END
	| Kind.Bool =>
	    Wr.PutText(stdout, Prefix & Fmt.Bool(Lex.Bool(rd)))
	| Kind.Int =>
            signed := Lex.Int(rd, base);
	    Wr.PutText(stdout, Prefix);
            IF signed < 0 THEN
              Wr.PutChar(stdout, '-');
              signed := Word.Plus(Word.Not(signed), 1)
            END;
            IF base # 10 THEN Wr.PutText(stdout, Fmt.Int(base) & "_") END;
            Wr.PutText(stdout, Fmt.Unsigned(signed, base))
	| Kind.Unsigned =>
            unsigned := Lex.Unsigned(rd, base);
	    Wr.PutText(stdout, Prefix);
            IF base # 16 THEN Wr.PutText(stdout, Fmt.Int(base) & "_") END;
            Wr.PutText(stdout, Fmt.Unsigned(unsigned, base))
	| Kind.Real =>
	    Wr.PutText(stdout, Prefix & Fmt.Real(Lex.Real(rd)))
	| Kind.LongReal =>
	    Wr.PutText(stdout, Prefix & Fmt.LongReal(Lex.LongReal(rd)))
	| Kind.Extended =>
	    Wr.PutText(stdout, Prefix & Fmt.Extended(Lex.Extended(rd)))
	END;
        Wr.PutChar(stdout, '\n');
        WriteUnread(rd);
        Wr.Flush(stdout)
      EXCEPT
	Lex.Error =>
          Wr.PutText(stdout, "** Raised \"Lex.Error\"\n");
          WriteUnread(rd)
      | FloatMode.Trap (flag) =>
          Wr.PutText(stdout, "** " & FlagName[ORD(flag)] & "\n");
          WriteUnread(rd)
      | Error (msg) =>
          Wr.PutText(stdout, "** " & "Error: " & msg & "\n");
          WriteUnread(rd)
      END
    END
  END;
  IF inFile # stdin THEN Rd.Close(inFile) END
END LexTest.
