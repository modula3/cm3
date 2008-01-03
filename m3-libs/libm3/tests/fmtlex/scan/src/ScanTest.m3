(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sun Feb 27 19:04:51 PST 1994 by heydon                   *)
(*      modified on Fri Feb 25 14:45:27 PST 1994 by kalsow                   *)

MODULE Main;

(* SYNTAX: ScanTest [infile]

   This program tests the "Scan" interface by reading lines from the file named
   "infile" (or standard input if no filename argument is specified) and
   writing parsed values to standard output. The first character of each line
   indicates which procedure in Lex to test:

|    'i', 'I' => Lex.Int        'r' => Lex.Real
|    'u', 'U' => Lex.Unsigned   'l' => Lex.LongReal
|    'b'      => Lex.Bool       'e' => Lex.Extended

   The program consumes the first character of each input line, and then
   converts the remainder of the line. There are some exceptions:

|    'I': skips blanks, reads base-10 base, converts remainder of line w/ base
|    'U': skips blanks, reads base-10 base, converts remainder of line w/ base

   For the  'I' and 'U' variants, the base must be between 2 and 16 (by
   default, the bases in the 'i' and 'u' cases are 10 and 16, respectively).
   The program prints the converted value followed by the unconsumed
   characters on the line.

   Blank lines are ignored. Comments start at a '#' character. All whitespace
   immediately before the first '#' character and everything from that '#'
   character to the end of the line is ignored. A line starting with 'd'
   should be followed by an integer; this sets the number of digits of
   precision to print when formatting real-valued results. *)

IMPORT Scan, Lex, Fmt, Params, OSError, TextWr;
IMPORT Wr, Rd, FileRd, Text, TextRd, Thread, FloatMode, Word;
FROM Stdio IMPORT stdin, stdout;

<* FATAL Thread.Alerted, Rd.EndOfFile, Rd.Failure, Wr.Failure, OSError.E *>

CONST FlagName = ARRAY OF TEXT {
  "Invalid Operation", "Inexact Operation", "Floating-point Overflow",
  "Floating-point Underflow", "Division by Zero", "Integer Overflow",
  "Integer Division by Zero"};

CONST
  Prefix = "-> ";
  FloatStyle = Fmt.Style.Auto;

TYPE
  Kind = {Bool, Int, Unsigned, Real, LongReal, Extended, Digits};

CONST
  CommentChar = '#';

PROCEDURE GetBase (VAR (*INOUT*) line: TEXT): INTEGER =
  <*FATAL ANY*>
  VAR
    rd := TextRd.New (line);
    base := Lex.Int (rd);
    wr := TextWr.New ();
  BEGIN
    WHILE NOT Rd.EOF (rd) DO Wr.PutChar (wr, Rd.GetChar (rd)); END;
    line := TextWr.ToText (wr);
    RETURN base;
  END GetBase;

VAR
  line: TEXT;
  kind: Kind;
  base: CARDINAL;
  signed: INTEGER;
  unsigned: Word.T;
  inFile: Rd.T;
  digits := 20;
  cmd: CHAR;

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
      TRY
        cmd := Text.GetChar (line, 0);
        line := Text.Sub (line, 1);
	CASE cmd OF <* NOWARN *>
	| 'b' => kind := Kind.Bool
	| 'i' => kind := Kind.Int; base := 10;
	| 'I' => kind := Kind.Int; base := GetBase (line);
	| 'u' => kind := Kind.Unsigned; base := 16;
	| 'U' => kind := Kind.Unsigned; base := GetBase (line);
	| 'r' => kind := Kind.Real
	| 'l' => kind := Kind.LongReal
	| 'e' => kind := Kind.Extended
        | 'd' => kind := Kind.Digits
	END;
	CASE kind OF
	| Kind.Bool =>
	    Wr.PutText(stdout, Prefix & Fmt.Bool(Scan.Bool(line)))
	| Kind.Int =>
	    Wr.PutText(stdout, Prefix);
            signed := Scan.Int(line, base);
            IF signed < 0 THEN
              Wr.PutChar(stdout, '-');
              signed := Word.Plus(Word.Not(signed), 1)
            END;
            IF base # 10 THEN Wr.PutText(stdout, Fmt.Int(base) & "_") END;
            Wr.PutText(stdout, Fmt.Unsigned(signed, base))
	| Kind.Unsigned =>
	    Wr.PutText(stdout, Prefix);
            unsigned := Scan.Unsigned(line, base);
            IF base # 16 THEN Wr.PutText(stdout, Fmt.Int(base) & "_") END;
            Wr.PutText(stdout, Fmt.Unsigned(unsigned, base))
	| Kind.Real =>
	    Wr.PutText(stdout, Prefix & Fmt.Real(Scan.Real(line),
              prec := digits, style := FloatStyle, literal := TRUE));
	| Kind.LongReal =>
	    Wr.PutText(stdout, Prefix & Fmt.LongReal(Scan.LongReal(line),
              prec := digits, style := FloatStyle, literal := TRUE));
	| Kind.Extended =>
            Wr.PutText(stdout, Prefix & Fmt.Extended(Scan.Extended(line),
              prec := digits, style := FloatStyle, literal := TRUE));
        | Kind.Digits =>
            digits := Scan.Int(line);
            Wr.PutText(stdout, Prefix & "Digits := "& Fmt.Int(digits));
	END
      EXCEPT
	Lex.Error =>
          Wr.PutText(stdout, "** Raised \"Lex.Error\"");
      | FloatMode.Trap (flag) =>
          Wr.PutText(stdout, "** " & FlagName[ORD(flag)]);
      END;
      Wr.PutText(stdout, "\n\n");
      Wr.Flush(stdout)
    END
  END;
  IF inFile # stdin THEN Rd.Close(inFile) END
END Main.
