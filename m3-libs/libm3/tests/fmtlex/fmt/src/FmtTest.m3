(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Mar 24 15:13:20 PST 1994 by heydon                   *)

UNSAFE MODULE FmtTest EXPORTS Main;

(* SYNTAX: FmtTest [infile]

   This program tests the "Fmt" interface by reading lines from the file named
   "infile" (or standard input if no filename argument is specified) and
   writing parsed values to standard output.

   Comments are sequences of characters starting at a '#' character up to and
   including the first newline. Blank lines and comments are ignored.

   The first character of each line indicates which procedure of the Fmt
   interface to test:

|    'i' => Fmt.Int         'r' => Fmt.Real        'p' => Fmt.Pad
|    'u' => Fmt.Unsigned    'l' => Fmt.LongReal    'f' => Fmt.F
|                           'e' => Fmt.Extended    'F' => Fmt.FN

   The program consumes the first character of each input line, then consumes
   arguments remaining on the line depending on the command character. Here
   are the formats of each command line:

|    i[<base>] <value>
|    u[<base>] <value>

   The base is optional, but if it appears, it must immediately follow the
   command character. The base defaults to 10 or 16, respectively. The base is
   parsed with "Lex.Int" (base 10), and the value is parsed with "Lex.Int" or
   "Lex.Unsigned", respectively, in the specified base. 

|    r <style> <prec> <value>
|    l <style> <prec> <value>
|    e <style> <prec> <value>

   Here, <style> is one of the characters 'S', 'F', or 'A', for the styles
   "Style.Sci", "Style.Fix", and "Style.Auto", respectively. <prec> is a
   base-10 non-negative number specifying the format precision. If <prec> is
   negative, the maximum number of significant digits for the type of number
   being formatted is used. The <value> is printed twice: once for each of the
   values of the "literal" parameter. The current rounding mode under which
   the procedures for formatting float-point values operate may be set with a
   line of the form:

|    m <roundingMode>

   where <roundingMode> is one of the rounding modes named in the FloatMode
   interface. This command also attempts to change the rounding mode for
   purposes of lexing the input <value> passed to the "r", "l", and "e"
   commands. Unfortunately, the attempt does not necessarily succeed, since
   not all rounding modes are necessarily supported on all architectures.

|    p <align> <length> <text>

   Here, <align> is one of the characters 'L' or 'R', for the alignments
   "Align.Left" and "Align.Right", respectively. <length> is the base-10
   length in which to pad. Whitespace after the length is skipped, and then
   <text> is padded with '*' characters in the specified width with the
   specified alignment.

|    f <fmt> <t1> <t2> <t3> ... <t5>
|    F <fmt> <t1> ...

   Here, <fmt> and the <ti> are strings *without* whitespace. In the first
   case, any number of <ti> up to at most 5 may be specified. In the second
   case, any number may be specified, but this number must match the number of
   format specifiers in <fmt>.
*)

IMPORT Lex, Fmt, FmtBufF, FmtBufTest, Params, OSError;
IMPORT Wr, Rd, FileRd, Text, TextRd, Thread, FloatMode;
IMPORT Real AS R, LongReal AS LR, Extended AS ER;
IMPORT RealFloat, LongFloat, ExtendedFloat;
FROM Stdio IMPORT stdin, stdout, stderr;

VAR debug := FALSE;

(* When "debug" is "TRUE" during the floating-point tests ("r", "l", or "e"),
   an ASCII representation of the decimal approximation returned by
   "ToDecimal" is printed before either of the normal outputs. *)

<* FATAL Thread.Alerted, Rd.Failure, Wr.Failure, OSError.E *>

CONST FlagName = ARRAY FloatMode.Flag OF TEXT {
  "Invalid Operation", "Inexact Operation", "Floating-point Overflow",
  "Floating-point Underflow", "Division by Zero", "Integer Overflow",
  "Integer Division by Zero"};

CONST RoundingName = ARRAY FloatMode.RoundingMode OF TEXT {
  "NearestElseEven", "TowardMinusInfinity", "TowardPlusInfinity",
  "TowardZero", "NearestElseAwayFromZero", "IBM370", "Other"};

CONST Prefix = "-> ";

EXCEPTION Error(TEXT);

PROCEDURE GetBase(rd: Rd.T; defaultBase: [2..16]): [2..16]
    RAISES {Error, Lex.Error, FloatMode.Trap, Rd.EndOfFile} =
  VAR c := Rd.GetChar(rd); res: INTEGER; BEGIN
    Rd.UnGetChar(rd);
    IF c IN Lex.Blanks THEN RETURN defaultBase END;
    res := Lex.Int(rd);
    IF res < 2 OR res > 16 THEN
      RAISE Error("Base specifier too large or too small")
    END;
    RETURN res
  END GetBase;

PROCEDURE GetFloatParams(
    rd: Rd.T;
    VAR (*OUT*) style: Fmt.Style;
    VAR (*INOUT*) prec: CARDINAL)
  RAISES {Error, Lex.Error, FloatMode.Trap, Rd.EndOfFile} =
(* Parse the style and precision arguments, setting "style" and "prec" as
   appropriate. If the specified precision is negative, then "prec" is
   unchanged. *)
  VAR c: CHAR; sPrec: INTEGER; BEGIN
    Lex.Skip(rd);
    c := Rd.GetChar(rd);
    CASE c OF
      'S', 's' => style := Fmt.Style.Sci
    | 'F', 'f' => style := Fmt.Style.Fix
    | 'A', 'a' => style := Fmt.Style.Auto
    ELSE RAISE Error("Invalid style specifier \'" & Text.FromChar(c) & "\'")
    END;
    sPrec := Lex.Int(rd, defaultBase := 10);
    IF sPrec >= 0 THEN prec := sPrec END
  END GetFloatParams;

PROCEDURE GetPadParams(
    rd: Rd.T;
    VAR (*OUT*) align: Fmt.Align;
    VAR (*OUT*) width: CARDINAL)
  RAISES {Error, Lex.Error, FloatMode.Trap, Rd.EndOfFile} =
  VAR c: CHAR; sWidth: INTEGER; BEGIN
    Lex.Skip(rd);
    c := Rd.GetChar(rd);
    CASE c OF
      'l', 'L' => align := Fmt.Align.Left
    | 'r', 'R' => align := Fmt.Align.Right
    ELSE RAISE Error("Invalid alignment specifier \'"& Text.FromChar(c) &"\'")
    END;
    sWidth := Lex.Int(rd, defaultBase := 10);
    IF sWidth < 0 THEN RAISE Error("Negative width specification") END;
    width := sWidth
  END GetPadParams;

PROCEDURE GetFNParams(rd: Rd.T): REF ARRAY OF TEXT =
  TYPE Texts = REF ARRAY OF TEXT;
  VAR buff := NEW(Texts, 10); i := 0; BEGIN
    LOOP
      Lex.Skip(rd);
      IF Rd.EOF(rd) THEN EXIT END;
      IF i > LAST(buff^) THEN
        VAR new := NEW(Texts, 2 * NUMBER(buff^)); BEGIN
          SUBARRAY(new^, 0, i) := buff^;
          buff := new
        END
      END;
      buff[i] := Lex.Scan(rd);
      INC(i)
    END;
    VAR res := NEW(Texts, i); BEGIN
      res^ := SUBARRAY(buff^, 0, i);
      RETURN res
    END
  END GetFNParams;

PROCEDURE WriteResult(t: TEXT) =
  BEGIN
    Wr.PutText(stdout, Prefix);
    Wr.PutChar(stdout, '\"');
    Wr.PutText(stdout, t);
    Wr.PutText(stdout, "\"\n")
  END WriteResult;

PROCEDURE WriteUnread(rd: Rd.T) =
  BEGIN
    Wr.PutText(stdout, Prefix & "\"");
    Wr.PutText(stdout, Rd.GetText(rd, LAST(CARDINAL)) & "\"\n\n");
    Wr.Flush(stdout)
  END WriteUnread;

PROCEDURE WriteBits(READONLY w: ARRAY OF CHAR) =
  BEGIN
    Wr.PutText(stdout, Prefix & "16_");
    FOR i := FIRST(w) TO LAST(w) DO
      Wr.PutText(stdout, Fmt.Pad(Fmt.Unsigned(ORD(w[i])), 2, padChar := '0'))
    END;
    Wr.PutText(stdout, " (floating-binary bits)\n");
    Wr.Flush(stdout)
  END WriteBits;

PROCEDURE WriteRealApprox(r: REAL) =
  VAR num: FmtBufF.NumAttr; da := RealFloat.ToDecimal(r); BEGIN
    num.class := FmtBufF.ClassMapReal[da.class];
    num.kind := FmtBufF.IEEEKind.Single;
    num.sign := da.sign;
    IF num.class = FmtBufF.Class.Number THEN
      num.maxExpDigits := R.MaxExpDigits;
      num.len := da.len;
      num.exp := da.exp;
      num.errorSign := da.errorSign
    END;
    WriteApprox(num, da.digits)
  END WriteRealApprox;

PROCEDURE WriteLongApprox(r: LONGREAL) =
  VAR num: FmtBufF.NumAttr; da := LongFloat.ToDecimal(r); BEGIN
    num.class := FmtBufF.ClassMapReal[da.class];
    num.kind := FmtBufF.IEEEKind.Double;
    num.sign := da.sign;
    IF num.class = FmtBufF.Class.Number THEN
      num.maxExpDigits := LR.MaxExpDigits;
      num.len := da.len;
      num.exp := da.exp;
      num.errorSign := da.errorSign
    END;
    WriteApprox(num, da.digits)
  END WriteLongApprox;

PROCEDURE WriteExtdApprox(r: EXTENDED) =
  VAR num: FmtBufF.NumAttr; da := ExtendedFloat.ToDecimal(r); BEGIN
    num.class := FmtBufF.ClassMapReal[da.class];
    num.kind := FmtBufF.IEEEKind.Extended;
    num.sign := da.sign;
    IF num.class = FmtBufF.Class.Number THEN
      num.maxExpDigits := ER.MaxExpDigits;
      num.len := da.len;
      num.exp := da.exp;
      num.errorSign := da.errorSign
    END;
    WriteApprox(num, da.digits)
  END WriteExtdApprox;

PROCEDURE WriteApprox(
    READONLY num: FmtBufF.NumAttr;
    READONLY digits: FmtBufF.Digits) =
  BEGIN
    Wr.PutText(stdout, Prefix & "\"");
    IF num.sign = 1 THEN Wr.PutChar(stdout, '-') END;
    CASE num.class OF
      FmtBufF.Class.NaN =>  Wr.PutText(stdout, "NaN")
    | FmtBufF.Class.Inf =>  Wr.PutText(stdout, "Infinity")
    | FmtBufF.Class.Zero => Wr.PutChar(stdout, '0')
    | FmtBufF.Class.Number =>
        FOR i := 0 TO num.len - 1 DO
          Wr.PutChar(stdout, VAL(digits[i] + ORD('0'), CHAR));
          IF i = 0 AND num.len > 1 THEN Wr.PutChar(stdout, '.') END
        END;
        Wr.PutChar(stdout, 'e');
        Wr.PutText(stdout, Fmt.Int(num.exp))
    END;
    Wr.PutText(stdout, "\" (decimal approximation)\n")
  END WriteApprox;

TYPE
  Kind = {
    Int, Unsigned,
    Real, LongReal, Extended, SetRoundingMode,
    Pad, F, FN};

CONST
  CommentChar = '#';
  PadChar = '*';

VAR
  line: TEXT;
  kind: Kind;
  rd := NEW(TextRd.T);
  inFile: Rd.T;

BEGIN
  (* open file if one is specified *)
  IF Params.Count > 1
    THEN inFile := FileRd.Open(Params.Get(1))
    ELSE inFile := stdin
  END;

  WHILE NOT Rd.EOF(inFile) DO
    <* FATAL Rd.EndOfFile *> BEGIN
      line := Rd.GetLine(inFile)
    END;
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
	CASE Rd.GetChar(rd) OF
	| 'i' => kind := Kind.Int
	| 'u' => kind := Kind.Unsigned
	| 'r' => kind := Kind.Real
	| 'l' => kind := Kind.LongReal
	| 'e' => kind := Kind.Extended
        | 'm' => kind := Kind.SetRoundingMode
        | 'p' => kind := Kind.Pad
        | 'f' => kind := Kind.F
        | 'F' => kind := Kind.FN
        ELSE RAISE Error("Unrecognized command character")
	END;
	CASE kind OF
	| Kind.Int =>
            VAR base := GetBase(rd, defaultBase := 10); BEGIN
              WriteResult(Fmt.Int(Lex.Int(rd, base), base))
            END
	| Kind.Unsigned =>
            VAR base := GetBase(rd, defaultBase := 16); BEGIN
              WriteResult(Fmt.Unsigned(Lex.Unsigned(rd, base), base))
            END
	| Kind.Real =>
            VAR
              style: Fmt.Style; r: REAL;
              prec: CARDINAL := R.MaxSignifDigits - 1;
            BEGIN
              GetFloatParams(rd, style, prec);
              r := Lex.Real(rd);
              IF debug THEN WriteRealApprox(r) END;
              WriteResult(Fmt.Real(r, style, prec, literal := FALSE));
              WriteResult(Fmt.Real(r, style, prec, literal := TRUE));
            END
	| Kind.LongReal =>
            VAR
              style: Fmt.Style; r: LONGREAL;
              prec: CARDINAL := LR.MaxSignifDigits - 1;
            BEGIN
              GetFloatParams(rd, style, prec);
              r := Lex.LongReal(rd);
              IF debug THEN
                WriteBits(LOOPHOLE(r, ARRAY OF CHAR)); WriteLongApprox(r)
              END;
              WriteResult(Fmt.LongReal(r, style, prec, literal := FALSE));
              WriteResult(Fmt.LongReal(r, style, prec, literal := TRUE));
            END
	| Kind.Extended =>
            VAR
              style: Fmt.Style; r: EXTENDED;
              prec: CARDINAL := ER.MaxSignifDigits - 1;
            BEGIN
              GetFloatParams(rd, style, prec);
              r := Lex.Extended(rd);
              IF debug THEN WriteExtdApprox(r) END;
              WriteResult(Fmt.Extended(r, style, prec, literal := FALSE));
              WriteResult(Fmt.Extended(r, style, prec, literal := TRUE));
            END
        | Kind.SetRoundingMode =>
            VAR modeName: TEXT; mode: FloatMode.RoundingMode; BEGIN
              Lex.Skip(rd);
              modeName := Lex.Scan(rd);
              VAR match := FALSE; BEGIN
              	FOR i := FIRST(RoundingName) TO LAST(RoundingName) DO
              	  IF Text.Equal(modeName, RoundingName[i]) THEN
              	    match := TRUE;
              	    mode := i;
                    EXIT
              	  END
              	END;
                IF NOT match THEN
                  RAISE Error("unrecognized rounding mode \""& modeName &"\"")
                END
              END;
              FmtBufTest.useCurrentRounding := FALSE;
              FmtBufTest.testRoundingMode := mode;
              TRY FloatMode.SetRounding(mode) EXCEPT
                FloatMode.Failure =>
                  Wr.PutText(stderr, "** FloatMode.SetRounding failed: \"");
                  Wr.PutText(stderr, RoundingName[mode] & "\"\n");
                  Wr.Flush(stderr)
              END;
              WriteResult(modeName)
            END
        | Kind.Pad =>
            VAR align: Fmt.Align; width: CARDINAL; BEGIN
              GetPadParams(rd, align, width);
              Lex.Skip(rd);
              WriteResult(Fmt.Pad(Lex.Scan(rd), width,
                padChar := PadChar, align := align))
            END
        | Kind.F =>
            VAR fmt: TEXT; t: ARRAY [0..4] OF TEXT; i := 0; BEGIN
              Lex.Skip(rd); fmt := Lex.Scan(rd);
              WHILE i < 5 DO
                Lex.Skip(rd);
                IF Rd.EOF(rd) THEN EXIT END;
                t[i] := Lex.Scan(rd);
                INC(i)
              END;
              WHILE i < 5 DO t[i] := NIL; INC(i) END;
              WriteResult(Fmt.F(fmt, t[0], t[1], t[2], t[3], t[4]))
            END
        | Kind.FN =>
            VAR fmt: TEXT; t: REF ARRAY OF TEXT; BEGIN
              Lex.Skip(rd); fmt := Lex.Scan(rd);
              t := GetFNParams(rd);
              WriteResult(Fmt.FN(fmt, t^))
            END
	END;
        Wr.PutChar(stdout, '\n');
        Wr.Flush(stdout)
      EXCEPT
	Lex.Error =>
          Wr.PutText(stdout, "** Raised \"Lex.Error\"\n");
          WriteUnread(rd)
      | FloatMode.Trap (flag) =>
          Wr.PutText(stdout, "** " & FlagName[flag] & "\n");
          WriteUnread(rd)
      | Rd.EndOfFile =>
          Wr.PutText(stdout, "** " & "Premature end-of-line\n");
          WriteUnread(rd)
      | Error (msg) =>
          Wr.PutText(stdout, "** " & "Error: " & msg & "\n");
          WriteUnread(rd)
      END
    END
  END;
  IF inFile # stdin THEN Rd.Close(inFile) END
END FmtTest.
