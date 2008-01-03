(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon May  8 13:52:34 PDT 1995 by heydon                   *)

UNSAFE MODULE RuntimeTest EXPORTS Main;

(* "RuntimeTest" reads a sequence of bytecode program from standard input,
   disassembles them, and then executes the last one read (which may call
   the other read bytecode programs).  Each bytecode program consists of a
   series of statements that represent the body of a procedure (so each
   should end with .RETURN).  Each statement may be one of the following:

| C<num>

   Creates a new bytestream at "RT.code_tbl[<num>]".  Subsequent bytecodes
   will be appended to this bytestream.  The bytestreams in an input file must
   be numbered sequentially starting with 1.

| .<name>

   The bytecode for the instruction named "<name>" is appended to the
   current bytestream (1 byte). The valid bytecode names are the texts in
   the range of the "JunoByteCode.names" array.

| b<num>

   The number is converted to a "RT.ByteCode" and appended to the current
   bytestream (1 byte).

| c<num>

   The number is converted to a "cardinal" and marshalled into the current
   bytestream (4 bytes).

| s<num>

   The number is converted to a "JunoMarshal.short" and marshalled into the
   current bytestream (2 bytes).

| u<num>

   The number is converted to a "JunoMarshal.ushort" and marshalled into the
   current bytestream (2 bytes).

| #<comment>

   Lines beginning with "#" are ignored, as are empty lines.

| V<num>
| <val>

   Set the value of global variable "<num>" to the value "<val>", which must
   be a legal Juno value (including reals, texts, pairs, and lists).

| END

   Terminates reading of the program.  Input for the "Readln" procedure may
   be provided after this point. *)

(* External Procedures:

   At the beginning of program execution, the global variable table
   "RT.value_tbl" and the global code table "RT.code_tbl" are empty, but
   the global external code table "RT.ext_code_tbl" contains 4 procedures,
   which may be used by the program that is read in.  The procedures are:

| PROCEDURE Print(t); (* external procedure 0 *)

   Prints the text value "t" to the standard output, leaving "TRUE" in
   "cond".  If "t" is not a text value, or if there is a failure attempting
   to write to standard output, "cond = FALSE" after invocation.

| PROCEDURE t := Readln(); (* external procedure 1 *)

   Reads a line of text into "t".  "cond = TRUE" after invocation unless
   there is a failure attempting to read from standard input.  The input
   line is echoed to the output, including the terminating newline.

| PROCEDURE t := Unparse(n); (* external procedure 2 *)

   Returns a textual representation of the argument "n", which may be any
   JunoValue.T.

| PROCEDURE n := Atof(t); (* external procedure 3 *)

   Returns the real number of which "t" is a textual representation.  Iff
   no such number exists, "cond = FALSE" after the invocation.
*)

IMPORT JunoLiteral;
IMPORT JunoRT, JunoDisassem, JunoArgs, JunoMarshal, JunoValue, RTVal;
IMPORT   JunoByteCode AS BC;

IMPORT Atom, Lex, Scan, FloatMode, Rd, Wr, Text, Fmt, Thread, TextWr, TextRd;
FROM Stdio IMPORT stdin, stdout, stderr;
FROM Thread IMPORT Alerted;

<* FATAL Wr.Failure, Alerted *>

VAR
  cindex := -1;
  offset := 0;
  linenumber := 0;
  current_stream := NEW(JunoRT.ByteStream, 1000);

PROCEDURE Print(t: TEXT) RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    Wr.PutText(stdout, t);
    Wr.Flush(stdout)
  END Print;

PROCEDURE Linenum() RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    Wr.PutText(stdout, Fmt.Int(linenumber));
    Wr.Flush(stdout)
  END Linenum;

PROCEDURE Newln() RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    Wr.PutChar(stdout, '\n');
    Wr.Flush(stdout)
  END Newln;

EXCEPTION Failed;

TYPE
  PrintObj =   JunoRT.ExternalCode OBJECT OVERRIDES invoke := ExtPrint   END;
  ReadlnObj =  JunoRT.ExternalCode OBJECT OVERRIDES invoke := ExtReadln  END;
  AtofObj =    JunoRT.ExternalCode OBJECT OVERRIDES invoke := ExtAtof    END;
  UnparseObj = JunoRT.ExternalCode OBJECT OVERRIDES invoke := ExtUnparse END;

PROCEDURE ExtPrint(<*UNUSED*> o: PrintObj): BOOLEAN =
(* External procedure 0. *)
  VAR err := FALSE; t := JunoArgs.ReadText(1, err); BEGIN
    IF NOT err THEN
      TRY Print(t); RETURN TRUE EXCEPT
        Wr.Failure, Thread.Alerted => (* SKIP *)
      END
    END;
    RETURN FALSE
  END ExtPrint;

PROCEDURE ExtReadln(<*UNUSED*> o: ReadlnObj): BOOLEAN =
(* External procedure 1. *)
  VAR t: TEXT; BEGIN
    TRY
      t := Rd.GetLine(stdin);
      Print(t);
      Newln();
      JunoArgs.WriteText(1, t);
      RETURN TRUE
    EXCEPT
      Rd.Failure, Thread.Alerted, Rd.EndOfFile => (* SKIP *)
    END;
    RETURN FALSE
  END ExtReadln;

PROCEDURE ExtUnparse(<*UNUSED*> o: UnparseObj): BOOLEAN =
(* External procedure 2. *)
  <*FATAL Wr.Failure*>
  VAR wr := TextWr.New(); BEGIN
    JunoValue.Unparse(wr, RTVal.ToJV(JunoArgs.ReadValue(1)));
    JunoArgs.WriteText(2, TextWr.ToText(wr));
    Wr.Close(wr);
    RETURN TRUE
  END ExtUnparse;

PROCEDURE ExtAtof(<*UNUSED*> o: AtofObj): BOOLEAN =
(* External procedure 3. *)
  VAR err := FALSE; t := JunoArgs.ReadText(1, err); x: JunoValue.Real; BEGIN
    IF NOT err THEN
      TRY
        x := Scan.Real(t);
        JunoArgs.WriteReal(2, x);
        RETURN TRUE
      EXCEPT
        Lex.Error, FloatMode.Trap => (* SKIP *)
      END;
    END;
    RETURN FALSE
  END ExtAtof;

PROCEDURE Finish() RAISES {Wr.Failure, Thread.Alerted} =
  VAR newcode := NEW(JunoRT.ByteStream, offset); slot: CARDINAL; BEGIN
    FOR i := 0 TO offset - 1 DO newcode[i] := current_stream[i] END;
    slot := JunoRT.GetCodeIndex(
      JunoRT.ProcAttr{NIL, Atom.FromText("CodeSegment" & Fmt.Int(cindex)),
        JunoRT.Sig{999, 999, 999}});
    <* ASSERT slot = cindex *>
    JunoRT.code_tbl[slot] := newcode;
    Print("------- code table " & Fmt.Int(cindex) & "\n");
    JunoDisassem.P(newcode, stdout);
    Newln()
  END Finish;

PROCEDURE TextToConstant(t: TEXT): JunoValue.T RAISES {Failed} =
  <* FATAL Rd.Failure *>
  VAR res: JunoValue.T; BEGIN
    TRY res := JunoLiteral.Parse(TextRd.New(t)) EXCEPT
      JunoLiteral.ParseError =>
        Print("Error parsing expression\n");
        RAISE Failed
    END;
    RETURN res
  END TextToConstant;

PROCEDURE CodeLocKnown() RAISES {Failed} =
  BEGIN
    IF cindex < 0 THEN
      Print("Line ");
      Linenum();
      Print(": you have to specify a code table location.\n");
      RAISE Failed
    END
  END CodeLocKnown;

PROCEDURE ReadProgram(rd: Rd.T) RAISES {Failed, Rd.Failure, Rd.EndOfFile} =
  BEGIN
    WHILE NOT Rd.EOF(rd) DO
      VAR line: TEXT; BEGIN
        TRY line := Rd.GetLine(rd) EXCEPT Rd.EndOfFile => EXIT END;
        INC(linenumber);
        IF Text.Equal(line, "END") THEN EXIT END;
        IF Text.Length(line) # 0 AND Text.GetChar(line, 0) # '#' THEN
          CASE Text.GetChar(line, 0) OF
          | 'C' =>
              IF cindex >= 0 THEN Finish() END;
              TRY
                cindex := Scan.Int(Text.Sub(line, 1, Text.Length(line) - 1))
              EXCEPT
                Lex.Error, FloatMode.Trap =>
                  Print("Can't parse code table index in line ");
                  Linenum();
                  Newln();
                  RAISE Failed
              END;
              offset := 0
          | 'V' =>
              VAR vindex: INTEGER; BEGIN
                TRY
                  vindex := Scan.Int(Text.Sub(line, 1, Text.Length(line) - 1))
                EXCEPT
                  Lex.Error, FloatMode.Trap =>
                    Print("Can't parse code table index in line ");
                    Linenum();
                    Newln();
                    RAISE Failed
                END;
                line := Rd.GetLine(rd);
                INC(linenumber);
                JunoRT.value_tbl[vindex] := TextToConstant(line)
              END
          | 'u', 'b', 's', 'c' =>
              TRY
                CodeLocKnown();
                VAR
                  n := Scan.Int(Text.Sub(line, 1, Text.Length(line) - 1));
                  a: JunoMarshal.BytePtr := ADR(current_stream[offset]);
                  a0 := a;
                BEGIN
                  CASE Text.GetChar(line, 0) OF <* NOWARN *>
                  | 'b' =>
                      IF n < FIRST(BC.names) OR n > LAST(BC.names)
                          OR Text.Equal(BC.names[n], "INVALID") THEN
                        Print("Unknown bytecode " & Fmt.Int(n) & " on line ");
                        Linenum();
                        Print(".\n");
                        RAISE Failed
                      END;
                      a^ := n; INC(a)
                  | 'u' => JunoMarshal.WriteUShort(a, n)
                  | 's' => JunoMarshal.WriteShort(a, n)
                  | 'c' => JunoMarshal.WriteULong(a, n)
                  END;
                  INC(offset, a - a0)
                END
              EXCEPT
                Lex.Error, FloatMode.Trap =>
                  Print("Can't parse bytecode datum in line ");
                  Linenum();
                  Newln();
                  RAISE Failed
              END
          | '.' =>
              CodeLocKnown();
              VAR
                found := FALSE;
                bc_name := Text.Sub(line, 1, Text.Length(line) - 1);
              BEGIN
                FOR i := FIRST(BC.names) TO LAST(BC.names) DO
                  IF Text.Equal(BC.names[i], bc_name) THEN
                    found := TRUE;
                    current_stream[offset] := i;
                    INC(offset);
                    EXIT
                  END
                END;
                IF NOT found THEN
                  Print("Unrecognized instruction \"");
                  Print(Text.Sub(line, 1, Text.Length(line) - 1));
                  Print("\" in line ");
                  Linenum();
                  Newln();
                  RAISE Failed
                END
              END
          ELSE
            Print("Unrecognized statement at line ");
            Linenum();
            Newln();
            RAISE Failed
          END
        END
      END
    END;
    IF cindex >= 0 THEN Finish() END
  END ReadProgram;

PROCEDURE RegisterExtProc(
    nm: TEXT;
    code: JunoRT.ExternalCode;
    inCnt, outCnt: CARDINAL) =
  VAR slot: CARDINAL; BEGIN
    slot := JunoRT.GetExtCodeIndex(
      JunoRT.ProcAttr{ NIL, Atom.FromText(nm),
        JunoRT.Sig{ outCnt, 0, inCnt }});
    JunoRT.ext_code_tbl[slot] := code
  END RegisterExtProc;

BEGIN
  RegisterExtProc("Print",   NEW(PrintObj),   1, 0);
  RegisterExtProc("Readln",  NEW(ReadlnObj),  0, 1);
  RegisterExtProc("Unparse", NEW(UnparseObj), 1, 1);
  RegisterExtProc("Atof",    NEW(AtofObj),    1, 1);
  TRY
    ReadProgram(stdin);
    Print("Running program...\n");
    JunoRT.ResetMachine();
    JunoRT.PushFrame(JunoRT.PC{proc := cindex, offset := 0}, 0);
    Print(JunoRT.TrapMessage(JunoRT.Exec()) & "\n")
  EXCEPT
    Failed, Rd.Failure, Rd.EndOfFile =>
      Wr.PutText(stderr, "Failed, line " & Fmt.Int(linenumber) & ".\n");
      Wr.Flush(stderr)
  END
END RuntimeTest.
