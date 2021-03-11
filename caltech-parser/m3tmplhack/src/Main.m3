(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)

MODULE Main;
IMPORT Pathname;
IMPORT Args;
IMPORT Wr, Rd;
IMPORT TextRd;
IMPORT FileWr, FileRd;
IMPORT Text;
IMPORT Process;
IMPORT TextReader;
IMPORT Thread;
IMPORT OSError;
FROM Stdio IMPORT stderr;

<* FATAL Wr.Failure, Thread.Alerted, Rd.EndOfFile, Rd.Failure, OSError.E *>

(* input format: 3 or 4 lines:
   mode: (interface/implementation/module)
   macro: macro base name
   args-file1 (lastBase is NM)
   [args-file2]
*)

CONST
  Delims = " ";
  ModuleMode = "module";
  RawVers = "$Id$";
VAR
  Version := GetVersion();

PROCEDURE GetVersion() : TEXT =
  BEGIN
    IF Text.Length(RawVers) > 17 THEN
      RETURN Text.Sub(RawVers, 15, Text.Length(RawVers) - 17)
    ELSE
      RETURN "3.14159265359"
    END
  END GetVersion;

PROCEDURE FormatNames(args: TextReader.T; countLimit := LAST(INTEGER)): TEXT =
  VAR
    result: TEXT := "";
  BEGIN
    TRY
      FOR i := 1 TO countLimit DO
        result := result & ", " & args.nextE(Delims, TRUE);
      END;
    EXCEPT ELSE END;
    RETURN result;
  END FormatNames;

PROCEDURE WriteProc(commaArgs, instArgs, build, name: TEXT) =
  PROCEDURE Write1(nmExt, pnameExt: TEXT) =
    BEGIN
    Wr.PutText(out, "\nreadonly proc " & name & pnameExt & "(nm" &
      commaArgs & ") is\n    " & build & "_generic_" & mode &
      "(nm" & nmExt & ", \"" & source & "\"" & instArgs & ")\nend\n");
    END Write1;
  BEGIN
    Write1(" & \"" & highNM & "\"", "");
    Write1("", "_named");
  END WriteProc;

PROCEDURE WriteProcs(commaArgs, instArgs: TEXT; doSuffix := TRUE) =
  VAR
    suffix := "";
  BEGIN
    IF doSuffix THEN
      suffix := "_" & mode;
    END;
    WriteProc(commaArgs, instArgs, "build", lowNM & suffix);
    WriteProc(commaArgs, instArgs, "Build", highNM & suffix);
  END WriteProcs;

PROCEDURE Braquefy(a: TEXT; prefix: TEXT := ", "): TEXT =
  VAR
    in := a;
  BEGIN
    IF Text.Length(in) # 0 THEN
      in := Text.Sub(in, 2, LAST(INTEGER));
      (* strip leading comma *)
    END;
    RETURN prefix & "[" & in & "]";
  END Braquefy;

PROCEDURE ConvertArgs(fn: TEXT): TEXT =
  VAR
    rd: Rd.T;
    line, result, sym: TEXT;
    leftPos, rightPos: INTEGER;
    reader: TextReader.T;
  BEGIN
    TRY
      rd := FileRd.Open(fn);
    EXCEPT
      OSError.E =>
      line := "m3tmplhack: ConvertArgs: cannot find file \"" & fn & "\"!";
      Wr.PutText(stderr, line & "\n");
      Process.Exit(0);
      (* remove above line to produce template anyway
         but number of args is wrong *)
      Wr.PutText(out, "\n/* " & line & " */\n");
      rd := TextRd.New("ERROR()\n");
    END;
    REPEAT
      line := Rd.GetLine(rd);
      leftPos := Text.FindChar(line, '(');
    UNTIL (leftPos > 0) OR Text.FindChar(line,';')#-1;
    (* should skip comments, too. oh well.
       We have some comments starting in column 0...*)
    Rd.Close(rd);
    rightPos := Text.FindChar(line, ')');
    IF rightPos=-1 THEN
      sym := "";
    ELSE
      sym := Text.Sub(line, leftPos+1, rightPos-leftPos-1);
    END;
    reader := NEW(TextReader.T).init(sym);
    result := "";
    WHILE reader.next(", \t", sym, TRUE) DO
      sym := LowerFirst(sym);
      IF Text.Length(result) = 0 THEN
        result := sym;
      ELSE
        result := result & " " & sym;
      END;
    END;
    Wr.PutText(out, "\n/* ConvertArgs(\"" & fn & "\")\n   => \"" & line &
      "\"\n   => " & result & " */\n");
    RETURN result;
  END ConvertArgs;

PROCEDURE DoModule() =
  VAR
    argsFN2 := Rd.GetLine(in);
    line1 := ConvertArgs(argsFN1);
    line2 := ConvertArgs(argsFN2);
    args1 := NEW(TextReader.T).init(line1);
    args2 := NEW(TextReader.T).init(line2);
    com := 0;
    comma, bracket: ARRAY [0..2] OF TEXT;
    (* 0 = common, 1 = interface, 2 = implementation *)
  BEGIN
    Wr.PutText(out, "\n/* DoModule()\n   intf_args = \""&line1 &
      "\",\n   impl_args = \""&line2&"\"\n   shared_args = ");
    TRY
      WHILE Text.Equal(args1.nextE(Delims, TRUE),
                       args2.nextE(Delims, TRUE)) DO
        INC(com);
      END;
    EXCEPT ELSE END;
    args1 := NEW(TextReader.T).init(line1);
    args2 := NEW(TextReader.T).init(line2);
    comma[0] := FormatNames(args1, com);
    comma[0] := FormatNames(args2, com);
    comma[1] := FormatNames(args1);
    comma[2] := FormatNames(args2);
    bracket[1] := Braquefy(comma[0] & comma[1]);
    bracket[2] := Braquefy(comma[0] & comma[2]);

    Wr.PutText(out, Braquefy(comma[0],"") & " */\n");

    WriteProcs(comma[0] & comma[1] & comma[2], bracket[1] & bracket[2], FALSE);
    mode := "interface";
    WriteProcs(comma[0] & comma[1], bracket[1]);
    mode := "implementation";
    WriteProcs(comma[0] & comma[2], bracket[2]);
  END DoModule;

PROCEDURE Shorthand(short, ext: TEXT := "") =
  VAR
    newName := short & ext;
  BEGIN
    Wr.PutText(out, "if not defined(\"" & newName & "\") " &
      newName & " = " & short & "_" & mode & ext &" end\n");
  END Shorthand;

PROCEDURE DoOther() =
  VAR
    line := ConvertArgs(argsFN1);
    args := NEW(TextReader.T).init(line);
    comma: TEXT;
  BEGIN
    Wr.PutText(out, "\n/* DoOther(), args = \""&line&"\" */\n");
    <* ASSERT Text.Equal(mode, "interface") OR
    Text.Equal(mode, "implementation") *>
    comma := FormatNames(args);
    WriteProcs(comma, Braquefy(comma));
    Wr.PutText(out, "\n/* shorthand */\n");
    Shorthand(lowNM);
    Shorthand(highNM);
    Shorthand(lowNM, "_named");
    Shorthand(highNM, "_named");
  END DoOther;

PROCEDURE LowerFirst(a: TEXT): TEXT =
  VAR
    c := ORD(Text.GetChar(a, 0));
  BEGIN
    <* ASSERT c >= ORD('A') AND c <= ORD('Z') *>
    RETURN Text.FromChar(VAL(c + ORD('a') - ORD('A'), CHAR)) &
           Text.Sub(a, 1, Text.Length(a) - 1);
  END LowerFirst;

PROCEDURE Base(): TEXT =
  BEGIN
    IF Text.Equal(mode, ModuleMode) THEN
      RETURN highNM;
    ELSE
      RETURN highNM & "_" & mode;
    END;
  END Base;

VAR
  in := FileRd.Open(Args.CommandLine()[0]);
  mode := Rd.GetLine(in);
  highNM := Rd.GetLine(in);
  lowNM := LowerFirst(highNM);
  argsFN1 := Rd.GetLine(in);
  source := Pathname.LastBase(argsFN1);
  base := Base();
  outFN := base & ".tmpl";
  out := FileWr.Open(outFN);
BEGIN
  Wr.PutText(out, "/* " & outFN &
    " generated by m3tmplhack\n   version " & Version & "\n");
  Wr.PutText(out, "   mode = " & mode & ", source = " & source & ".*g */\n");
  IF Text.Equal(mode, ModuleMode) THEN
    DoModule();
  ELSE
    DoOther();
  END;
  Wr.PutText(out, "\n/* END " & outFN & ". */\n\n");
  Wr.Close(out);
END Main.
