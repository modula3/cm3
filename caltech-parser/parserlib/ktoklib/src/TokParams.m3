(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: TokParams.m3,v 1.2 2001-09-19 15:03:34 wagner Exp $ *)

MODULE TokParams;
IMPORT ParseParams;
IMPORT Wr, Process, Rd;
IMPORT Stdio;
IMPORT Pathname;
IMPORT Text;
IMPORT Thread;
IMPORT TokSpec;
IMPORT FileRdErr;
<* FATAL Thread.Alerted, Wr.Failure, Rd.Failure *>

PROCEDURE Base(t, suffix: TEXT): TEXT =
  VAR
    len: INTEGER;
  BEGIN
    IF Text.Equal("." & Pathname.LastExt(t), suffix) THEN
      len := Text.Length(t) - Text.Length(suffix);
      RETURN Text.Sub(t, 0, len);
    ELSE
      RETURN NIL;
    END;
  END Base;

PROCEDURE Get(progName, inSuffix, outSuffix: TEXT;
              specifyTok: BOOLEAN := TRUE): T =
  VAR
    pp := NEW(ParseParams.T).init(Stdio.stderr);
    result: T;
    usage := progName & " mylang" & inSuffix;
    base: TEXT;
    setBaseSource, setBaseSuffix: TEXT;
  PROCEDURE SetBaseFrom(t, suffix: TEXT) =
    BEGIN
      setBaseSource := t;
      setBaseSuffix := suffix;
      base := Base(t, suffix);
    END SetBaseFrom;
  PROCEDURE Complain(option: TEXT): TEXT =
    BEGIN
      RETURN "Explicit " & option &
             " option necessary because \"" & setBaseSource &
             "\" does not end in \"" & setBaseSuffix & "\"";
    END Complain;
  PROCEDURE RemoveDirsAndExt(fileName: TEXT;
                     VAR destBase: TEXT) RAISES{ParseParams.Error} =
    BEGIN
      destBase := Base(Pathname.Last(fileName), ".i3");
      IF destBase = NIL THEN
        pp.error("\"" & fileName & "\" does not end in \".i3\"");
      END;
    END RemoveDirsAndExt;
  BEGIN
    IF specifyTok THEN
      usage := usage & "  [ -t mylang.t [-ti3 mylangTok.i3] ]";
    END;
    usage := usage & "  [ -o mylang" & outSuffix & " ]\n";
    TRY
      result.source := pp.getNext();
      SetBaseFrom(result.source, inSuffix);
      
      IF pp.keywordPresent("-o") THEN
        result.out := pp.getNext();
      ELSIF base # NIL THEN
        result.out := base & outSuffix;
      ELSE
        pp.error(Complain("-o"));
      END;

      IF NOT specifyTok THEN
        result.tokSource := result.source;
        result.tokOut := result.out;
      ELSIF pp.keywordPresent("-t") THEN
        result.tokSource := pp.getNext();
        SetBaseFrom(result.tokSource, ".t");
        IF pp.keywordPresent("-ti3") THEN
          result.tokOut := pp.getNext();
        ELSIF base # NIL THEN
          result.tokOut := base & "Tok.i3";
        ELSE
          pp.error(Complain("-ti3"));
        END;
      ELSIF base # NIL THEN
        result.tokSource := base & ".t";
        result.tokOut := base & "Tok.i3";
      ELSE
        pp.error(Complain("-t"));
      END;

      RemoveDirsAndExt(result.out, result.outBase);
      RemoveDirsAndExt(result.tokOut, result.tokOutBase);
      pp.finish();
    EXCEPT
    | ParseParams.Error =>
      Wr.PutText(Stdio.stderr, usage);
      Process.Exit(1);
    END;
    RETURN result;
  END Get;
PROCEDURE ReadTokens(tp: T): TokSpec.T =
  VAR
    rd := FileRdErr.Open(tp.tokSource);
    tok := NEW(TokSpec.T).init();
  BEGIN
    tok.read(rd);
    Rd.Close(rd);
    RETURN tok;
  END ReadTokens;
BEGIN
END TokParams.
