(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright 95 Digital Equipment Corporation.
   Digital Internal Use Only
   Last modified on Mon Dec  4 13:26:37 PST 1995 by detlefs
*)

MODULE CGITrans;

IMPORT HTMLUtils;
IMPORT Env, Text, Scan, Lex, FloatMode, Stdio, Rd;
IMPORT TextTextTbl;

IMPORT Thread;

<*FATAL Rd.Failure, Thread.Alerted *>

PROCEDURE P(reqMeth, conType: TEXT := NIL;
            conLen := FIRST(INTEGER)): TextTextTbl.T RAISES { Error } =
  VAR clStr := Env.Get("CONTENT_LENGTH");
      cl: INTEGER; useCL := FALSE;
      res := NEW(TextTextTbl.Default).init();
  BEGIN
    IF reqMeth = NIL THEN reqMeth := Env.Get("REQUEST_METHOD") END (* IF *);
    IF conType = NIL THEN conType := Env.Get("CONTENT_TYPE") END (* IF *);
    IF reqMeth = NIL OR NOT Text.Equal(reqMeth, PostMeth) THEN
      RAISE Error("Bad REQUEST_METHOD")
    END (* IF *);
    IF conType = NIL OR
      NOT Text.Equal(conType, DefCT) THEN
      RAISE Error("Bad CONTENT_TYPE")
    END (* IF *);
    IF conLen > 0 THEN
      useCL := TRUE
    ELSIF conLen = FIRST(INTEGER) AND clStr # NIL THEN
      TRY
        cl := Scan.Int(clStr);
        IF cl < 0 THEN RAISE Error("Bad CONTENT_LENGTH") END (* IF *);
        useCL := TRUE
      EXCEPT
      | Lex.Error, FloatMode.Trap => 
          RAISE Error("Bad CONTENT_LENGTH")
      END (* TRY *)
    END (* IF *);
    WHILE (NOT useCL OR cl > 0) AND NOT Rd.EOF(Stdio.stdin) DO
      VAR str := HTMLUtils.FMakeWord(Stdio.stdin, '&', cl); BEGIN
        HTMLUtils.PlusToSpace(str);
        str := HTMLUtils.UnEscapeURL(str);
        VAR name := HTMLUtils.MakeWord(str, '='); BEGIN
          EVAL res.put(name, str)
        END (* BEGIN *)
      END (* BEGIN *)
    END (* WHILE *);
    RETURN res
  END P;

BEGIN
END CGITrans.
