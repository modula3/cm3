(*

  Main.m3 - test module for regular expressions

  This module performs timing and correctness testing for the
  RegEx.i3 interface.

  Edit History:
   Feb 9  1993		Schilit		Update for release.
*)

(* Copyright (c) 1991, 1992 Xerox Corporation.  All rights reserved.

   Use and copying of this software and preparation of derivative works
   based upon this software are permitted.  Any distribution of this
   software or derivative works must comply with all applicable United
   States export control laws.  This software is made available AS IS, and
   Xerox Corporation makes no warranty about the software, its performance
   or its conformity to any specification. *)


UNSAFE MODULE Main;

IMPORT Rd, Wr, Text, M3toC, Ctypes, Time, Fmt, ParseParams, Lex, Thread;
IMPORT Process, Params;

FROM Wr IMPORT PutText;
FROM Stdio IMPORT stdin, stdout, stderr;

IMPORT RegEx;
IMPORT regex;

<*FATAL Thread.Alerted, Wr.Failure, Rd.Failure *>

VAR
  idx                           : INTEGER;
  cmperr                        : TEXT;
  program                       : TEXT;
  pstr, dstr, expect            : TEXT;
  start_time                    : Time.T;
  stop_time                     : Time.T;
  pat                           : RegEx.Pattern;
  reps                          : INTEGER := 1;
  unix, verbose, dump, time, ret: BOOLEAN;

PROCEDURE Usage () =
  BEGIN
    Wr.PutText(
      stderr, "Usage: " & program & " [options]\n" & "Options:\n"
                & "-dump       -- show compiled pattern\n"
                & "-verbose    -- show lots of info\n"
                & "-time n     -- print timing for n repetitions\n"
                & "-unix       -- compare unix regex(3) matching\n");
  END Usage;

PROCEDURE GetParameters () =
  VAR 
    pp := NEW(ParseParams.T).init(stderr);
  BEGIN
    TRY
      program := Params.Get(0);
      time := pp.keywordPresent("-time");
      IF time THEN reps := pp.getNextInt(1) END;
      verbose := pp.keywordPresent("-verbose");
      unix := pp.keywordPresent("-unix");
      dump := pp.keywordPresent("-dump");
      pp.finish();
    EXCEPT
    | ParseParams.Error => Usage(); Process.Exit(1);
    END; (* try *)
  END GetParameters;

PROCEDURE ParseLine (VAR pat, data, expect: TEXT) RAISES {Rd.EndOfFile} =
  VAR c: CHAR;
  BEGIN
    LOOP
      Lex.Skip(stdin); (* skip blanks *)
      c := Rd.GetChar(stdin);
      IF c # '#' THEN EXIT; END;
      EVAL Rd.GetLine(stdin); (* discard comment line *)
    END; (* loop *)

    IF c = '"' OR c = '\'' THEN
      pat := Lex.Scan(stdin, SET OF CHAR {FIRST(CHAR)..LAST(CHAR)}
                             - SET OF CHAR{c});
      EVAL Rd.GetChar(stdin); (* skip close quote *)
    ELSE
      Rd.UnGetChar(stdin);
      pat := Lex.Scan(stdin);
    END; (* if *)

    Lex.Skip(stdin); (* skip blanks *)

    c := Rd.GetChar(stdin);
    IF c = '"' OR c = '\'' THEN
      data := Lex.Scan(stdin, SET OF CHAR {FIRST(CHAR)..LAST(CHAR)}
                              - SET OF CHAR{c});
      EVAL Rd.GetChar(stdin); (* skip close quote *)
    ELSE
      Rd.UnGetChar(stdin);
      data := Lex.Scan(stdin);
    END; (* if *)

    Lex.Skip(stdin, SET OF CHAR{'\t', ' '}); (* skip blanks *)
    expect := Lex.Scan(stdin);
    Lex.Skip(stdin);
  END ParseLine;

PROCEDURE PrintTime (t: TEXT; t1, t2: Time.T) =
  VAR
    usecs := t2 - t1;
  BEGIN
    PutText (stdout, t);
    PutText(stdout, "Time per repeat "& 
      Fmt.LongReal(usecs / FLOAT(reps, LONGREAL) / 1000.0D0) & "\n");

  END PrintTime;

PROCEDURE test_unix (pline, sline: TEXT; cmperr, result: BOOLEAN) =
  VAR
    ret    : Ctypes.char_star;
    rex    : Ctypes.char_star;
    unixret: Ctypes.int;
  BEGIN
    (* first compile for unix and report error if it doesn't work *)

    rex := M3toC.SharedTtoS(pline);
    ret := regex.re_comp(rex);
    M3toC.FreeSharedS(pline, rex);
    IF ret # NIL AND NOT cmperr THEN
      PutText(
        stdout, " ** Unexpected unix re_comp error for " & pline & "\n");
      RETURN;
    END;

    (* execute for unix and time... *)

    start_time := Time.Now();
    FOR i := 1 TO reps DO
      rex := M3toC.SharedTtoS(sline);
      unixret := regex.re_exec(rex);
      M3toC.FreeSharedS(sline, rex);
    END; (* for *)
    stop_time := Time.Now();

    IF ((unixret = 1) # result) THEN
      PutText(
        stdout, Fmt.F(" ** re_exec disagrees for '%s' (unix says %s)\n",
                      pline, Fmt.Bool(unixret = 1)));
    END;

    IF time THEN PrintTime("Unix ", start_time, stop_time); END; (* if *)

  END test_unix;

BEGIN
  GetParameters();
  Wr.PutText(stdout, "Enter a pattern followed by text...\n");
  LOOP
    TRY
      Wr.Flush(stdout);
      ParseLine(pstr, dstr, expect);

      (* compile chan check for compilation error *)

      TRY
        cmperr := NIL;
        pat := RegEx.Compile(pstr);
      EXCEPT
      | RegEx.Error (err) => cmperr := err;
      END; (* try *)

      IF cmperr = NIL AND dump THEN
        PutText(stdout, "Binary pattern: " & RegEx.Dump(pat) & "\n");
      END; (* if *)

      IF verbose THEN
        idx := Wr.Index(stdout);

        IF cmperr = NIL THEN
          PutText(stdout, pstr & " (" & RegEx.Decompile(pat) & ") ");
        ELSE
          PutText(stdout, pstr & " ");
        END;
        FOR i := Wr.Index(stdout) TO 20 + idx DO
          Wr.PutChar(stdout, ' ');
        END;
        PutText(stdout, " " & dstr);
        FOR i := Wr.Index(stdout) TO 40 + idx DO
          Wr.PutChar(stdout, ' ');
        END;
        PutText(stdout, " " & expect);
      END;

      (* if we have a compiled pattern then execute and time *)

      IF cmperr = NIL THEN
        start_time := Time.Now();
        FOR i := 1 TO reps DO
          ret := RegEx.Execute(pat, dstr) # -1;
        END; (* for *)
        stop_time := Time.Now();
      END; (* if *)

      IF cmperr # NIL THEN
        IF NOT Text.Equal(expect, "ERROR") THEN
          PutText(
            stdout, Fmt.F(" ** Unexpected M3 compile error (%s) for %s\n",
                          cmperr, pstr));

        END;
      ELSIF (ret AND NOT Text.Equal(expect, "TRUE"))
              OR (NOT ret AND NOT Text.Equal(expect, "FALSE")) THEN
        PutText(stdout, Fmt.F(" ** Unexpected M3 result (%s) for %s\n",
                              Fmt.Bool(ret), pstr));
      ELSE
        IF verbose THEN PutText(stdout, " OK\n"); END;
      END; (* if *)

      IF cmperr = NIL THEN
        IF time THEN PrintTime("M3 ", start_time, stop_time); END; (* if *)
      END; (* if *)


      IF unix THEN test_unix(pstr, dstr, cmperr # NIL, ret); END;
    EXCEPT
    | Rd.EndOfFile => EXIT;
    END; (* try *)


  END; (* loop *)
END Main.
