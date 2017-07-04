(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: Host.m3                                               *)
(* Last modified on Thu Jun 29 13:35:46 PDT 1995 by kalsow     *)
(*      modified on Thu Jun 15 11:50:20 PDT 1995 by ericv      *)
(*      modified on Tue May 25 14:27:57 PDT 1993 by muller     *)

MODULE Host;

IMPORT File, Text, (*ETimer, M3Timers,*) M3ID, M3Compiler, Target;

PROCEDURE Initialize (READONLY options: ARRAY OF TEXT): BOOLEAN =
  BEGIN
    verbose              := FALSE;
    errorDie             := -1;
    warnings             := 2;
    coverage             := FALSE;
    versionStamps        := TRUE;
    emitBuiltins         := FALSE;
    init_floats          := FALSE;
    vs_debug             := FALSE;
    load_map             := TRUE;
    stack_walker         := TRUE;
    nested_calls         := FALSE;
    nested_procs_first   := FALSE;
    inline_nested_procs  := TRUE;
    direct_struct_assign := TRUE;
    clean_stores         := FALSE;
    clean_jumps          := TRUE;
    doNarrowChk          := TRUE;
    doRangeChk           := TRUE;
    doReturnChk          := TRUE;
    doCaseChk            := TRUE;
    doTCaseChk           := TRUE;
    doAsserts            := TRUE;
    doNilChk             := TRUE;
    doRaisesChk          := TRUE;
    doProcChk            := FALSE;
    doDebugs             := TRUE;
    new_adr              := FALSE;
    report_stats         := FALSE;
    doIncGC              := TRUE;
    doGenGC              := TRUE;

    FOR i := 0 TO LAST (options) DO
      IF NOT ProcessArg (options[i]) THEN RETURN FALSE; END;
    END;
    RETURN TRUE;
  END Initialize;

(*------------------------------------------------ command line arguments ---*)

PROCEDURE ProcessArg (t: TEXT): BOOLEAN =
  VAR key: TEXT;
  BEGIN
    IF (t = NIL) THEN RETURN TRUE; END;
    key := Text.Sub (t, 0, 2);
    IF (Text.Equal (t, "-v")) THEN
      verbose  := TRUE;
      vs_debug := TRUE;
      warnings := -1;
    ELSIF (Text.Equal (key, "-t")) THEN
      (* ignore the old -tTARGET option which is still in the config files  *)
    ELSIF (Text.Equal (t, "-g")) THEN
      (* generate debugging, which we always do anyway! *)
    ELSIF (Text.Equal (t, "-S")) THEN
      versionStamps := FALSE;   (* don't generate version stamps *)
    ELSIF (Text.Equal (t, "-w")) THEN
      warnings := 99;
    ELSIF (Text.Equal (key, "-w")) THEN
      warnings := GetInt (t, 2);
    ELSIF (Text.Equal (t, "-builtins")) THEN
      emitBuiltins := TRUE; (* emit the predefined scopes *)
    ELSIF (Text.Equal (t, "-Z")) THEN
      coverage := TRUE; (* generate line profiling *)
    ELSIF (Text.Equal (key, "-E")) THEN
      errorDie := GetInt (t, 2);
    ELSIF (Text.Equal (t, "-NoAsserts")) THEN
      doAsserts := FALSE;
    ELSIF (Text.Equal (t, "-NoDebug")) THEN
      doDebugs := FALSE;
    ELSIF (Text.Equal (t, "-NoNarrowChk")) THEN
      doNarrowChk := FALSE;
    ELSIF (Text.Equal (t, "-NoRangeChk")) THEN
      doRangeChk := FALSE;
    ELSIF (Text.Equal (t, "-NoReturnChk")) THEN
      doReturnChk := FALSE;
    ELSIF (Text.Equal (t, "-NoCaseChk")) THEN
      doCaseChk := FALSE;
    ELSIF (Text.Equal (t, "-NoTypecaseChk")) THEN
      doTCaseChk := FALSE;
    ELSIF (Text.Equal (t, "-NoNilChk")) THEN
      doNilChk := FALSE;
    ELSIF (Text.Equal (t, "-NoRaisesChk")) THEN
      doRaisesChk := FALSE;
    ELSIF (Text.Equal (t, "-NoChecks")) THEN
      doAsserts   := FALSE;
      doNarrowChk := FALSE;
      doRangeChk  := FALSE;
      doReturnChk := FALSE;
      doCaseChk   := FALSE;
      doTCaseChk  := FALSE;
      doNilChk    := FALSE;
      doRaisesChk := FALSE;
      doDebugs    := FALSE;
    ELSIF (Text.Equal (t, "-NoIncGC")) THEN
      doIncGC := FALSE;
    ELSIF (Text.Equal (t, "-NoGenGC")) THEN
      doGenGC := FALSE;
    ELSIF (Text.Equal (t, "-InitFloats")) THEN
      init_floats := TRUE;
    ELSIF (Text.Equal (t, "-load_map")) THEN
      load_map := TRUE;
    ELSIF (Text.Equal (t, "-No_load_map")) THEN
      load_map := FALSE;
    ELSIF (Text.Equal (t, "-No_stack_walker")) THEN
      stack_walker := FALSE;
    ELSIF (Text.Equal (t, "-nested_calls")) THEN
      nested_calls  := TRUE;
    ELSIF (Text.Equal (t, "-no_nested_calls")) THEN
      nested_calls  := FALSE;
    ELSIF (Text.Equal (t, "-nested_procs_first")) THEN
      (* nested_procs_first := TRUE; *)
      (* THIS IS BOGUS:  nested_procs_first => the nested procedure
         cannot reference local variables of its parent that
         weren't declared at the outermost level.  *)
    ELSIF (Text.Equal (t, "-nested_procs_last")) THEN
      nested_procs_first := FALSE;
    ELSIF (Text.Equal (t, "-inline_nested_procs")) THEN
      inline_nested_procs := TRUE;
    ELSIF (Text.Equal (t, "-unfold_nested_procs")) THEN
      inline_nested_procs := FALSE;
    ELSIF (Text.Equal (t, "-direct_struct_assign")) THEN
      direct_struct_assign := TRUE;
    ELSIF (Text.Equal (t, "-copying_struct_assign")) THEN
      direct_struct_assign := FALSE;
    ELSIF (Text.Equal (t, "-clean_stores")) THEN
      clean_stores  := TRUE;
    ELSIF (Text.Equal (t, "-dirty_stores")) THEN
      clean_stores  := FALSE;
    ELSIF (Text.Equal (t, "-clean_jumps")) THEN
      clean_jumps   := TRUE;
    ELSIF (Text.Equal (t, "-check_procs")) THEN
      doProcChk     := TRUE;
    ELSIF (Text.Equal (t, "-dirty_jumps")) THEN
      clean_jumps   := FALSE;
    ELSIF (Text.Equal (t, "-vsdebug")) THEN
      vs_debug := TRUE;
    ELSIF (Text.Equal (t, "-new_adr")) THEN
      new_adr := TRUE;
    ELSIF (Text.Equal (t, "-old_adr")) THEN
      new_adr := FALSE;
    ELSIF (Text.Equal (t, "-stats")) THEN
      report_stats := TRUE;
    ELSE
      env.report_error (NIL, 0, "m3c: unknown option, \"" & t & "\"");
      RETURN FALSE;
    END;
    RETURN TRUE;
  END ProcessArg;

PROCEDURE GetInt (t: TEXT;  start: INTEGER): INTEGER =
  VAR c: CHAR;  n: INTEGER := 0;
  BEGIN
    FOR j := start TO Text.Length (t)-1 DO
      c := Text.GetChar (t, j);
      IF (c < '0') OR ('9' < c) THEN RETURN n END;
      n := n * 10 + ORD (c) - ORD ('0');
    END;
    RETURN n;
  END GetInt;

(*-------------------------------------------------- misc file operations ---*)

PROCEDURE OpenUnit (name: M3ID.T; interface, generic: BOOLEAN;
                                  VAR(*OUT*) filename: TEXT): File.T =
  VAR file: M3Compiler.SourceFile;
  BEGIN
    (* ETimer.Push (M3Timers.search); *)
    file := env.find_source (name, interface, generic);
    filename := file.name;
    (* ETimer.Pop (); *)
    RETURN file.contents;
  END OpenUnit;

PROCEDURE CloseFile (rd: File.T) =
  BEGIN
    IF (rd # NIL) THEN
      TRY rd.close () EXCEPT ELSE END;
    END;
  END CloseFile;

PROCEDURE FileTail (path: TEXT): TEXT =
  VAR c: CHAR;
  BEGIN
    IF NOT Target.ReduceTargetVariation THEN RETURN path; END;

    IF (path = NIL) THEN RETURN NIL END;

    (* search for the last slash or blank in the string *)
    FOR x := Text.Length (path) - 1 TO 0 BY -1 DO
      c := Text.GetChar (path, x);
      IF (c = '/') OR (c = ' ') OR (c = '\\') THEN
        RETURN Text.Sub (path, x+1);
      END;
    END;

    (* no slashes *)
    RETURN path;
  END FileTail;

BEGIN
END Host.
