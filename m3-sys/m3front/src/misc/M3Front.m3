(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: M3Front.m3                                            *)
(* Last modified on Tue Dec  6 08:16:20 PST 1994 by kalsow     *)
(*      modified on Sun Jan 21 06:56:46 1990 by muller         *)

MODULE M3Front;

IMPORT Wr, Fmt, Thread(** , RTCollector, RTCollectorSRC **);
IMPORT Token, Error, Scanner, Value, Scope, M3String, M3WString, Brand;
IMPORT Module, Type, BuiltinTypes, Host, Tracer, M3Header, InfoModule;
IMPORT BuiltinOps, WordModule, M3, Time, Coverage, Marker, TypeFP;
IMPORT Ident, TextExpr, Procedure, SetExpr, TipeDesc, Pathname;
IMPORT ESet, CG, TextWr, Target, ProcBody, RunTyme, M3ID, Variable;

VAR mu         : MUTEX    := NEW (MUTEX);
VAR builtins   : Module.T := NIL;

PROCEDURE ParseImports (READONLY input : SourceFile;
                                 env   : Environment): IDList =
  VAR ids: IDList := NIL;
  BEGIN
    LOCK mu DO
      (* make the arguments globally visible *)
      Host.env      := env;
      Host.source   := input.contents;
      Host.filename := input.name;

      IF (builtins = NIL) THEN Initialize () END;

      Scanner.Push (Host.filename, Host.source, is_main := TRUE);
        ids := M3Header.Parse ();
      Scanner.Pop ();
      RETURN ids;
    END;
  END ParseImports;

PROCEDURE Compile (READONLY input    : SourceFile;
                            env      : Environment;
                   READONLY options  : ARRAY OF TEXT): BOOLEAN =
  VAR ok: BOOLEAN;  start: Time.T;
  BEGIN
    LOCK mu DO
      start := Time.Now ();

      (* make the arguments globally visible *)
      Host.env      := env;
      Host.source   := input.contents;
      Host.filename := input.name;

      IF NOT Host.Initialize (options) THEN RETURN FALSE; END;

      IF NOT Host.stack_walker THEN
        (* command line override... *)
        Target.Has_stack_walker := FALSE;
      END;

      IF (builtins = NIL) THEN Initialize () END;

      Reset ();
      DoCompile ();
      ok := Finalize ();

      IF (Host.report_stats) THEN DumpStats (start, Time.Now ()); END;
    END;
    RETURN ok;
  END Compile;

PROCEDURE Initialize () =
  BEGIN
    (* this list is ordered! *)
    Type.Initialize ();
    TypeFP.Initialize ();

    Scanner.Push ("M3_BUILTIN", NIL, is_main := Host.emitBuiltins);
      builtins := Module.NewDefn ("M3_BUILTIN", TRUE, Scope.Initial);
      BuiltinTypes.Initialize ();
      BuiltinOps.Initialize ();
    Scanner.Pop ();

    Scanner.Push ("Word.i3", NIL, is_main := Host.emitBuiltins);
      WordModule.Initialize ();
    Scanner.Pop ();

    Scanner.Push ("Compiler.i3", NIL, is_main := Host.emitBuiltins);
      InfoModule.Initialize ();
    Scanner.Pop ();
  END Initialize;

PROCEDURE Reset () =
  BEGIN
    (* this list is ordered! *)
    M3String.Reset ();
    M3WString.Reset ();
    Scanner.Reset ();
    Scope.Reset ();
    Coverage.Reset ();
    Error.Reset ();
    Marker.Reset ();
    ESet.Reset ();
    ProcBody.Reset ();
    RunTyme.Reset ();
    TipeDesc.Reset ();
    Tracer.Reset ();
    Type.Reset ();
    TypeFP.Reset ();
    Brand.Reset ();
    Value.Reset ();
    Module.Reset ();
    Ident.Reset ();
    TextExpr.Reset ();
    Procedure.Reset ();
    Variable.Reset ();
    SetExpr.Init ();
    InfoModule.Reset ();
  END Reset;


PROCEDURE DoCompile () =
  VAR m: Module.T;  cs := M3.OuterCheckState;  m_name, filename: M3ID.T;
  BEGIN
(***
RTCollectorSRC.gcRatio := 0.5; (* don't bother collecting much *)
RTCollectorSRC.incremental := FALSE;
RTCollector.Disable ();
***)
    Scanner.Push (Host.filename, Host.source, is_main := TRUE);

    StartPhase ("initializing builtins");
    CheckBuiltins ();

    StartPhase ("parsing");
    m := Module.Parse ();

    (* check that the module name matches the file name *)
    m_name := Module.Name (m);
    filename := M3ID.Add (Pathname.LastBase (Host.filename));
    IF (m_name # filename) THEN
      Error.Warn (2, "file name (" & Pathname.Last (Host.filename)
                    & ") doesn't match module name ("
                    & M3ID.ToText (m_name) & ")");
    END;
(***
RTCollector.Enable ();
***)
    IF Failed () THEN RETURN END;

    StartPhase ("type checking");
    Module.TypeCheck (m, TRUE, cs);
    IF Failed () THEN RETURN END;

    StartPhase ("emitting code");
    CG.Init ();
    IF Failed () THEN RETURN END;
    IF (Host.emitBuiltins) THEN
      Module.MakeCurrent (builtins);
      Module.MakeCurrent (WordModule.M);
      Module.MakeCurrent (InfoModule.M);
      Module.Compile (builtins);
      Module.Compile (WordModule.M);
      Module.Compile (InfoModule.M);
    ELSE
      Module.Compile (m);
    END;
    IF Failed () THEN RETURN END;
  END DoCompile;

PROCEDURE CheckBuiltins () =
  VAR cs := M3.OuterCheckState;
  BEGIN
    Value.TypeCheck (builtins, cs);
    Value.TypeCheck (WordModule.M, cs);
    Value.TypeCheck (InfoModule.M, cs);
  END CheckBuiltins;

PROCEDURE StartPhase (tag: TEXT) =
  BEGIN
    IF (Host.verbose) THEN
      Host.env.report_error (NIL, 0, tag & "...");
    END;
  END StartPhase;

PROCEDURE Failed (): BOOLEAN =
  VAR errs, warns: INTEGER;
  BEGIN
    Error.Count (errs, warns);
    RETURN (errs > 0);
  END Failed;

PROCEDURE DumpStats (start, stop: Time.T) =
  <*FATAL Wr.Failure, Thread.Alerted*>
  VAR
    wr      := TextWr.New ();
    elapsed := MAX (stop - start, 1.0d-6);
    speed   := FLOAT (Scanner.nLines, LONGREAL) / elapsed;
  BEGIN
    Wr.PutText (wr, "  ");
    Wr.PutText (wr, Fmt.Int (Scanner.nLines));
    Wr.PutText (wr, " lines (");
    Wr.PutText (wr, Fmt.Int (Scanner.nPushed));
    Wr.PutText (wr, " files) scanned, ");
    Wr.PutText (wr, Fmt.LongReal (elapsed, Fmt.Style.Fix, 2));
    Wr.PutText (wr, " seconds, ");
    Wr.PutText (wr, Fmt.LongReal (speed, Fmt.Style.Fix, 1));
    Wr.PutText (wr, " lines / second.");
    Host.env.report_error (NIL, 0, TextWr.ToText (wr));
  END DumpStats;

PROCEDURE Finalize (): BOOLEAN =
  <*FATAL Wr.Failure, Thread.Alerted*>
  VAR errs, warns: INTEGER;  wr: TextWr.T;
  BEGIN
    Scanner.Pop ();

    Error.Count (errs, warns);
    IF (errs + warns > 0) THEN
      wr := TextWr.New ();
      IF (errs > 0) THEN
        Wr.PutText (wr, Fmt.Int (errs));
        Wr.PutText (wr, " error");
        IF (errs > 1) THEN Wr.PutText (wr, "s") END;
      END;
      IF (warns > 0) THEN
        IF (errs > 0) THEN Wr.PutText (wr, " and ") END;
        Wr.PutText (wr, Fmt.Int (warns));
        Wr.PutText (wr, " warning");
        IF (warns > 1) THEN Wr.PutText (wr, "s") END;
      END;
      Wr.PutText (wr, " encountered");
      Host.env.report_error (NIL, 0, TextWr.ToText (wr));
    END;

    RETURN (errs <= 0);
  END Finalize;

BEGIN
  M3String.Initialize ();
  M3WString.Initialize ();
  Token.Initialize ();
  Scanner.Initialize ();
  Scope.Initialize ();
END M3Front.
