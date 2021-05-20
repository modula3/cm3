(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE RunTyme;

IMPORT M3ID, Value, ValueRep, Scope, Module, Error, Procedure;
IMPORT CG, M3RT, Target, TInt, M3CG;

CONST
  RunTimeModuleName = "RTHooks";
  LocalAlias = "__" & RunTimeModuleName & "__";
  (* the automagic import:  IMPORT Runtime AS __RunTime__  *)

CONST
  HookNames = ARRAY Hook OF TEXT {
    "CheckIsType", "ScanTypecase",
    "Raise",  "ResumeRaise",  "PushEFrame", "PopEFrame",
    "Concat", "MultiCat",
    "AllocateTracedObj", "AllocateTracedRef", "AllocateOpenArray",
    "AllocateUntracedObj", "AllocateUntracedRef", "AllocateUntracedOpenArray",
    "DisposeUntracedRef", "DisposeUntracedObj",
    "ReportFault", "AssertFailed", "DebugMsg",
    "TextLitInfo", "TextLitGetChar", "TextLitGetWideChar",
    "TextLitGetChars", "TextLitGetWideChars",
    "CheckLoadTracedRef", "CheckStoreTraced"
  };

VAR
  hooks       : Module.T := NIL;
  hooks_name  : M3ID.T   := M3ID.NoID;
  hooks_alias : M3ID.T   := M3ID.NoID;
  hook_procs  : ARRAY Hook OF Procedure.T;

(*---------------------------------------------------------------------------*)

PROCEDURE Reset () =
  BEGIN
    hooks := NIL;
  END Reset;

PROCEDURE Import () =
  BEGIN
    IF (hooks # NIL) THEN RETURN END;
    hooks_name  := M3ID.Add (RunTimeModuleName);
    hooks_alias := M3ID.Add (LocalAlias);
    hooks := Module.LookUp (hooks_name, internal := TRUE);
    FOR h := FIRST (hook_procs) TO LAST (hook_procs) DO
      hook_procs[h] := NIL;
    END;
  END Import;

PROCEDURE Bind (dest: Module.T;  VAR runtime: Module.T;  VAR id: M3ID.T) =
  BEGIN
    IF (dest.name = hooks_name) AND Module.IsInterface ()
      THEN runtime := NIL;    id := M3ID.NoID;
      ELSE runtime := hooks;  id := hooks_alias;
    END;
  END Bind;

PROCEDURE LookUpProc (h: Hook): Procedure.T =
  VAR p := hook_procs [h];
  BEGIN
    <*ASSERT hooks # NIL*>
    IF (p = NIL) THEN
      p := LookUpNewProc (M3ID.Add (HookNames [h]));
      hook_procs [h] := p;
    END;
    <*ASSERT p # NIL*>
    RETURN p;
  END LookUpProc;

(*-------------------------------------------------------------- internal ---*)

PROCEDURE LookUpNewProc (name: M3ID.T): Procedure.T =
  VAR
    v := LookUp (name);
    c := Value.ClassOf (v);
  BEGIN
    IF (c # Value.Class.Procedure) THEN RETURN NIL END;
    Value.Declare (v);  (* force a version stamp *)
    v := Value.Base (v);
    CG.Set_runtime_proc (v.name, Procedure.CGName (v));
    RETURN v;
  END LookUpNewProc;

PROCEDURE LookUp (name: M3ID.T): Value.T =
  VAR syms: Scope.T;  v, v2: Value.T;
  BEGIN
    IF (hooks = NIL) THEN RETURN NIL END;
    syms := Module.ExportScope (hooks);
    IF (syms # NIL)
      THEN v := Scope.LookUp (syms, name, TRUE);
      ELSE v := NIL; (* probably a circular import! *)
    END;
    IF (v = NIL) THEN
      Error.QID (M3CG.QID {module := hooks_name, item := name},
                  "undefined runtime symbol !!")
    END;

    (* If possible, use the local explicit declaration... *)
    syms := Scope.Top ();
    v2 := Scope.LookUp (syms, name, strict := FALSE);
    IF (v2 # NIL) AND Procedure.IsEqual (v2, v) THEN
      v.used := FALSE; (* forget about using the version in the interface *)
      v := v2;
    END;

    RETURN v;
  END LookUp;

PROCEDURE EmitCheckLoadTracedRef () =
  VAR
    proc := LookUpProc (Hook.CheckLoadTracedRef);
    ref := CG.Pop_temp (); 
    skip := CG.Next_label ();
  BEGIN
    CG.Push (ref);
    CG.Load_nil ();
    CG.If_compare (CG.Type.Addr, CG.Cmp.EQ, skip, CG.Maybe);
    CG.Push (ref);
    CG.Loophole (CG.Type.Addr, Target.Integer.cg_type);
    CG.Load_integer (Target.Integer.cg_type, TInt.One);
    CG.And (Target.Integer.cg_type);
    CG.If_true (skip, CG.Maybe);
    CG.Push (ref);
    CG.Ref_to_info (M3RT.RH_gray_offset, M3RT.RH_gray_size);
    CG.If_false (skip, CG.Maybe);
    Procedure.StartCall (proc);
    CG.Push (ref);
    CG.Pop_param (CG.Type.Addr);
    Procedure.EmitCall (proc);
    CG.Set_label (skip);
    CG.Push (ref);
    CG.Free (ref);
  END EmitCheckLoadTracedRef;

PROCEDURE EmitCheckStoreTraced () =
  VAR
    proc := LookUpProc (Hook.CheckStoreTraced);
    ref := CG.Pop_temp ();
    skip := CG.Next_label ();
  BEGIN
    CG.Push (ref);
    CG.Ref_to_info (M3RT.RH_dirty_offset, M3RT.RH_dirty_size);
    CG.If_true (skip, CG.Maybe);
    Procedure.StartCall (proc);
    CG.Push (ref);
    CG.Pop_param (CG.Type.Addr);
    Procedure.EmitCall (proc);
    CG.Set_label (skip);
    CG.Push (ref);
    CG.Free (ref);
  END EmitCheckStoreTraced;

BEGIN
END RunTyme.
