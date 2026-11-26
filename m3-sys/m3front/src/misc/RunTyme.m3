(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE RunTyme;

IMPORT M3ID, Value, ValueRep, Scope, Module, Error, Procedure;
IMPORT CG, M3RT, Target, TInt, M3;
IMPORT Host, Type, TipeMap;

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
    "CheckLoadTracedRef", "CheckLoadTracedBlock", "CheckStoreTraced", "NoOp"
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
    blockMaps := NIL;
    blockMapsUnit := NIL;
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
      Error.QID (M3.QID {module := hooks_name, item := name},
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

(*------------------------------------------------- block read barriers ---*)

TYPE
  BlockMap = REF RECORD
    uid   : INTEGER;
    offset: INTEGER;     (* in the unit's constant pool; -1: no refs *)
    next  : BlockMap;
  END;

VAR
  blockMaps    : BlockMap := NIL;  (* the gc maps emitted for this unit *)
  blockMapsUnit: Module.T := NIL;

PROCEDURE BlockMapOffset (t: M3.Type): INTEGER =
  (* The offset in the current unit's constant pool of the garbage
     collector's map of type 't', emitting it the first time; -1 if 't'
     holds no traced references. *)
  VAR unit := Module.Current ();  uid := Type.GlobalUID (t);  m: BlockMap;
  BEGIN
    IF unit # blockMapsUnit THEN
      blockMaps := NIL;
      blockMapsUnit := unit;
    END;
    m := blockMaps;
    WHILE m # NIL DO
      IF m.uid = uid THEN RETURN m.offset END;
      m := m.next;
    END;
    TipeMap.Start ();
    Type.GenMap (t, 0, -1, refs_only := TRUE);
    m := NEW (BlockMap, uid := uid, next := blockMaps,
              offset := TipeMap.Finish ("gc map for block copy of ",
                                        Type.Name (t)));
    blockMaps := m;
    RETURN m.offset;
  END BlockMapOffset;

PROCEDURE BlockNeedsBarrier (t: M3.Type): BOOLEAN =
  VAR info: Type.Info;
  BEGIN
    IF NOT Host.doIncGC THEN RETURN FALSE END;
    EVAL Type.CheckInfo (t, info);
    RETURN info.isTraced;
  END BlockNeedsBarrier;

PROCEDURE EmitCheckLoadTracedBlock (t: M3.Type;  n: CG.Val := NIL;
                                    stride: INTEGER := 0) =
  VAR proc: Procedure.T;  dst: CG.Val;  map: INTEGER;
  BEGIN
    IF NOT BlockNeedsBarrier (t) THEN CG.Discard (CG.Type.Addr); RETURN END;
    map := BlockMapOffset (t);
    IF map < 0 THEN CG.Discard (CG.Type.Addr); RETURN END;
    dst := CG.Pop ();
    proc := LookUpProc (Hook.CheckLoadTracedBlock);
    Procedure.StartCall (proc);
    IF Target.DefaultCall.args_left_to_right THEN
      FOR i := 0 TO 3 DO PassBlockParam (i, dst, map, n, stride) END;
    ELSE
      FOR i := 3 TO 0 BY -1 DO PassBlockParam (i, dst, map, n, stride) END;
    END;
    Procedure.EmitCall (proc);
    CG.Free (dst);
  END EmitCheckLoadTracedBlock;

PROCEDURE PassBlockParam (i: [0..3];  dst: CG.Val;  map: INTEGER;  n: CG.Val;
                          stride: INTEGER) =
  (* Pass parameter 'i' of RTHooks.CheckLoadTracedBlock (dst, map, n,
     stride). *)
  BEGIN
    CASE i OF
    | 0 => CG.Push (dst);
           CG.Pop_param (CG.Type.Addr);
    | 1 => Module.LoadGlobalAddr (Module.Current (), map, is_const := TRUE);
           CG.Pop_param (CG.Type.Addr);
    | 2 => IF n = NIL
             THEN CG.Load_intt (1);
             ELSE CG.Push (n);
           END;
           CG.Pop_param (Target.Integer.cg_type);
    | 3 => CG.Load_intt (stride);
           CG.Pop_param (Target.Integer.cg_type);
    END;
  END PassBlockParam;

PROCEDURE EmitCopy (t: M3.Type;  size: INTEGER;  overlap, barrier: BOOLEAN) =
  VAR src, dst: CG.Val;
  BEGIN
    IF NOT (barrier AND BlockNeedsBarrier (t)) THEN
      CG.Copy (size, overlap);
      RETURN;
    END;
    src := CG.Pop ();
    dst := CG.Pop ();
    CG.Push (dst);
    CG.Push (src);
    CG.Copy (size, overlap);
    CG.Free (src);
    CG.Push (dst);
    CG.Free (dst);
    EmitCheckLoadTracedBlock (t);
  END EmitCopy;

PROCEDURE EmitCopy_n (eltT: M3.Type;  eltPack: INTEGER;
                      overlap, barrier: BOOLEAN) =
  VAR n, src, dst: CG.Val;
  BEGIN
    IF NOT (barrier AND BlockNeedsBarrier (eltT)) THEN
      CG.Copy_n (eltPack, overlap);
      RETURN;
    END;
    <* ASSERT eltPack MOD Target.Byte = 0 *>
    n := CG.Pop ();
    src := CG.Pop ();
    dst := CG.Pop ();
    CG.Push (dst);
    CG.ForceStacked ();
    CG.Push (src);
    CG.ForceStacked ();
    CG.Push (n);
    CG.Copy_n (eltPack, overlap);
    CG.Free (src);
    CG.Push (dst);
    CG.Free (dst);
    EmitCheckLoadTracedBlock (eltT, n, eltPack DIV Target.Byte);
    CG.Free (n);
  END EmitCopy_n;

BEGIN
END RunTyme.
