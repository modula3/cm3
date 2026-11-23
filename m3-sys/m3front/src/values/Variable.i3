(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: Variable.i3                                           *)
(* Last Modified On Tue Dec 20 15:08:50 PST 1994 By kalsow     *)
(*      Modified On Fri Apr 27 03:11:00 1990 By muller         *)

INTERFACE Variable;

IMPORT M3, M3ID, Type, Value,  Scope, Decl, Target, CG, Tracer, MSIR;

TYPE
  T <: Value.T;

PROCEDURE ParseDecl (READONLY att: Decl.Attributes);

PROCEDURE New (name: M3ID.T;  used: BOOLEAN): T;
(* doesn't insert the object into any scope.... *)

PROCEDURE NewFormal (formal: Value.T;  name: M3ID.T): T;

PROCEDURE BindType (t: T;  type: Type.T;
                    indirect, readonly, open_array_ok, needs_init: BOOLEAN);

PROCEDURE Split (t: T;  VAR type: Type.T;
                 VAR global, indirect, lhs: BOOLEAN);

PROCEDURE IsFormal   (t: T): BOOLEAN;
PROCEDURE IsUpLevel  (t: T): BOOLEAN;  (* accessed from a nested procedure *)

PROCEDURE InitExpr   (t: T): M3.Expr;  (* t's initializer expression, or NIL.
   (M3.Expr == Expr.T; named via M3 to avoid a Variable->Expr->CaptureAnalysis
   ->Variable interface import cycle.)  Used by the MSIR capture-analysis pass
   so an up-level variable referenced ONLY in a nested proc's local-variable
   initializer (never in a body statement) is still discovered as a capture. *)

(* MSIR declarations — called from DeclareGlobalsMSIR and BeginProc. *)
PROCEDURE DeclareGlobalMSIR  (t: T;  weak: BOOLEAN := FALSE);
(* TipeMap bytes for the scope's traced globals (RT0.ModuleInfo gc_map/var_map);
   NIL if none.  Same walk as GenGlobalMap, returned as bytes for the MSIR
   emitter instead of being emitted into the CG global segment. *)
PROCEDURE GenGlobalMapBytesMSIR (s: Scope.T): MSIR.GcMapBytes;
(* weak=TRUE marks the emitted global weak — used when a same-name module
   re-defines an interface-exported variable (the interface unit's strong def
   wins at link).  Module-private globals use the default (strong). *)
PROCEDURE RegisterExternMSIR (t: T);
(* force=TRUE turns an indirect-flagged var (e.g. a WITH designator alias in the
   no-lvalue/bitfield path) into a by-value local: it skips the indirect reject
   and clears t.indirect so later reads treat the slot as a value. *)
PROCEDURE AddLocalMSIR       (t: T;  b: MSIR.Block;  force: BOOLEAN := FALSE): BOOLEAN;
PROCEDURE BindFormalMSIR     (t: T;  p: MSIR.Proc;  b: MSIR.Block);
PROCEDURE HasClosure (t: T): BOOLEAN;

PROCEDURE NeedsAddress (t: T);

PROCEDURE CopyOpenArray (tipe: Type.T;  ref: Type.T);
(* PRE: Pointer to array dope is on TOS. *)
(* POST: TOS replaced by pointer to dope of copy. *) 

PROCEDURE CopyOpenArrayMSIR (ref: Type.T;  READONLY dims: ARRAY OF MSIR.Value;
                             src, bytes: MSIR.Value): MSIR.Value;
(* As CopyOpenArray, for MSIR: allocate on the traced heap an open array of
   ref's referent with shape dims, copy 'bytes' bytes of elements to it from
   src, and return the address of its dope; NIL if abandoned. *)

PROCEDURE SetBounds (t: T;  READONLY min, max: Target.Int);
PROCEDURE GetBounds (t: T;  VAR min, max: Target.Int);

PROCEDURE Load       (t: T);
PROCEDURE LoadLValue (t: T);
PROCEDURE SetLValue  (t: T);

PROCEDURE LocalCGName (t: T;  VAR unit: CG.Var;  VAR offset: INTEGER);
(* return the back-end address of the non-imported variable 't'. *)

PROCEDURE NeedGlobalInit (t: T): BOOLEAN;
PROCEDURE InitGlobal (t: T);

PROCEDURE GenGlobalMap (s: Scope.T): INTEGER;

PROCEDURE ParseTrace (): Tracer.T;
PROCEDURE BindTrace  (t: T;  x: Tracer.T);
PROCEDURE CheckTrace (x: Tracer.T;  VAR cs: Value.CheckState);
PROCEDURE ScheduleTrace (t: T);

PROCEDURE Reset ();

END Variable.
