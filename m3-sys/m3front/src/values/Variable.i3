(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: Variable.i3                                           *)
(* Last Modified On Tue Dec 20 15:08:50 PST 1994 By kalsow     *)
(*      Modified On Fri Apr 27 03:11:00 1990 By muller         *)

INTERFACE Variable;

IMPORT M3ID, Type, Value,  Scope, Decl, Target, CG, Tracer, MSIR;

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

(* MSIR declarations — called from DeclareGlobalsMSIR and BeginProc. *)
PROCEDURE DeclareGlobalMSIR  (t: T);
PROCEDURE RegisterExternMSIR (t: T);
PROCEDURE AddLocalMSIR       (t: T;  b: MSIR.Block): BOOLEAN;
PROCEDURE BindFormalMSIR     (t: T;  p: MSIR.Proc;  b: MSIR.Block);
(* If t.initPending is TRUE, force the MSIR initialization of t now.  Used by
   NamedExpr.CompileMSIR to respect initialization order (p026: f := j before
   j := 4 would leave f = 0 without this). *)
PROCEDURE ForceInitMSIR (t: T);
PROCEDURE HasClosure (t: T): BOOLEAN;

PROCEDURE NeedsAddress (t: T);

PROCEDURE CopyOpenArray (tipe: Type.T;  ref: Type.T);
(* PRE: Pointer to array dope is on TOS. *)
(* POST: TOS replaced by pointer to dope of copy. *) 

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
