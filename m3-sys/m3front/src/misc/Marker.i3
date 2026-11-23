(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: Marker.i3                                             *)
(* Last Modified On Wed Oct 25 11:39:40 PDT 1995 by ericv      *)
(*      Modified On Fri May 19 07:41:46 PDT 1995 by kalsow     *)
(*      Modified On Sat Jun 10 18:44:15 PDT 1989 by muller     *)

INTERFACE Marker;

IMPORT CG, Type, Variable, ESet, Expr, M3RT, MSIR;

CONST
  Return_exception = -1;
  Exit_exception = -2;

PROCEDURE Pop ();
PROCEDURE InvokeSeen () : BOOLEAN;
PROCEDURE Invoked ();
PROCEDURE Excepts() : ESet.EList;

(* pop to top scope. *)

PROCEDURE SaveFrame ();
(* mark and save the top scope so it can be emitted in the
   global table of scopes *)

(* TRY-EXCEPT *)
PROCEDURE PushTry     (l_start, l_stop, l_stopBody: CG.Label := CG.No_label;
                       info: CG.Var := NIL;  ex: ESet.T := NIL;
                       msirScope: MSIR.Scope := NIL);
PROCEDURE PushTryElse (l_start, l_stop, l_stopBody: CG.Label := CG.No_label;
                       info: CG.Var := NIL;  msirElse: MSIR.Block := NIL;
                       msirScope: MSIR.Scope := NIL);
(* The C walk passes the labels and info; the MSIR walk passes the ELSE
   code, where an EXIT or RETURN in the protected body goes, and the EH
   scope of the protected body (as for PushTry and PushFinally). *)

(* TRY-FINALLY *)
PROCEDURE PushFinally     (l_start, l_stop, l_stopBody: CG.Label := CG.No_label;
                           info: CG.Var := NIL;  msirScope: MSIR.Scope := NIL);
PROCEDURE PushFinallyProc (l_start, l_stop: CG.Label;  info: CG.Var;
                           handler: CG.Proc;  h_level: INTEGER);
PROCEDURE PopFinally      (VAR(*OUT*) returnSeen, exitSeen: BOOLEAN);

(* LOCK-END *)
PROCEDURE PushLock (l_start, l_stop: CG.Label;  mutex: CG.Var);

(* LOOP-EXIT *)
PROCEDURE PushExit (l_stop: CG.Label := CG.No_label;
                    msirExit: MSIR.Block := NIL);
(* The C walk passes the loop's exit label; the MSIR walk its exit block. *)
PROCEDURE ExitOK   (): BOOLEAN;

(* TRY-PASSING (RAISES) *)
PROCEDURE PushRaises (l_start, l_stop: CG.Label;  ex: ESet.T;  info: CG.Var);

(* PROCEDURE-RETURN *)
PROCEDURE PushProcedure (t: Type.T;  v: Variable.T;  cc: CG.CallingConvention);
PROCEDURE ReturnVar     (VAR(*OUT*) t: Type.T;  VAR(*OUT*) v: Variable.T);
PROCEDURE ReturnOK      (): BOOLEAN;

(* code generation *)
PROCEDURE EmitExit ();
PROCEDURE EmitExitMSIR ();
(* As EmitExit, for the MSIR walk: to the innermost loop's exit block, or
   an enclosing TRY EXCEPT ELSE's ELSE code if nearer.  FINALLY and LOCK
   code on the way runs through the EH scopes (D26). *)
PROCEDURE EmitReturnMSIR (v: MSIR.Value);
(* As EmitReturn, for the MSIR walk, with v the result (NIL for none): to an
   enclosing TRY EXCEPT ELSE's ELSE code, which drops v, or out of the
   procedure. *)
PROCEDURE AllocReturnTemp ();
PROCEDURE EmitReturn (expr: Expr.T;  fromFinally: BOOLEAN);
PROCEDURE EmitScopeTable (doEmit : BOOLEAN := FALSE): INTEGER;
PROCEDURE EmitExceptionTest (signature: Type.T;  need_value: BOOLEAN): CG.Val;
PROCEDURE NextHandler (VAR(*OUT*) handler: CG.Label;
                       VAR(*OUT*) handler_body: CG.Label;
                       VAR(*OUT*) info: CG.Var): BOOLEAN;
PROCEDURE PushFrame (frame: CG.Var;  class: M3RT.HandlerClass);
PROCEDURE PopFrame (frame: CG.Var);
(* generate code to link and unlink 'frame' from the global
   stack of exception frames *)

PROCEDURE SetLock (acquire: BOOLEAN;  var: CG.Var;  offset: INTEGER);
(* generate the call to acquire or release a mutex *)

PROCEDURE CaptureState (frame: CG.Var;  jmpbuf: CG.Var;  handler: CG.Label);
(* frame.jmpbuf = jmpbuf
   if (setjmp(jmpbuf)) goto handler
   or
   if (sigsetjmp(jmpbuf, 0)) goto handler
*)

PROCEDURE Reset ();

END Marker.
