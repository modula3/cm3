(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Scope.i3                                              *)
(* Last modified on Fri Jun 24 16:51:59 PDT 1994 by kalsow     *)
(*      modified on Thu Aug  9 20:08:18 1990 by muller         *)

INTERFACE Scope;

IMPORT M3, M3ID, M3Buf, Value;
FROM M3 IMPORT QID;

TYPE
  T = M3.Scope;

VAR
  Initial: T;

PROCEDURE PopNew ();
PROCEDURE PushNew (open: BOOLEAN;  name: M3ID.T;
                   module := FALSE;  nested := FALSE): T;

PROCEDURE New1 (obj: Value.T): T;

PROCEDURE Push (t: T): T;
PROCEDURE Pop  (old: T);
PROCEDURE Top  (): T;

PROCEDURE InsertScope (o: Value.T; t: T);
(* Insert o into t. *)

PROCEDURE Insert (o: Value.T);
(* Insert o into Top(). *)

PROCEDURE LookUp    (t: T;  name: M3ID.T;  strict: BOOLEAN): Value.T;
PROCEDURE LookUpQID (t: T;  READONLY q: QID): Value.T;

PROCEDURE ToList     (t: T): Value.T;
PROCEDURE OuterMost  (t: T): BOOLEAN;
PROCEDURE TypeCheck  (t: T;  VAR cs: Value.CheckState);
PROCEDURE WarnUnused (t: T);

PROCEDURE Enter      (t: T);
PROCEDURE InitValues (t: T);
PROCEDURE Exit       (t: T);

TYPE ValueVisitor = PROCEDURE (v: Value.T);

PROCEDURE ForEachValue (t: T;  visit: ValueVisitor);
(* Call visit(v) for each value declared directly in scope t, in declaration
   order.  Used by the MSIR nested-proc inline path (Procedure.GenBodyMSIR) to
   reach a procedure's OWN nested procedures and compile their MSIR bodies
   directly (GenBodyMSIR) — bypassing Value.LangInit, whose
   CG.Note_procedure_origin side effect would corrupt the parallel C backend. *)

PROCEDURE ModuleName (v: Value.T): M3ID.T;
PROCEDURE ToUnit     (v: Value.T): M3.Value (* == Module.T *);

TYPE
  IDStack = RECORD
    top: INTEGER;
    stk: ARRAY [0..99] OF M3ID.T;
  END;
  
PROCEDURE NameToPrefix (v: Value.T;  VAR(*OUT*) p: IDStack;
                        considerExternal := TRUE;
                        dots := FALSE;
                        with_module := TRUE);

PROCEDURE PutStack (mbuf: M3Buf.T;  READONLY s: IDStack);
PROCEDURE StackToText (READONLY s: IDStack): TEXT;

PROCEDURE Initialize ();
PROCEDURE Reset ();

END Scope.
