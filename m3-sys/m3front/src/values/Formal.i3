(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Formal.i3                                             *)
(* Last Modified On Mon Dec  5 15:19:54 PST 1994 By kalsow     *)
(*      Modified On Wed Feb  7 01:02:52 1990 By muller         *)

INTERFACE Formal;

IMPORT M3ID, Type, Value, Expr, Tracer;

TYPE Mode = {mVALUE, mVAR, mCONST};

TYPE
  Info = RECORD
    name   : M3ID.T;
    mode   : Mode;
    offset : INTEGER;
    type   : Type.T;
    dfault : Expr.T;
    unused : BOOLEAN;
    trace  : Tracer.T;
  END;

PROCEDURE New (READONLY info: Info): Value.T;

PROCEDURE NewBuiltin (name: TEXT;  offset: INTEGER;  type: Type.T): Value.T;

PROCEDURE Split (formal: Value.T;  VAR info: Info);

PROCEDURE HasClosure (formal: Value.T): BOOLEAN;

PROCEDURE OpenArrayByVALUE (formal: Value.T;  VAR refType: Type.T): BOOLEAN;
(* If 'formal' is a "VALUE ARRAY OF X" formal, sets 'refType' to "REF ARRAY OF X"
   and returns TRUE, otherwise returns FALSE. *)

PROCEDURE CheckArgs (VAR cs       : Value.CheckState;
                     VAR actuals  : Expr.List;
                         formals  : Value.T;
                         proc     : Expr.T): BOOLEAN;

PROCEDURE PrepArg (formalValue: Value.T;  actual: Expr.T);
PROCEDURE EmitArg (proc: Expr.T;  formalValue: Value.T;  actual: Expr.T);
(* generate code to pass 'actual' as a 'formal'.  *)

PROCEDURE EmitDeclaration (formal: Value.T;  types_only, param: BOOLEAN);

PROCEDURE GenCopy (t: Type.T);
(* Load the address of a temporary that holds the value of type 't'
   that is currently on top of the stack. *)

END Formal.
