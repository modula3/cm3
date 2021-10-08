(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Field.i3                                              *)
(* Last Modified On Fri Jun 24 09:30:30 PDT 1994 By kalsow     *)

INTERFACE Field;

IMPORT M3ID, Type, Value, Expr;

TYPE T <: Value.T;

TYPE
  Info = RECORD
    name   : M3ID.T;
    index  : INTEGER;  (* 0..nFields-1 *)
    offset : INTEGER;  (* bit offset from beginning of record *)
    type   : Type.T;
    dfault : Expr.T;
  END;

PROCEDURE New (READONLY info: Info): Value.T;

PROCEDURE SetOffset (field: Value.T;  newOffset: INTEGER);

PROCEDURE Is (v: Value.T): BOOLEAN;
(* Returns TRUE iff 'v' is a field *)

PROCEDURE Split (field: Value.T;  VAR info: Info);

PROCEDURE NameAnonConstr (VAR (*IN OUT*) o: Value.T; VAR cs: Value.CheckState);

PROCEDURE Compile (field: T);
(* compile, i.e. declare, the underlying type *)

PROCEDURE EmitDeclaration (field: Value.T);
(* emit the C struct member or bit-field for 'field' *)

PROCEDURE IsEqualList (va, vb: Value.T;  x: Type.Assumption;
                       types: BOOLEAN): BOOLEAN;
(* Returns "TRUE" if the two lists of values represented by "va" and "vb"
   have the same length and for each pair of values "a" and "b",
   "IsEqual(a, b, x, types)" returns "TRUE".  Otherwise, returns "FALSE". *)

PROCEDURE IsEqual (va, vb: Value.T;  x: Type.Assumption;  types: BOOLEAN): BOOLEAN;
(* If "types" is "FALSE", only the surface syntax (name & field index) are
   checked.  Otherwise, the field types and default values are checked too. *)

END Field.
