(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: TypeRep.i3                                            *)
(* Last Modified On Wed Nov 16 14:38:18 PST 1994 by kalsow     *)
(*      Modified On Fri Dec 21 00:53:14 1990 by muller         *)

INTERFACE TypeRep;

IMPORT M3, M3FP, Type;

CONST
  NO_UID   = -1;
  NO_SCC   = 0;

TYPE
  TT = M3.Type;

REVEAL
  M3.Type = M3.Node BRANDED "Type.T" OBJECT
    fp          : M3FP.T;
    info        : Type.Info;
    uid         : INTEGER;
    scc_id      : INTEGER;
    rep_id      : INTEGER;
    checkDepth  : BITS 12 FOR [-2048..2047];
    checked     : M3.Flag;
    errored     : M3.Flag;
    next        : TT;   (* linked list of all types in the same module *)
  METHODS
    check       ();
    check_align (offset: INTEGER): BOOLEAN;
    isEqual     (t: TT;  x: Type.Assumption): BOOLEAN;
    isSubtype   (t: TT): BOOLEAN := NoSubtypes;
    compile     ();
    initCost    (zeroed: BOOLEAN): INTEGER;
    initValue   (zeroed: BOOLEAN);
    mapper      (offset, size: INTEGER; refs: BOOLEAN);
    gen_desc    ();
    fprint      (VAR x: M3.FPInfo);
  END;

PROCEDURE Init (t: TT;  c: Type.Class);
(* initialize the shared fields of a Type.T *)

PROCEDURE NeverEqual (a, b: TT;  x: Type.Assumption): BOOLEAN;
(* == RETURN FALSE *)

PROCEDURE NoSubtypes (a, b: TT): BOOLEAN;
(* == RETURN FALSE *)

PROCEDURE InitToZeros (t: TT;  zeroed: BOOLEAN);
(* == initialize Size(t) bits to zero *)

PROCEDURE GenRefMap (t: TT;  offset, size: INTEGER;  refs_only: BOOLEAN);
(* == TypeMap.Add (offset, Op.{Untraced}Ref, 0) *)

PROCEDURE GenRefDesc (t: TT);
(* == TypeDesc.AddO (Op.{Untraced}Ref); TypeDesc.AddI (UID(t)) *)

PROCEDURE ScalarAlign (t: TT;  offset: INTEGER): BOOLEAN;
(* == RETURN (offset MOD t.alignment = 0) *)

END TypeRep.
