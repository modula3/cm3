(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: GenModule.mg                                          *)
(* Last Modified On Tue Mar  2 09:13:12 PST 1993 By kalsow     *)
(*      Modified On Thu Jul 27 17:10:39 1989 By muller         *)

GENERIC MODULE GenModule (Rep,
                          Plus, Times, Minus, Divide, Mod,
                          LT, LE, GT, GE, And, Or, Xor, Not,
                          Shift, Rotate, Extract, Insert);

IMPORT Scope, Tipe, Module, Int, IntegerExpr, Constant, Target, TInt,
       NamedType;
FROM Rep IMPORT T;

PROCEDURE Initialize (name: TEXT) =
  VAR zz: Scope.T;  size: Target.Int;  b: BOOLEAN;
  BEGIN
    b := TInt.FromInt (Rep.size, Target.Integer.bytes, size);
    <*ASSERT b*>
    M := Module.NewDefn (name, TRUE, NIL);

    (* WARNING: The following list must be in the same order
        as the actual GenWord.ig file, otherwise the version
        stamps will be messed up! *)

    zz := Scope.Push (Module.ExportScope (M));
    Tipe.Define ("T", NamedType.New (T), FALSE);
    Constant.Declare ("Size", IntegerExpr.New (Int.T, size), FALSE);
    Plus.Initialize ();
    Times.Initialize ();
    Minus.Initialize ();
    Divide.Initialize ();
    Mod.Initialize ();
    LT.Initialize ();
    LE.Initialize ();
    GT.Initialize ();
    GE.Initialize ();
    And.Initialize ();
    Or.Initialize ();
    Xor.Initialize ();
    Not.Initialize ();
    Shift.Initialize ();
    Rotate.Initialize ();
    Extract.Initialize ();
    Insert.Initialize ();
    Scope.Pop (zz);
  END Initialize;

BEGIN
END GenModule.
