(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: LongModule.m3                                         *)
(* Last Modified On Tue Mar  2 09:13:12 PST 1993 By kalsow     *)
(*      Modified On Thu Jul 27 17:10:39 1989 By muller         *)

MODULE LongModule;

IMPORT Scope, Tipe, Module, Int, IntegerExpr, Constant, Target, TInt, NamedType;
IMPORT LongPlus AS Plus, LongMinus AS Minus, LongTimes AS Times;
IMPORT LongLT AS LT, LongLE AS LE, LongGT AS GT, LongGE AS GE;
IMPORT LongAnd AS And, LongOr AS Or, LongXor AS Xor;
IMPORT LongShift AS Shift, LongRotate AS Rotate;
IMPORT LongExtract AS Extract, LongInsert AS Insert;
IMPORT LongNot AS Not, LongDivide AS Divide, LongMod AS Mod;
FROM LInt IMPORT T;

PROCEDURE Initialize () =
  VAR zz: Scope.T;  size: Target.Int;  b: BOOLEAN;
  BEGIN

    b := TInt.FromInt (Target.Longint.size, Target.Integer.bytes, size);
    <*ASSERT b*>
    M := Module.NewDefn ("Long", TRUE, NIL);

    (* WARNING: The following list must be in the same order
        as the actual Long.i3 file, otherwise the version
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
END LongModule.
