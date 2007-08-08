(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: LongModule.m3                                         *)
(* Last Modified On Tue Mar  2 09:13:12 PST 1993 By kalsow     *)
(*      Modified On Thu Jul 27 17:10:39 1989 By muller         *)

MODULE LongModule;

IMPORT Scope, Tipe, Module, LInt, IntegerExpr, Constant, Target, TInt, NamedType;
IMPORT LongPlus, LongMinus, LongTimes, LongLT, LongLE, LongGT, LongGE;
IMPORT LongAnd, LongOr, LongXor, LongShift, LongRotate, LongExtract;
IMPORT LongInsert, LongNot, LongDivide, LongMod;

PROCEDURE Initialize () =
  VAR zz: Scope.T;  size: Target.Int;  b: BOOLEAN;
  BEGIN

    b := TInt.FromInt (Target.Longint.size, Target.Pre.Integer, size);
    <*ASSERT b*>
    M := Module.NewDefn ("Long", TRUE, NIL);

    (* WARNING: The following list must be in the same order
        as the actual Long.i3 file, otherwise the version
        stamps will be messed up! *)

    zz := Scope.Push (Module.ExportScope (M));
    Tipe.Define ("T", NamedType.New (LInt.T), FALSE);
    Constant.Declare ("Size", IntegerExpr.New (size), FALSE);
    LongPlus.Initialize ();
    LongTimes.Initialize ();
    LongMinus.Initialize ();
    LongDivide.Initialize ();
    LongMod.Initialize ();
    LongLT.Initialize ();
    LongLE.Initialize ();
    LongGT.Initialize ();
    LongGE.Initialize ();
    LongAnd.Initialize ();
    LongOr.Initialize ();
    LongXor.Initialize ();
    LongNot.Initialize ();
    LongShift.Initialize ();
    LongRotate.Initialize ();
    LongExtract.Initialize ();
    LongInsert.Initialize ();
    Scope.Pop (zz);
  END Initialize;

BEGIN
END LongModule.
