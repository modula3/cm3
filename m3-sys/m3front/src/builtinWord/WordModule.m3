(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: WordModule.m3                                         *)
(* Last Modified On Tue Mar  2 09:13:12 PST 1993 By kalsow         *)
(*      Modified On Thu Jul 27 17:10:39 1989 By muller         *)

MODULE WordModule;

IMPORT Scope, Tipe, Module, Int, IntegerExpr, Constant, Target, TInt, NamedType;
IMPORT WordPlus, WordMinus, WordTimes, WordLT, WordLE, WordGT, WordGE;
IMPORT WordAnd, WordOr, WordXor, WordShift, WordRotate, WordExtract;
IMPORT WordInsert, WordNot, WordDivide, WordMod;

PROCEDURE Initialize () =
  VAR zz: Scope.T;  size: Target.Int;  b: BOOLEAN;
  BEGIN

    b := TInt.FromInt (Target.Integer.size, size); <*ASSERT b*>
    M := Module.NewDefn ("Word", TRUE, NIL);

    (* WARNING: The following list must be in the same order
        as the actual Word.i3 file, otherwise the version
        stamps will be messed up! *)

    zz := Scope.Push (Module.ExportScope (M));
    Tipe.Define ("T", NamedType.New (Int.T), FALSE);
    Constant.Declare ("Size", IntegerExpr.New (size), FALSE);
    WordPlus.Initialize ();
    WordTimes.Initialize ();
    WordMinus.Initialize ();
    WordDivide.Initialize ();
    WordMod.Initialize ();
    WordLT.Initialize ();
    WordLE.Initialize ();
    WordGT.Initialize ();
    WordGE.Initialize ();
    WordAnd.Initialize ();
    WordOr.Initialize ();
    WordXor.Initialize ();
    WordNot.Initialize ();
    WordShift.Initialize ();
    WordRotate.Initialize ();
    WordExtract.Initialize ();
    WordInsert.Initialize ();
    Scope.Pop (zz);
  END Initialize;

BEGIN
END WordModule.
