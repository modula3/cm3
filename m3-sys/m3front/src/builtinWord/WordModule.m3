(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: WordModule.m3                                         *)
(* Last Modified On Tue Mar  2 09:13:12 PST 1993 By kalsow     *)
(*      Modified On Thu Jul 27 17:10:39 1989 By muller         *)

MODULE WordModule;

IMPORT Scope, Tipe, Module, Int, IntegerExpr, Constant, Target, TInt, NamedType;
IMPORT WordPlus AS Plus, WordMinus AS Minus, WordTimes AS Times;
IMPORT WordLT AS LT, WordLE AS LE, WordGT AS GT, WordGE AS GE;
IMPORT WordAnd AS And, WordOr AS Or, WordXor AS Xor;
IMPORT WordShift AS Shift, WordRotate AS Rotate;
IMPORT WordExtract AS Extract, WordInsert AS Insert;
IMPORT WordNot AS Not, WordDivide AS Divide, WordMod AS Mod;
FROM Int IMPORT T;

PROCEDURE Initialize () =
  VAR zz: Scope.T;  size: Target.Int;  b: BOOLEAN;
  BEGIN

    b := TInt.FromInt (Target.Integer.size, Target.Integer.bytes, size);
    <*ASSERT b*>
    M := Module.NewDefn ("Word", TRUE, NIL);

    (* WARNING: The following list must be in the same order
        as the actual Word.i3 file, otherwise the version
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
END WordModule.
