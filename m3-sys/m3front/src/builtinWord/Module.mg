(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

GENERIC MODULE Module (Rep,
                       Plus, Times, Minus, Divide, Mod,
                       LT, LE, GT, GE, And, Or, Xor, Not,
                       Shift, Rotate, Extract, Insert);

IMPORT Scope, Tipe, Module, Int, IntegerExpr, Constant, Target, TInt,
       NamedType, Type, TWord;
FROM Rep IMPORT T;
FROM TargetMap IMPORT Word_types;

PROCEDURE SetRep (): INTEGER =
  VAR min, max: Target.Int;  b: BOOLEAN;
  BEGIN
    b := Type.GetBounds (T, min, max);  <*ASSERT b*>
    FOR i := FIRST (Word_types) TO LAST (Word_types) DO
      WITH t = Word_types[i] DO
        IF TWord.LE (max, t.max) THEN
          RETURN i;
        END;
      END;
    END;
    RETURN LAST (Word_types);
  END SetRep;

PROCEDURE Initialize (name: TEXT) =
  VAR zz: Scope.T;  size: Target.Int;  b: BOOLEAN;  rep := SetRep ();
  BEGIN
    b := TInt.FromInt (Word_types[rep].size, size);  <*ASSERT b*>
    M := Module.NewDefn (name, TRUE, NIL);

    (* WARNING: The following list must be in the same order
        as the actual GenWord.ig file, otherwise the version
        stamps will be messed up! *)

    zz := Scope.Push (Module.ExportScope (M));
    Tipe.Define ("T", NamedType.New (T), FALSE);
    EVAL Constant.Declare ("Size", IntegerExpr.New (Int.T, size), FALSE);
    Plus.Initialize (rep);
    Times.Initialize (rep);
    Minus.Initialize (rep);
    Divide.Initialize (rep);
    Mod.Initialize (rep);
    LT.Initialize (rep);
    LE.Initialize (rep);
    GT.Initialize (rep);
    GE.Initialize (rep);
    And.Initialize (rep);
    Or.Initialize (rep);
    Xor.Initialize (rep);
    Not.Initialize (rep);
    Shift.Initialize (rep);
    Rotate.Initialize (rep);
    Extract.Initialize (rep);
    Insert.Initialize (rep);
    Scope.Pop (zz);
  END Initialize;

BEGIN
END Module.
