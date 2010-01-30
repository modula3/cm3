(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: AtomicModule.mg                                       *)

GENERIC MODULE AtomicModule (Rep,
                             IsLockFree,
                             Store, Load, Swap, CompareSwap,
                             Fence,
                             FetchInc, FetchDec, FetchOr, FetchXor, FetchAnd);

IMPORT Scope, Tipe, Module, TInt;
IMPORT Field, PackedType, M3ID, TargetMap, Type, Value, RecordType;
IMPORT EnumType, EnumExpr, EnumElt;

PROCEDURE Initialize (name: TEXT) =
  VAR zz, elts: Scope.T;  bits: Field.Info;  cs: Value.CheckState;
  BEGIN
    M := Module.NewDefn (name, TRUE, NIL);

    (* WARNING: The following list must be in the same order
        as the actual Atomic.ig file, otherwise the version
        stamps will be messed up! *)

    zz := Scope.Push (Module.ExportScope (M));

    elts := Scope.PushNew (FALSE, M3ID.NoID);
    bits :=
        Field.Info { name := M3ID.Add ("bits"),
                     index := 0,
                     offset := 0,
                     type := PackedType.New (
                                 TargetMap.CG_Size [Type.CGType (
                                                        Rep.T,
                                                        in_memory := TRUE)],
                                 Rep.T),
                     dfault := NIL };
    Scope.Insert (Field.New (bits));
    Scope.PopNew ();
    Scope.TypeCheck (elts, cs);
    T := RecordType.New (elts);
    Tipe.Define ("T", T, FALSE);

    elts := Scope.PushNew (FALSE, M3ID.NoID);
    Order := EnumType.New (5, elts);

    Scope.Insert (EnumElt.New (M3ID.Add ("Relaxed"),        TInt.Zero,  Order));
    Scope.Insert (EnumElt.New (M3ID.Add ("Release"),        TInt.One,   Order));
    Scope.Insert (EnumElt.New (M3ID.Add ("Acquire"),        TInt.Two,   Order));
    Scope.Insert (EnumElt.New (M3ID.Add ("AcquireRelease"), TInt.Three, Order));
    Scope.Insert (EnumElt.New (M3ID.Add ("Sequential"),     TInt.Four,  Order));

    Scope.PopNew ();
    Scope.TypeCheck (elts, cs);
    Tipe.Define ("Order", Order, FALSE);

    Sequential := EnumExpr.New (Order, TInt.Four);

    IsLockFree.Initialize ();
    Store.Initialize ();
    Load.Initialize ();
    Swap.Initialize ();
    CompareSwap.Initialize ();
    Fence.Initialize ();
    FetchInc.Initialize ();
    FetchDec.Initialize ();
    FetchOr.Initialize ();
    FetchXor.Initialize ();
    FetchAnd.Initialize ();
    Scope.Pop (zz);
  END Initialize;

BEGIN
END AtomicModule.
