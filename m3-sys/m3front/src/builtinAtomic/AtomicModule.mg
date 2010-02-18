(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: AtomicModule.mg                                       *)

GENERIC MODULE AtomicModule (Rep,
                             IsLockFree,
                             Store, Load, Swap, CompareSwap,
                             Fence,
                             FetchInc, FetchDec, FetchOr, FetchXor, FetchAnd);

IMPORT Scope, Tipe, Module, Target;
IMPORT Field, M3ID, Value, RecordType;
IMPORT EnumType, EnumExpr, EnumElt;

PROCEDURE Initialize (name: TEXT) =
  CONST
    Zero  = Target.Int{0,0,..};
    One   = Target.Int{1,0,..};
    Two   = Target.Int{2,0,..};
    Three = Target.Int{3,0,..};
    Four  = Target.Int{4,0,..};
  VAR zz, elts: Scope.T;  rep: Field.Info;  cs: Value.CheckState;
  BEGIN
    M := Module.NewDefn (name, TRUE, NIL);

    (* WARNING: The following list must be in the same order
        as the actual Atomic.ig file, otherwise the version
        stamps will be messed up! *)

    zz := Scope.Push (Module.ExportScope (M));

    elts := Scope.PushNew (FALSE, M3ID.NoID);
    rep :=
        Field.Info { name := M3ID.Add ("rep"),
                     index := 0,
                     offset := 0,
                     type := Rep.T,
                     dfault := NIL };
    Scope.Insert (Field.New (rep));
    Scope.PopNew ();
    Scope.TypeCheck (elts, cs);
    T := RecordType.New (elts);
    Tipe.Define ("T", T, FALSE);

    elts := Scope.PushNew (FALSE, M3ID.NoID);
    Order := EnumType.New (5, elts);
    Scope.Insert (EnumElt.New (M3ID.Add ("Relaxed"),        Zero,  Order));
    Scope.Insert (EnumElt.New (M3ID.Add ("Release"),        One,   Order));
    Scope.Insert (EnumElt.New (M3ID.Add ("Acquire"),        Two,   Order));
    Scope.Insert (EnumElt.New (M3ID.Add ("AcquireRelease"), Three, Order));
    Scope.Insert (EnumElt.New (M3ID.Add ("Sequential"),     Four,  Order));
    Scope.PopNew ();
    Scope.TypeCheck (elts, cs);
    Tipe.Define ("Order", Order, FALSE);

    Sequential := EnumExpr.New (Order, Four);

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
