(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

GENERIC MODULE AtomicSequential(Rep, Impl);

PROCEDURE IsLockFree (): BOOLEAN =
  BEGIN
    RETURN Impl.IsLockFree()
  END IsLockFree;

PROCEDURE Store (VAR var: T; val: Rep.T; <*UNUSED*>order: Order) =
  BEGIN
    Impl.Store (var, val, Order.Sequential);
  END Store;

PROCEDURE Load (READONLY var: T; <*UNUSED*>order: Order): Rep.T =
  BEGIN
    RETURN Impl.Load (var, Order.Sequential);
  END Load;

PROCEDURE Swap (VAR var: T; val: Rep.T; <*UNUSED*>order: Order): Rep.T =
  BEGIN
    RETURN Impl.Swap (var, val, Order.Sequential);
  END Swap;

PROCEDURE CompareSwap (VAR var: T; VAR expected: Rep.T; desired: Rep.T;
                       <*UNUSED*>success, failure: Order): BOOLEAN =
  BEGIN
    RETURN Impl.CompareSwap (var, expected, desired, Order.Sequential, Order.Sequential);
  END CompareSwap;

PROCEDURE Fence (<*UNUSED*>order: Order) =
  BEGIN
    Impl.Fence (Order.Sequential);
  END Fence;

PROCEDURE FetchInc (VAR var: T; incr: INTEGER; <*UNUSED*>order: Order): Rep.T =
  BEGIN
    RETURN Impl.FetchInc (var, incr, Order.Sequential);
  END FetchInc;

PROCEDURE FetchDec (VAR var: T; decr: INTEGER; <*UNUSED*>order: Order): Rep.T =
  BEGIN
    RETURN Impl.FetchDec (var, decr, Order.Sequential);
  END FetchDec;

PROCEDURE FetchOr (VAR var: T; mask: Rep.T; <*UNUSED*>order: Order): Rep.T =
  BEGIN
    RETURN Impl.FetchOr (var, mask, Order.Sequential);
  END FetchOr;

PROCEDURE FetchXor (VAR var: T; mask: Rep.T; <*UNUSED*>order: Order): Rep.T =
  BEGIN
    RETURN Impl.FetchXor (var, mask, Order.Sequential);
  END FetchXor;

PROCEDURE FetchAnd (VAR var: T; mask: Rep.T; <*UNUSED*>order: Order): Rep.T =
  BEGIN
    RETURN Impl.FetchAnd (var, mask, Order.Sequential);
  END FetchAnd;

BEGIN
END AtomicSequential.
