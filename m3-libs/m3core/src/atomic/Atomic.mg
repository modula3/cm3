(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

GENERIC MODULE Atomic(Rep,Impl);

PROCEDURE IsLockFree (): BOOLEAN =
  BEGIN
    RETURN Impl.IsLockFree()
  END IsLockFree;

PROCEDURE Store (VAR var: T; val: Rep.T; order: Order) =
  BEGIN
    CASE order OF <*NOWARN*>
    | Order.Relaxed    => Impl.Store (var, val, Order.Relaxed);
(*
    | Order.Release    => Impl.Store (var, val, Order.Release);
    | Order.Sequential => Impl.Store (var, val, Order.Sequential);
*)
    END;
  END Store;

PROCEDURE Load (READONLY var: T; order: Order): Rep.T =
  BEGIN
    CASE order OF <*NOWARN*>
    | Order.Relaxed    => RETURN Impl.Load (var, Order.Relaxed);
(*
    | Order.Acquire    => RETURN Impl.Load (var, Order.Acquire);
    | Order.Sequential => RETURN Impl.Load (var, Order.Sequential);
*)
    END;
  END Load;

PROCEDURE Swap (VAR var: T; val: Rep.T; order: Order): Rep.T =
  BEGIN
    CASE order OF
    | Order.Relaxed        => RETURN Impl.Swap (var, val, Order.Relaxed);
    | Order.Release        => RETURN Impl.Swap (var, val, Order.Release);
    | Order.Acquire        => RETURN Impl.Swap (var, val, Order.Acquire);
    | Order.AcquireRelease => RETURN Impl.Swap (var, val, Order.AcquireRelease);
    | Order.Sequential     => RETURN Impl.Swap (var, val, Order.Sequential);
    END;
  END Swap;

PROCEDURE CompareSwap (VAR var: T; VAR expected: Rep.T; desired: Rep.T;
                       success, failure: Order): BOOLEAN =
  BEGIN
    CASE failure OF <*NOWARN*>
    | Order.Relaxed =>
      CASE success OF
      | Order.Relaxed =>
        RETURN Impl.CompareSwap (var, expected, desired,
                                 Order.Relaxed, Order.Relaxed);
      | Order.Release =>
        RETURN Impl.CompareSwap (var, expected, desired,
                                 Order.Release, Order.Relaxed);
      | Order.Acquire =>
        RETURN Impl.CompareSwap (var, expected, desired,
                                 Order.Acquire, Order.Relaxed);
      | Order.AcquireRelease =>
        RETURN Impl.CompareSwap (var, expected, desired,
                                 Order.AcquireRelease, Order.Relaxed);
      | Order.Sequential =>
        RETURN Impl.CompareSwap (var, expected, desired,
                                 Order.Sequential, Order.Relaxed);
      END;
    | Order.Acquire =>
      CASE success OF <*NOWARN*>
      | Order.Acquire =>
        RETURN Impl.CompareSwap (var, expected, desired,
                                 Order.Acquire, Order.Acquire);
      | Order.AcquireRelease =>
        RETURN Impl.CompareSwap (var, expected, desired,
                                 Order.AcquireRelease, Order.Acquire);
      | Order.Sequential =>
        RETURN Impl.CompareSwap (var, expected, desired,
                                 Order.Sequential, Order.Acquire);
      END;
    | Order.Sequential =>
      RETURN Impl.CompareSwap (var, expected, desired,
                               Order.Sequential, Order.Sequential);
    END;
  END CompareSwap;

PROCEDURE Fence (order: Order) =
  BEGIN
    CASE order OF
    | Order.Relaxed         => Impl.Fence (Order.Relaxed);
    | Order.Release         => Impl.Fence (Order.Release);
    | Order.Acquire         => Impl.Fence (Order.Acquire);
    | Order.AcquireRelease  => Impl.Fence (Order.AcquireRelease);
    | Order.Sequential      => Impl.Fence (Order.Sequential);
    END;
  END Fence;

PROCEDURE FetchInc (VAR var: T; incr: INTEGER; order: Order): Rep.T =
  BEGIN
    CASE order OF
    | Order.Relaxed =>
      RETURN Impl.FetchInc (var, incr, Order.Relaxed);
    | Order.Release =>
      RETURN Impl.FetchInc (var, incr, Order.Release);
    | Order.Acquire =>
      RETURN Impl.FetchInc (var, incr, Order.Acquire);
    | Order.AcquireRelease =>
      RETURN Impl.FetchInc (var, incr, Order.AcquireRelease);
    | Order.Sequential =>
      RETURN Impl.FetchInc (var, incr, Order.Sequential);
    END;
  END FetchInc;

PROCEDURE FetchDec (VAR var: T; decr: INTEGER; order: Order): Rep.T =
  BEGIN
    CASE order OF
    | Order.Relaxed =>
      RETURN Impl.FetchDec (var, decr, Order.Relaxed);
    | Order.Release =>
      RETURN Impl.FetchDec (var, decr, Order.Release);
    | Order.Acquire =>
      RETURN Impl.FetchDec (var, decr, Order.Acquire);
    | Order.AcquireRelease =>
      RETURN Impl.FetchDec (var, decr, Order.AcquireRelease);
    | Order.Sequential =>
      RETURN Impl.FetchDec (var, decr, Order.Sequential);
    END;
  END FetchDec;

PROCEDURE FetchOr (VAR var: T; mask: Rep.T; order: Order): Rep.T =
  BEGIN
    CASE order OF
    | Order.Relaxed =>
      RETURN Impl.FetchOr (var, mask, Order.Relaxed);
    | Order.Release =>
      RETURN Impl.FetchOr (var, mask, Order.Release);
    | Order.Acquire =>
      RETURN Impl.FetchOr (var, mask, Order.Acquire);
    | Order.AcquireRelease =>
      RETURN Impl.FetchOr (var, mask, Order.AcquireRelease);
    | Order.Sequential =>
      RETURN Impl.FetchOr (var, mask, Order.Sequential);
    END;
  END FetchOr;

PROCEDURE FetchXor (VAR var: T; mask: Rep.T; order: Order): Rep.T =
  BEGIN
    CASE order OF
    | Order.Relaxed =>
      RETURN Impl.FetchXor (var, mask, Order.Relaxed);
    | Order.Release =>
      RETURN Impl.FetchXor (var, mask, Order.Release);
    | Order.Acquire =>
      RETURN Impl.FetchXor (var, mask, Order.Acquire);
    | Order.AcquireRelease =>
      RETURN Impl.FetchXor (var, mask, Order.AcquireRelease);
    | Order.Sequential =>
      RETURN Impl.FetchXor (var, mask, Order.Sequential);
    END;
  END FetchXor;

PROCEDURE FetchAnd (VAR var: T; mask: Rep.T; order: Order): Rep.T =
  BEGIN
    CASE order OF
    | Order.Relaxed =>
      RETURN Impl.FetchAnd (var, mask, Order.Relaxed);
    | Order.Release =>
      RETURN Impl.FetchAnd (var, mask, Order.Release);
    | Order.Acquire =>
      RETURN Impl.FetchAnd (var, mask, Order.Acquire);
    | Order.AcquireRelease =>
      RETURN Impl.FetchAnd (var, mask, Order.AcquireRelease);
    | Order.Sequential =>
      RETURN Impl.FetchAnd (var, mask, Order.Sequential);
    END;
  END FetchAnd;

BEGIN
END Atomic.
