(* Copyright (C) 2005, Purdue Research Foundation                  *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT-PURDUE for a full description.           *)

(* This experimental interface mirrors <stdatomic.h> in the proposed C1x
   standard: http://www.open-std.org/JTC1/SC22/wg14/www/docs/n1425.pdf *)

GENERIC INTERFACE Atomic(Rep);

TYPE T = Rep.T;

TYPE
  Order = { Relaxed, Release, Acquire, AcquireRelease, Sequential };
  (* "Relaxed": The operation does not order memory.

     "Release": Performs a release operation on the affected memory locations,
     thus making regular memory writes visible to other threads through the
     variable to which it is applied.

     "Acquire": Performs an acquire operation on the affected memory
     locations, thus making regular memory writes in other threads released
     through the atomic variable to which it is applied, visible to the
     current thread.

     "AcquireRelease": The operation has both acquire and release semantics.

     "Sequential": The operation has both acquire and release semantics, and
     in addition, has sequentially-consistent operation ordering. *)

PROCEDURE IsLockFree(): BOOLEAN;
  (* True if the operations on T are lock-free, false otherwise. *)

PROCEDURE Store(VAR var: T; val: Rep.T; order := Order.Sequential);
  (* Atomically replace the value in "var" with "val".  Memory is affected as
     per "order".  The "order" shall be neither "Acquire" nor
     "AcquireRelease". *)

PROCEDURE Load(READONLY var: T; order := Order.Sequential): Rep.T;
  (* Atomically return the value in "var".  Memory is affected as per "order".
     The "order" shall be neither "Release" nor "AcquireRelease". *)

PROCEDURE Swap(VAR var: T; val: Rep.T; order := Order.Sequential): Rep.T;
  (* Atomically replace the value in "var" with "val". Returns the value of
     "var" immediately before the effects. Memory is affected as per order.
     This is a read-modify-write operation and synchronizes with any
     evaluation that reads the updated value. *)

PROCEDURE CompareSwap(VAR var: T; VAR expected: Rep.T; desired: Rep.T;
                      order := Order.Sequential): BOOLEAN;
  (* Atomically, compares the value in "var" for equality with that in
     "expected", and if true, replaces the value in "var" with "desired", and
     if false, updates the value in "expected" with the value in "var".
     Returns the result of the comparison.  The "order" shall be neither
     "Release" nor "AcquireRelease".  This is a read-modify-write operation
     and synchronizes with any evaluation that reads the updated value.  The
     effect of the CompareSwap operation is:

     IF var = expected THEN var := desired ELSE expected := var;

     The CompareSwap operation may fail spuriously, that is return false while
     leaving the value in "expected" unchanged.  A consequence of spurious
     failure is that nearly all uses of CompareSwap will be in a loop:

     expected := Atomic.Load(current);
     DO
       desired := function(expected);
     WHILE NOT Atomic.CompareSwap(current, expected, desired);
  *)

PROCEDURE Fence(order := Order.Sequential);
  (* Memory is affected as per "order".
     Has no effects if "order" is "Relaxed". *)

PROCEDURE FetchInc (VAR var: T; incr := 1; order := Order.Sequential): T;
PROCEDURE FetchDec (VAR var: T; decr := 1; order := Order.Sequential): T;
PROCEDURE FetchOr  (VAR var: T; mask: T; order := Order.Sequential): T;
PROCEDURE FetchXOr (VAR var: T; mask: T; order := Order.Sequential): T;
PROCEDURE FetchAnd (VAR var: T; mask: T; order := Order.Sequential): T;
  (* Atomically replace the value in "var" with the result of the operation
     applied to the value in "var" and the given operand.  Memory is affected
     as per "order".  These operations are read-modify-write operations and
     synchronize with any evaluation that reads the updated value.  Returns
     the value of "var" immediately before the effects.

     For signed integral types, arithmetic is defined to use two's-complement
     representation.  There are no undefined results.  For address types, the
     result may be an undefined address, but the operations otherwise have no
     undefined behavior. *)

END Atomic.
