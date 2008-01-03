(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Thu Jul 15 15:43:43 PDT 1993 by swart    *)

UNSAFE MODULE Capability;

IMPORT Random, TimeStamp, Thread, Swap;

EXCEPTION Failure;
<*FATAL Failure*>

VAR state := NEW(Thread.Mutex OBJECT r: Random.T := NIL; END);

PROCEDURE New (): T =
  VAR t: T;
  BEGIN
    t.ts := TimeStamp.New();
    LOCK state DO
      IF state.r = NIL THEN state.r := NEW(Random.Default).init() END;
      IF BYTESIZE(INTEGER) = 4 THEN
        t.random := LOOPHOLE(Swap.Int64On32{a := state.r.integer(), b :=
                                            state.r.integer()},
                             ARRAY [0 .. 7] OF BITS 8 FOR [0 .. 255]);
      ELSIF BYTESIZE(INTEGER) = 8 THEN
        t.random := LOOPHOLE(Swap.Int64On64{v := state.r.integer()},
                             ARRAY [0 .. 7] OF BITS 8 FOR [0 .. 255]);
      ELSE
        RAISE Failure;
      END;
    END;
    RETURN t;
  END New;

PROCEDURE Equal(READONLY t1, t2: T): BOOLEAN =
  BEGIN RETURN t1 = t2; END Equal;

PROCEDURE Hash (READONLY t: T): INTEGER =
  VAR i: Swap.Int64On32;
  BEGIN
    LOOPHOLE(i, ARRAY [0 .. 7] OF BITS 8 FOR [0 .. 255]) := t.random;
    IF Swap.endian = Swap.Endian.Little THEN
      RETURN Swap.Swap4(i.a);
    ELSE
      RETURN i.a;
    END;
  END Hash;

BEGIN
END Capability.
