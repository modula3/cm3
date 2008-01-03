(* Copyright (C) 1994, Digital Equipment Corporation       *)
(* All rights reserved.                                    *)
(* See the file COPYRIGHT for a full description.          *)
(*                                                         *)
(* Last modified on Mon Nov 21 08:29:15 PST 1994 by kalsow *)

(* This module maintains a pool of free thread stacks *)

UNSAFE MODULE RTThreadStk EXPORTS RTThread;

VAR pool       : ARRAY [0..49] OF Stack;
VAR tos        : CARDINAL := 0;  (* next free pool slot *)
VAR clock      : CARDINAL := 0;

PROCEDURE GetStack (size: INTEGER;  VAR(*OUT*) s: Stack) =
  VAR
    bytes   := size * ADRSIZE (INTEGER);
    best    : INTEGER := -1;
    best_sz : INTEGER;
    sz      : INTEGER;
  BEGIN
    (* check the pool first *)
    FOR p := tos-1 TO FIRST (pool) BY -1 DO
      WITH pp = pool[p] DO
        sz := pp.last - pp.first;
        IF (sz = bytes) THEN
          (* exact match*)
          DEC (tos);
          s  := pp;
          pp := pool[tos];
          pool[tos].words := NIL;
          RETURN;
        ELSIF (sz >= bytes) AND ((best < 0) OR (sz < best_sz)) THEN
          (* a new best match *)
          best := p;  best_sz := sz;
        END;
      END;
    END;

    IF (best >= 0) THEN
      DEC (tos);
      s := pool[best];
      pool[best] := pool[tos];
      pool[tos].words := NIL;
      RETURN;
    END;

    (* nothing in the pool => allocate a fresh stack *)
    NewStack (size, s);
  END GetStack;

PROCEDURE FreeStack (VAR(*IN/OUT*) s: Stack) =
  BEGIN
    IF (tos < NUMBER (pool)) THEN
      (* the pool isn't full *)
      pool[tos] := s;  INC (tos);
      s.words := NIL;
      s.first := NIL;
      s.last  := NIL;
    ELSE
      (* no room in the pool => free an old stack from the pool *)
      IF (clock >= tos) THEN clock := 0 END;
      DisposeStack (pool[clock]);
      pool[clock] := s;
      INC (clock);
    END;
  END FreeStack;

BEGIN
END RTThreadStk.
