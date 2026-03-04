(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

MODULE ESCProve;

IMPORT RefList;

PROCEDURE Init() =
  BEGIN
    (* Phase 2+: Prover.Init(), load esc.ax *)
  END Init;

PROCEDURE Check(<* UNUSED *> bgPush: RefList.T;
                <* UNUSED *> vc: RefList.T): Result =
  BEGIN
    RETURN NEW(Result, valid := TRUE, labels := NIL);
  END Check;

BEGIN
END ESCProve.
