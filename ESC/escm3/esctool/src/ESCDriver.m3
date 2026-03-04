(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

MODULE ESCDriver;

IMPORT M3AST_AS, RefList, Atom, Wr;

PROCEDURE ProcessUnit(<* UNUSED *> cu: M3AST_AS.Compilation_Unit;
                      <* UNUSED *> unitName: Atom.T;
                      <* UNUSED *> pragmas: RefList.T;
                      <* UNUSED *> wr: Wr.T) =
  BEGIN
    (* Phase 2+: parse specs, generate VCs, run prover, report results *)
  END ProcessUnit;

BEGIN
END ESCDriver.
