(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

MODULE ESCReport;

IMPORT Atom, Wr, RefList;

PROCEDURE Report(<* UNUSED *> labels: RefList.T;
                 <* UNUSED *> unitName: Atom.T;
                 <* UNUSED *> wr: Wr.T) =
  BEGIN
    (* Phase 2+: parse label names, map to source locations, format warnings *)
  END Report;

BEGIN
END ESCReport.
