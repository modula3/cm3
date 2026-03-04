(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

(* Map counterexample labels to source locations and format warnings. *)

INTERFACE ESCReport;

IMPORT Atom, Wr, RefList;

PROCEDURE Report(labels: RefList.T;   (* of Atom.T *)
                 unitName: Atom.T;
                 wr: Wr.T);
(* Parse label names (e.g., |ERROR.deref.81.8|), map to source
   locations, and write formatted warnings. *)

END ESCReport.
