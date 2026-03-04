(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

(* ESC/Modula-3 driver: orchestrates VCG and proving for one unit. *)

INTERFACE ESCDriver;

IMPORT M3AST_AS, RefList, Atom, Wr;

PROCEDURE ProcessUnit(cu: M3AST_AS.Compilation_Unit;
                      unitName: Atom.T;
                      pragmas: RefList.T;   (* of ESCSpec.RawPragma *)
                      wr: Wr.T);
(* Process a single compilation unit: parse specs, generate VCs,
   run Simplify, report results.  Currently a stub that does nothing. *)

END ESCDriver.
