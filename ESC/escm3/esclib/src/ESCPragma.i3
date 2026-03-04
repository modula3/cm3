(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

(* Extract ESC specification pragmas from m3tk compilation units.

   Walks the pragma store of a Compilation_Unit and collects all
   <*SPEC ...*> and <*LOOPINV ...*> pragmas, returning them as
   a list of ESCSpec.RawPragma objects. *)

INTERFACE ESCPragma;

IMPORT M3AST_AS, RefList, Atom;

PROCEDURE Extract(cu: M3AST_AS.Compilation_Unit;
                  unitName: Atom.T): RefList.T;
(* Returns a RefList.T of ESCSpec.RawPragma, one per SPEC or LOOPINV
   pragma found in cu.  unitName is attached to each RawPragma for
   error reporting.  Also sets the hook on each processed pragma so
   m3tk won't warn about unrecognized pragmas. *)

PROCEDURE HasSpecs(cu: M3AST_AS.Compilation_Unit): BOOLEAN;
(* Quick check: does this compilation unit contain any SPEC or
   LOOPINV pragmas?  Does not consume them. *)

END ESCPragma.
