(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

(* Parse the ESC specification sub-language from pragma text.

   Grammar reconstructed from surviving annotated M3 library files
   (Sequence.ig, SequenceRep.ig, Fmt.i3, Thread.i3, Simplex.m3).

   Spec pragma forms:
     <*SPEC procname(formals) REQUIRES ... MODIFIES ... ENSURES ...*>
     <*SPEC T.method(formals) REQUIRES ... MODIFIES ... ENSURES ...*>
     <*SPEC VAR name: MAP T TO U *>
     <*SPEC VAR name: SEQ[T] *>
     <*SPEC ABSTRACT M.V[x: T]: body *>
     <*SPEC DEPEND M.V[x: T]: field1, field2, ... *>
     <*SPEC FUNC name(formals): body *>
     <*SPEC AXIOM body *>
     <*SPEC INVARIANT body *>
     <*SPEC REP body *>
     <*LOOPINV body *>
*)

INTERFACE ESCSpecParse;

IMPORT ESCSpec, RefList;

EXCEPTION ParseError(TEXT);

PROCEDURE Parse(raw: ESCSpec.RawPragma): ESCSpec.Spec RAISES {ParseError};
(* Parse a single raw pragma into a typed spec object. *)

PROCEDURE ParseAll(raws: RefList.T): RefList.T RAISES {ParseError};
(* Parse a list of RawPragma into a list of Spec.
   Skips PragmaSpec entries. *)

END ESCSpecParse.
