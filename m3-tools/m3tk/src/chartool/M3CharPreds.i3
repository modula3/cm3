(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)

INTERFACE M3CharPreds;

IMPORT M3AST_AS, M3Error;

PROCEDURE Es (e: M3AST_AS.EXP): BOOLEAN;
  (* constant expression that introduces dependency CHAR:
     *SIZE(TC(Ts))
     FIRST|LAST|NUMBER([a] Tn) *)

PROCEDURE Ecs (e: M3AST_AS.EXP): BOOLEAN;
  (* constant expression that depends on CHAR:
     Es
     VAL(Ecs, ...)
     binary or unary value combination (Ecs, ...)
     Word operation on Ecs, ...
     value cons ...{...Ecs, ...}
     Ecs.f
     Ecs[...]
     ...[Ecs] *)

PROCEDURE Tn (ts: M3AST_AS.TYPE_SPEC): BOOLEAN;
  (* type whose FIRST, LAST, or NUMBER depends on CHAR:
     CHAR
     ARRAY Tn OF ...
     [...Ecs...] *)

PROCEDURE Th (ts: M3AST_AS.TYPE_SPEC): BOOLEAN;
  (* type that will become impractically large ("Huge"):
     ARRAY Tn OF ...
     SET OF Tn *)

PROCEDURE Tr (ts: M3AST_AS.TYPE_SPEC): BOOLEAN;
  (* type that will be replaced:
     Th
     BITS Ecs FOR ...
     BITS ...  FOR TC(Ts) *)

PROCEDURE Ts (ts: M3AST_AS.TYPE_SPEC): BOOLEAN;
  (* type whose size introduces dependency on CHAR:
     Tn
     Tr *)

TYPE
  PredTypeProc = PROCEDURE (ts: M3AST_AS.TYPE_SPEC): BOOLEAN;
    (* procedure type for passing as argument to "TC" *)

PROCEDURE TC (ts: M3AST_AS.TYPE_SPEC; tp: PredTypeProc): BOOLEAN;
  (* maps kind Tx to kind of types that contain a Tx:
     Tx
     RECORD ... f: TC(Tx) ... END
     ARRAY ... OF TC(Tx)
     BITS ...  FOR TC(Tx) *)

PROCEDURE TCO (ts: M3AST_AS.TYPE_SPEC; tp: PredTypeProc): BOOLEAN;
  (* maps kind Tx to kind of object types that contain a Tx:
     ...  OBJECT ...  f: TC(Tx) ... END
     (* a TC(Tx) anywhere in type hierarchy *) *)

PROCEDURE Tm (ts: M3AST_AS.TYPE_SPEC): BOOLEAN;
  (* type that mentions CHAR in any way *)

TYPE
  Char_Grade =
    {No (* not in Tm *),
     Td (* Tm - TC(Ts) *),
     TcTs (* TC(Ts) *)};

PROCEDURE Grade (ts: M3AST_AS.TYPE_SPEC): Char_Grade;
  (* Determine whether this type is in TC(Ts), Td, or neither *)

PROCEDURE ArrayTnOf (ts: M3AST_AS.TYPE_SPEC): BOOLEAN;
  (* TRUE iff "ts" denotes an "ARRAY Tn OF ..." *)

(* A few random conveniences... *)

PROCEDURE M3TYPE_To_TYPE_SPEC (m: M3AST_AS.M3TYPE):
  M3AST_AS.TYPE_SPEC;
  (* M3CTypesMisc.GetTYPE_SPECFromM3TYPE recast as a function *)

PROCEDURE ReportInUnit(unit   : M3AST_AS.UNIT_ID;
                       n      : M3Error.ERROR_NODE;
                       message: TEXT) RAISES {};
  (* Like M3Error.Report, but for a (potentially) different
     compilation unit. *)

END M3CharPreds.
