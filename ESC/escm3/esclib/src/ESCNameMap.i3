(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

(* Map M3 identifiers to Simplify pipe-delimited naming convention.

   Naming patterns observed in addhi.sx:
     |Module.Type.field|        e.g. |IntSeqRep.Public.elem|
     |Is$Type|                  e.g. |Is$IntSeq.T|
     |FUNC.Module.Var|          e.g. |FUNC.IntSeq.Valid|
     |RESIDUE.Module.Var|       e.g. |RESIDUE.IntSeq.Valid|
     |Type.TYPECODE|            e.g. |IntSeqRep.Public.TYPECODE|
     |Module.Brand|             e.g. |Integer.Brand|
     |$NIL|                     null constant
     |@true|, |@false|          boolean constants
     |INTEGER.FIRST|, etc.      integer bounds *)

INTERFACE ESCNameMap;

IMPORT Atom;

PROCEDURE FieldName(module, type, field: Atom.T): Atom.T;
(* -> |module.type.field| *)

PROCEDURE TypePred(typeName: Atom.T): Atom.T;
(* -> |Is$typeName| *)

PROCEDURE FuncName(module, var: Atom.T): Atom.T;
(* -> |FUNC.module.var| *)

PROCEDURE ResidueName(module, var: Atom.T): Atom.T;
(* -> |RESIDUE.module.var| *)

PROCEDURE TypeCode(typeName: Atom.T): Atom.T;
(* -> |typeName.TYPECODE| *)

PROCEDURE QualName(module, name: Atom.T): Atom.T;
(* -> |module.name| *)

PROCEDURE PipeName(name: TEXT): Atom.T;
(* -> |name| (wrap in pipes) *)

(* Well-known constants *)
VAR (*CONST*)
  Nil, AtTrue, AtFalse: Atom.T;
  IntFirst, IntLast: Atom.T;
  Return, Exit: Atom.T;

END ESCNameMap.
