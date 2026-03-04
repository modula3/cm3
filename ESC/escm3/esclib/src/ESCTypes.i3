(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

(* Build a type environment from m3tk semantic information.

   Walks the m3tk AST to extract:
   - Type declarations with field lists, method signatures
   - Subtype relationships
   - TYPECODE constants
   - Type predicate names (Is$T)

   Uses sstubgen's AstToType/Type modules for the heavy lifting. *)

INTERFACE ESCTypes;

IMPORT Atom, RefList, M3Context;

TYPE
  Env <: REFANY;

  TypeInfo = OBJECT
    name: Atom.T;             (* fully qualified name *)
    typecode: Atom.T;         (* |Name.TYPECODE| *)
    isPred: Atom.T;           (* |Is$Name| *)
    supers: RefList.T;        (* list of Atom.T: direct supertypes *)
    fields: RefList.T;        (* list of FieldInfo *)
    methods: RefList.T;       (* list of MethodInfo *)
    isObject: BOOLEAN;
    isRef: BOOLEAN;
    isOrdinal: BOOLEAN;
  END;

  FieldInfo = OBJECT
    name: Atom.T;             (* field name *)
    typeName: Atom.T;         (* field type name *)
    selectName: Atom.T;       (* |Module.Type.field| for select/store *)
  END;

  MethodInfo = OBJECT
    name: Atom.T;             (* method name *)
    formals: RefList.T;       (* list of Atom.T: formal names *)
    resultType: Atom.T;       (* result type, or NIL for PROCEDURE *)
  END;

PROCEDURE Build(c: M3Context.T; unitName: Atom.T): Env;
(* Build a type environment from the compilation context. *)

PROCEDURE LookupType(env: Env; name: Atom.T): TypeInfo;
(* Returns NIL if not found. *)

PROCEDURE AllTypes(env: Env): RefList.T;
(* Returns list of all TypeInfo objects. *)

PROCEDURE AllTypeCodes(env: Env): RefList.T;
(* Returns list of all TYPECODE atoms for DISTINCT axiom. *)

END ESCTypes.
