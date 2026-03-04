(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

MODULE ESCTypes;

IMPORT Atom, RefList, M3Context;
IMPORT AtomRefTbl;
IMPORT ESCNameMap;

REVEAL
  Env = BRANDED REF RECORD
    types: AtomRefTbl.T;      (* Atom.T -> TypeInfo *)
    allTypes: RefList.T;      (* list of TypeInfo *)
    allTypeCodes: RefList.T;  (* list of Atom.T *)
  END;

PROCEDURE Build(<* UNUSED *> c: M3Context.T;
                <* UNUSED *> unitName: Atom.T): Env =
  VAR env := NEW(Env);
  BEGIN
    env.types := NEW(AtomRefTbl.Default).init();
    env.allTypes := NIL;
    env.allTypeCodes := NIL;

    (* Add built-in types *)
    AddBuiltin(env, "REFANY", TRUE, FALSE, FALSE, NIL);
    AddBuiltin(env, "ROOT", TRUE, FALSE, FALSE,
               RefList.List1(Atom.FromText("REFANY")));
    AddBuiltin(env, "MUTEX", TRUE, FALSE, FALSE,
               RefList.List1(Atom.FromText("ROOT")));
    AddBuiltin(env, "NULL", TRUE, FALSE, FALSE,
               RefList.List3(Atom.FromText("REFANY"),
                             Atom.FromText("TEXT"),
                             Atom.FromText("MUTEX")));
    AddBuiltin(env, "TEXT", TRUE, FALSE, FALSE,
               RefList.List1(Atom.FromText("REFANY")));
    AddBuiltin(env, "BOOLEAN", FALSE, FALSE, TRUE, NIL);
    AddBuiltin(env, "CHAR", FALSE, FALSE, TRUE, NIL);
    AddBuiltin(env, "INTEGER", FALSE, FALSE, TRUE, NIL);
    AddBuiltin(env, "CARDINAL", FALSE, FALSE, TRUE,
               RefList.List1(Atom.FromText("INTEGER")));

    (* Phase 1+: Walk m3tk context to extract declared types *)

    RETURN env;
  END Build;

PROCEDURE AddBuiltin(env: Env; name: TEXT;
                     isRef, isObject, isOrdinal: BOOLEAN;
                     supers: RefList.T) =
  VAR
    a := Atom.FromText(name);
    info := NEW(TypeInfo,
                name := a,
                typecode := ESCNameMap.TypeCode(a),
                isPred := ESCNameMap.TypePred(a),
                supers := supers,
                fields := NIL,
                methods := NIL,
                isObject := isObject,
                isRef := isRef,
                isOrdinal := isOrdinal);
  BEGIN
    EVAL env.types.put(a, info);
    env.allTypes := RefList.Cons(info, env.allTypes);
    env.allTypeCodes := RefList.Cons(info.typecode, env.allTypeCodes);
  END AddBuiltin;

PROCEDURE LookupType(env: Env; name: Atom.T): TypeInfo =
  VAR ref: REFANY;
  BEGIN
    IF env.types.get(name, ref) THEN
      RETURN NARROW(ref, TypeInfo);
    END;
    RETURN NIL;
  END LookupType;

PROCEDURE AllTypes(env: Env): RefList.T =
  BEGIN RETURN env.allTypes END AllTypes;

PROCEDURE AllTypeCodes(env: Env): RefList.T =
  BEGIN RETURN env.allTypeCodes END AllTypeCodes;

BEGIN
END ESCTypes.
