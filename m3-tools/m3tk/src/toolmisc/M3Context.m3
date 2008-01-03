(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)
(**)
(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3Context;

IMPORT Property, TextRefTbl, Text;

IMPORT M3AST_AS, M3CId, M3CUnit, M3Conventions;
IMPORT M3AST_AS_F;

REVEAL T = Property.Set BRANDED OBJECT t: TextRefTbl.T; END;

TYPE
  UnitHandle = REF RECORD
    interface: M3AST_AS.Compilation_Unit := NIL;
    module: M3AST_AS.Compilation_Unit := NIL;
  END;

VAR
  standard_g: M3AST_AS.Compilation_Unit;
  standardId_g: M3CId.T;

PROCEDURE New(): T RAISES {} =
  BEGIN
    RETURN NEW(T, t := NEW(TextRefTbl.Default).init(256));
  END New;

PROCEDURE Add(
    t: T;
    name: TEXT;
    unitType: M3CUnit.Type;
    cu: M3AST_AS.Compilation_Unit) RAISES {Duplicate} =
  VAR
    ra: REFANY;
    bp: UnitHandle;
  BEGIN
    IF unitType = M3CUnit.Type.Interface AND
      Text.Equal(name, M3Conventions.Standard) THEN
      IF standard_g = NIL THEN SetStandard(cu) END;
      RETURN
    END;

    IF t.t.get(name, ra) THEN
      bp := NARROW(ra, UnitHandle);
    ELSE
      bp := NEW(UnitHandle);
      EVAL t.t.put(name, bp);
    END;
    IF unitType IN M3CUnit.Interfaces THEN
      IF (bp.interface # NIL) AND (bp.interface # cu) THEN RAISE Duplicate END;
      bp.interface := cu;
    ELSE
      IF (bp.module # NIL) AND (bp.module # cu) THEN RAISE Duplicate END;
      bp.module := cu;
    END; (* if *)
  END Add;

PROCEDURE Remove(t: T; name: TEXT; unitType: M3CUnit.Type) RAISES {} =
  VAR
    ra: REFANY;
    bp: UnitHandle;
  BEGIN
    IF t.t.get(name, ra) THEN
      bp := NARROW(ra, UnitHandle);
      IF unitType IN M3CUnit.Interfaces THEN
        bp.interface := NIL;
      ELSE
        bp.module := NIL;
      END;
    END;
  END Remove;

PROCEDURE FindPossiblyExact(
    t: T; 
    name: TEXT;
    unitType: M3CUnit.Type;
    VAR cu: M3AST_AS.Compilation_Unit;
    exact := FALSE)
    : BOOLEAN
    RAISES {} =
  VAR
    ra: REFANY;
    bp: UnitHandle;
  BEGIN
    IF standard_g # NIL AND unitType = M3CUnit.Type.Interface AND
       Text.Equal(name, M3Conventions.Standard) THEN
      cu := standard_g;
      RETURN TRUE;
    ELSIF t.t.get(name, ra) THEN
      bp := NARROW(ra, UnitHandle);
      IF unitType IN M3CUnit.Interfaces THEN
        cu := bp.interface;
      ELSE
        cu := bp.module;
      END;
      IF exact THEN
        RETURN cu.as_root # NIL AND M3CUnit.ToType(cu.as_root) = unitType;
      ELSE
        RETURN cu # NIL;
      END; (* if *)
    ELSE
      cu := NIL;
      RETURN FALSE;
    END; (* if *)
  END FindPossiblyExact;

PROCEDURE Find(
    t: T; 
    name: TEXT;
    unitType: M3CUnit.Type;
    VAR cu: M3AST_AS.Compilation_Unit)
    : BOOLEAN
    RAISES {} =
  BEGIN
    RETURN FindPossiblyExact(t, name, unitType, cu);
  END Find;

PROCEDURE FindExact(
    t: T; 
    name: TEXT;
    unitType: M3CUnit.Type;
    VAR cu: M3AST_AS.Compilation_Unit)
    : BOOLEAN
    RAISES {} =
  BEGIN
    RETURN FindPossiblyExact(t, name, unitType, cu, TRUE);
  END FindExact;


PROCEDURE FindFromId(
    t: T;
    id: M3CId.T;
    unitType: M3CUnit.Type;
    VAR (*out*) cu: M3AST_AS.Compilation_Unit)
    : BOOLEAN RAISES {} =
  BEGIN
    IF standard_g # NIL AND unitType = M3CUnit.Type.Interface AND 
       standardId_g = id THEN
      cu := standard_g;
      RETURN cu # NIL;
    ELSE
      RETURN FindPossiblyExact(t, M3CId.ToText(id), unitType, cu)
    END;
  END FindFromId;

REVEAL
  Iter = BRANDED OBJECT 
    needStandard := FALSE;
    unitType: M3CUnit.Type;
    tblIter: TextRefTbl.Iterator;
  END;

PROCEDURE NewIter(
    t: T; 
    unitType: M3CUnit.Type;
    findStandard := TRUE
    ): Iter RAISES {} =
  BEGIN
    RETURN NEW(Iter, unitType := unitType, 
            needStandard := unitType = M3CUnit.Type.Interface AND findStandard,
            tblIter := t.t.iterate());
  END NewIter;

PROCEDURE Next(
    iter: Iter;
    VAR name: TEXT;
    VAR cu: M3AST_AS.Compilation_Unit)
    : BOOLEAN
    RAISES {} =
  VAR
    ra: REFANY;
    uh: UnitHandle;
  BEGIN
    IF iter.needStandard AND standard_g # NIL THEN 
       iter.needStandard := FALSE;
       cu := standard_g;
       name := M3Conventions.Standard;
       RETURN TRUE;
    ELSE
      WHILE iter.tblIter.next(name, ra) DO
        uh := NARROW(ra, UnitHandle);
        IF iter.unitType IN M3CUnit.Interfaces THEN cu := uh.interface 
        ELSE cu := uh.module 
        END;
        IF cu # NIL AND cu.as_root # NIL AND
           M3CUnit.ToType(cu.as_root) = iter.unitType THEN
          RETURN TRUE 
        END;
      END;
    END;
    RETURN FALSE;
  END Next;

REVEAL 
  Closure = Closure_public BRANDED OBJECT OVERRIDES init := Init END;

PROCEDURE ApplyToSet(t: T; cl: Closure; unitTypeSet := M3CUnit.AllTypes; 
    findStandard := FALSE) RAISES ANY =
  VAR
    iter: Iter;
    name: TEXT;
    cu: M3AST_AS.Compilation_Unit;
  BEGIN
    cl.context := t;
    TRY
      FOR unitType := FIRST(M3CUnit.Type) TO LAST(M3CUnit.Type) DO
        IF unitType IN unitTypeSet THEN
          iter := NewIter(t, unitType, findStandard);
          WHILE Next(iter, name, cu) DO
            cl.callback(unitType, name, cu);
          END; (* while *)
        END;
      END;
    EXCEPT
    | Aborted =>
    END;
  END ApplyToSet;

PROCEDURE Apply(t: T; cl: Closure; findStandard := TRUE) RAISES ANY=
  BEGIN
    ApplyToSet(t, cl, findStandard := findStandard);
  END Apply;

PROCEDURE AbortApply() RAISES {Aborted}=
  BEGIN
    RAISE Aborted;
  END AbortApply;

PROCEDURE Init(c: Closure): Closure RAISES {}=
  BEGIN
    RETURN c;
  END Init;

PROCEDURE SetStandard(cu: M3AST_AS.Compilation_Unit) RAISES {} =
  BEGIN
    standard_g := cu; 
    standardId_g := M3CId.Enter(M3Conventions.Standard);
  END SetStandard;


PROCEDURE Standard(): M3AST_AS.Compilation_Unit RAISES {} =
  BEGIN
    RETURN standard_g;
  END Standard;


BEGIN
  standard_g := NIL;
END M3Context.
