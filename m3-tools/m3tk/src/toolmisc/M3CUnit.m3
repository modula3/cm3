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


MODULE M3CUnit EXPORTS M3CUnit, M3AST_FE, M3CUnitRep;

IMPORT Rd, Fmt, Err, OSError, Atom, FS, Pathname, FileRd;
IMPORT M3Extension, M3FindFile;
IMPORT M3Conventions; 
IMPORT M3AST_AS;
IMPORT M3AST_AS_F, M3AST_SM_F;

TYPE ExtSet = M3Extension.TSet; Ext = M3Extension.T;

PROCEDURE FindUnit(
    f: M3FindFile.T;
    name: TEXT;
    unitType: Type;
    VAR (*inout*) uf: Form;
    VAR id: Uid)
    : Rd.T
    RAISES {OSError.E} =
  VAR
    m3Exts: ExtSet;
    result: Rd.T;
    t: M3Extension.T;
    pn: Pathname.T;
  BEGIN
    IF unitType = Type.Interface THEN 
      IF uf # Form.Source THEN m3Exts := ExtSet{Ext.PInt}
      ELSE m3Exts := M3Extension.Ints;
      END;
    ELSE m3Exts := M3Extension.Mods 
    END;
    IF M3Extension.Has(name, t) AND t # M3Extension.T.Null THEN
      result := FileRd.Open(name);
      pn := name;
    ELSE
      (* strip to module name *)
      name := Pathname.Base(name);
      result := OpenFromSet(f, name, m3Exts, pn);
    END;
    IF (result = NIL) AND (uf # Form.Source) THEN
      IF unitType = Type.Interface THEN
        m3Exts := M3Extension.Ints;
	result := OpenFromSet(f, name, m3Exts, pn);
      END;
    END;

    IF result # NIL THEN
      id := NEW(Uid, filename := pn);
      IF NOT (Ext.PInt IN m3Exts) THEN 
        TRY
          WITH status = FS.Status(pn) DO
    	    id.stamp := status.modificationTime;
          END;
	EXCEPT 
        | OSError.E(t) =>
            Err.Print(Fmt.F("problem reading timestamp for %s - %s", 
                id.filename, Atom.ToText(t.head)), Err.Severity.Error);
            result := NIL;
        END;
        uf := Form.Source;
      ELSE
        uf := Form.Ast;
      END;
    END; (* if *)
    RETURN result;
  END FindUnit;

PROCEDURE OpenFromSet(
    f: M3FindFile.T;
    name: TEXT;
    exts: M3Extension.TSet;
    VAR (*out*) pn: Pathname.T)
    : Rd.T
    RAISES {OSError.E}=
  VAR rd: Rd.T := NIL;
  BEGIN
    FOR ext := FIRST(M3Extension.T) TO LAST(M3Extension.T) DO
      IF ext IN exts THEN
        TRY 
           pn := f.find(name, ext);
           rd := f.openRead(name, ext);
           EXIT;
        EXCEPT
        | M3FindFile.Failed =>
        END;
      END; (* if *)
    END; (* for *)
    RETURN rd;
  END OpenFromSet;


PROCEDURE FindStandard(
    f: M3FindFile.T;
    VAR uf: Form;
    VAR id: Uid
    ): Rd.T RAISES {OSError.E} =
  BEGIN
    RETURN FindUnit(f, M3Conventions.Standard, Type.Interface, uf, id);
  END FindStandard;


PROCEDURE TextName(id: Uid): TEXT RAISES {} =
  BEGIN
    RETURN id^.filename;
  END TextName;

PROCEDURE TypeName(ut: Type): TEXT RAISES {} =
  BEGIN
    CASE ut OF
    | Type.Interface => RETURN "interface";
    | Type.Module => RETURN "module";
    | Type.Interface_gen_def => RETURN "generic interface"
    | Type.Interface_gen_ins => RETURN "instantiated interface"
    | Type.Module_gen_def => RETURN "generic module"
    | Type.Module_gen_ins => RETURN "instantiated module"
    (*ELSE crash *) 
    END;
  END TypeName;

EXCEPTION NullUNIT;

PROCEDURE ToType(u: M3AST_AS.UNIT): Type RAISES {}=
  <*FATAL NullUNIT*>
  BEGIN
    TYPECASE u OF <*NOWARN*>
    | NULL => RAISE NullUNIT;
    | M3AST_AS.Interface => RETURN Type.Interface;
    | M3AST_AS.Module => RETURN Type.Module;
    | M3AST_AS.Interface_gen_def => RETURN Type.Interface_gen_def;
    | M3AST_AS.Module_gen_def => RETURN Type.Module_gen_def;
    | M3AST_AS.Interface_gen_ins => RETURN Type.Interface_gen_ins;
    | M3AST_AS.Module_gen_ins => RETURN Type.Module_gen_ins;
    END;
  END ToType;

PROCEDURE ToGenIns(cu: M3AST_AS.Compilation_Unit;
    VAR (*inout*) ut: Type): M3AST_AS.Compilation_Unit RAISES {}=
  BEGIN
    TYPECASE cu.as_root OF
    | M3AST_AS.UNIT_GEN_INS(unit_ins) =>
        cu := unit_ins.sm_ins_comp_unit;
        IF cu # NIL THEN
          IF ISTYPE(cu.as_root, M3AST_AS.Interface) THEN ut := Type.Interface
          ELSE ut := Type.Module
          END;
        END;
    ELSE
    END; (* typecase *)
    RETURN cu
  END ToGenIns;

PROCEDURE Equal(id1, id2: Uid): BOOLEAN=  
  BEGIN
    RETURN id1.stamp = id2.stamp;
  END Equal;

<*INLINE*> PROCEDURE InclState(VAR status: Status; state: State) RAISES {}=
  BEGIN
    status := status + Status{state};
  END InclState;

<*INLINE*> PROCEDURE ExclState(VAR status: Status; state: State) RAISES {}=
  BEGIN
    status := status - Status{state};
  END ExclState;

BEGIN

END M3CUnit.
