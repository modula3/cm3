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

MODULE M3LMain;

IMPORT RefList;
IMPORT M3AST_AS, M3CUnit, M3Context, M3CId;
IMPORT M3AST_AS_F, M3AST_SM_F;
IMPORT SeqM3AST_AS_Used_interface_id;

TYPE
  FindExporterClosure = M3Context.Closure OBJECT
    unitId: M3CId.T;
    seqCu: RefList.T := NIL;
  OVERRIDES
    callback := FindExporter;
  END;


PROCEDURE Module(c: M3Context.T): RefList.T=
  VAR
    cl: FindExporterClosure;
  BEGIN
    cl := NEW(FindExporterClosure, unitId := M3CId.Enter(Name));
    (* try to find modules that exports an interface called 'name'. *)
    <*FATAL ANY*> BEGIN
      M3Context.Apply(c, cl);
    END;
    RETURN cl.seqCu;
  END Module;

(*PRIVATE*)
PROCEDURE FindExporter(
    cl: FindExporterClosure;
    ut: M3CUnit.Type;
    <*UNUSED*> name: TEXT; 
    cu: M3AST_AS.Compilation_Unit) RAISES {}=
  VAR
    iter: SeqM3AST_AS_Used_interface_id.Iter;
    used_intf_id: M3AST_AS.Used_interface_id;
  BEGIN
    cu := M3CUnit.ToGenIns(cu, ut);
    IF ut = M3CUnit.Type.Module THEN
      iter := SeqM3AST_AS_Used_interface_id.NewIter(
                NARROW(cu.as_root, M3AST_AS.Module).sm_export_s);
      WHILE SeqM3AST_AS_Used_interface_id.Next(iter, used_intf_id) DO
        IF used_intf_id.lx_symrep = cl.unitId THEN
          cl.seqCu := RefList.AppendD(cl.seqCu, RefList.List1(cu));
        END;
      END; (* while *)
    END; (* if *)
  END FindExporter;

BEGIN
END M3LMain.
