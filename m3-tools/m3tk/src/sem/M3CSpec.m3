MODULE M3CSpec;

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


IMPORT M3AST, M3AST_AS;

IMPORT M3AST_AS_F, M3AST_SM_F;



PROCEDURE Set(an: M3AST.NODE) RAISES {} =
  BEGIN
    TYPECASE an OF
    | M3AST_AS.Compilation_Unit(cu) =>
        cu.as_root.as_id.sm_spec := cu.as_root;
        cu.as_root.sm_comp_unit := cu;
    | M3AST_AS.Proc_decl(proc_decl) =>
      	proc_decl.as_id.sm_spec := proc_decl;
    | M3AST_AS.METHOD_OVERRIDE(method) =>
        method.as_id.sm_spec := method;
    ELSE
    END; (* case *)
  END Set;


BEGIN
END M3CSpec.
