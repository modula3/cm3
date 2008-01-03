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

<* PRAGMA FIELDS *>

INTERFACE M3AST_FE;

(* FE stands for "Front-End" as in "Compiler Front-End". The AST is
   augmented with attributes that pertain to the compilation process. *)

<* FIELDS OF M3AST_AS.Compilation_Unit
     fe_uid: Unit_uid := NIL;
     fe_status: Unit_status; *>

TYPE
  Unit_uid <: REFANY;     (* a unique identifier for a unit *)

  Unit_status = SET OF Unit_state;
  Unit_state = {  (* flags indicating compilation state *)
     Parsed,                     (* => tree present *)
     ImportsResolved,            (* IMPORT, EXPORT *)
     SemChecked,                 (* static semantics *)
     PErrors, IErrors, SErrors,  (* parse, import, semantic errors *)
     User1, User2, User3         (* hard-wired extensibility! *)
  }; 

CONST
  Unit_Errors = 
    Unit_status{Unit_state.PErrors, Unit_state.IErrors, Unit_state.SErrors};
  Unit_AllPhases =
    Unit_status{Unit_state.Parsed, Unit_state.ImportsResolved,
                Unit_state.SemChecked};

TYPE
  Unit_type = {Interface, Module, Interface_gen_def, Module_gen_def,
               Interface_gen_ins, Module_gen_ins};

END M3AST_FE.
