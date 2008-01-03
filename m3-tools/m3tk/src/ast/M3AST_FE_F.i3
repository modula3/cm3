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


INTERFACE M3AST_FE_F;

(*--------------------------------------------------------------------------
                             Naming Conventions

   Classes:    Upper case
   Nodes:      Lower case, first letter capitalised
   Attributes: Lower case, prefixed with xx_, with xx_ one of

   fe_         Front-end attribute

--------------------------------------------------------------------------*)

IMPORT M3AST_AS, M3AST_FE;
IMPORT M3ASTOp_SM AS Previous_View;

TYPE
  Compilation_Unit = Previous_View.Compilation_Unit OBJECT
    fe_uid: M3AST_FE.Unit_uid := NIL;
    fe_status := M3AST_FE.Unit_status{};
  END;

REVEAL
  M3AST_AS.Compilation_Unit <: Compilation_Unit;


END M3AST_FE_F.
