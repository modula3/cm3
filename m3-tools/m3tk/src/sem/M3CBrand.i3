INTERFACE M3CBrand;

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

(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

IMPORT AST, M3AST_AS;
IMPORT ASTWalk;


(* Provides a way to set the 'sm_brand' attribute. The 'Set' procedure should
be called by the tree walker. 'NewHandle' should be called before the tree
walk starts to create a handle for use with 'Set' *)

TYPE
  Handle <: REFANY;

PROCEDURE NewHandle(
    unit: M3AST_AS.UNIT_NORMAL;
    block: M3AST_AS.Block := NIL;
    typeDecl: M3AST_AS.Concrete_decl := NIL;
    revelation: M3AST_AS.Concrete_reveal := NIL)
    : Handle
    RAISES {};
(* 'unitId' is the id of the unit in which the tree walk is taking place.
  If the tree walk is starting inside a block other than the top level block
of an interface or module 'block' should be set appropriately.
  If the tree walk is starting inside a concrete type declaration or revelation
'typeDecl' or 'revelation' should be set appropriately *)

PROCEDURE Set(h: Handle; n: AST.NODE; vm: ASTWalk.VisitMode) RAISES {};
(* Sets up the 'sm_brand' field of a 'Brand'. Relies on the 'sm_def' attribute
being set up for ids on the left hand side of revelations *)

END M3CBrand.
