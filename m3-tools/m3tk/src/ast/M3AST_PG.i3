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

INTERFACE M3AST_PG;

(* Standard pragmas, as defined in the report, i.e. "INLINE" and
"EXTERNAL".  There was a time when these were part of the language
syntax, so they are integrated more closely with the AST than other
pragmas, and it is expected that a Modula-3 parser will process these
pragmas and build the AST nodes that are specified in this interface.
*)

IMPORT M3AST, M3AST_LX;

(* We use the general approach to multiple inheritance here and
   introduce the class "EXTERNAL_DECL" to carry the information
   as to whether an "Interface" or "DECL" node is "EXTERNAL". 
   So there is an attribute "vEXTERNAL_DECL" on "Interface",
   "Interface_gen_def" and "DECL" nodes. *)

(* Generic interfaces can be EXTERNAL *)

  <* FIELDS OF M3AST_AS.Interface_gen_def
     vEXTERNAL_DECL: EXTERNAL_DECL; *>

(* Interfaces can be EXTERNAL *)

  <* FIELDS OF M3AST_AS.Interface
     vEXTERNAL_DECL: EXTERNAL_DECL; *>

(* The abstract syntax permits any "DECL" to be EXTERNAL, although the
static semantics are more restrictive. *)

  <* FIELDS OF M3AST_AS.DECL
     vEXTERNAL_DECL: EXTERNAL_DECL; *>

  <* FIELDS OF M3AST_AS.Proc_decl
     pg_inline: Inline_NULL; *>

TYPE
  Inline <: M3AST_LX.SRC_NODE_C;
  Inline_NULL = Inline;

(* <*EXTERNAL L*> (DECL | INTERFACE) *)

  EXTERNAL_DECL <: M3AST.NODE;
  <* FIELDS OF EXTERNAL_DECL
     pg_external: External_NULL *>

  External <: M3AST_LX.SRC_NODE_C;
  <* FIELDS OF External
     lx_lang_spec: M3AST_LX.Text_rep *>

  External_NULL = External;
  External_NULL_UNSET = External_NULL;

(* Since the semantic analyser chooses to use "DEF_ID" nodes to carry
most of the semantic information, it is convenient to propagate the
"external" information to some "DEF_ID" nodes.  Again we use multiple
inheritance, and in "M3AST_SM" the associated nodes will contain a
"vEXTERNAL_ID" attribute.  The value of the "pg_external" field is a
back pointer to the "External" node of the enclosing "EXTERNAL"
declaration.  The value of "pg_external" will be NIL if the
"EXTERNAL_ID" is not "EXTERNAL". Note that for all "EXTERNAL_ID" nodes
in an interface that is "EXTERNAL", the value of "pg_external" will not be
"NIL". *)

  EXTERNAL_ID <: M3AST.NODE;    (* Ids that can be EXTERNAL *)
  <* FIELDS OF EXTERNAL_ID
     pg_external: External_NULL_UNSET *>

PROCEDURE IsA_EXTERNAL_DECL(n: M3AST.NODE;
    VAR(*out*) external_decl: EXTERNAL_DECL): BOOLEAN;
(* ISTYPE(n, Interface) OR ISTYPE(n, DECL) *)

PROCEDURE IsA_EXTERNAL_ID(n: M3AST.NODE;
    VAR(*out*) external_id: EXTERNAL_ID): BOOLEAN;
(* Interface_id OR Proc_id OR Var_id OR Const_id OR Type_id OR Exc_id *)

(* This proc returns distinguished values that indicates UNSET. *)

PROCEDURE UNSET_External(): External_NULL_UNSET;

END M3AST_PG.
