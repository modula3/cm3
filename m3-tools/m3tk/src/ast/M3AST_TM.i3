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
(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* "M3AST_TM" defines some {\em temporary} AST attributes to support
the semantic analyser. *)

INTERFACE M3AST_TM;

(* This interface specifies some additional attributes that are
notionally "temporary", most of which are to support the semantic
analyser.

   As with "M3AST_AS" and "M3AST_SM", this interface only specifies an
abstract representation of the attributes, leaving the exact
representation to a companion interfaace, e.g. "M3AST_TM_F".*)

<* PRAGMA FIELDS *>

(*-----------------------Private attributes-------------------------------*)

(* Clients should make no assumptions about the value of any of the following
   attributes, which are private to the semantic analyser and other tools.
   Typically, a procedural interface will be provided elsewhere to
   some of the values. *)

(* A Modula-3 linker must assign a type code to all reference types.
   It turns out be convenient to also have such an attribute for
   semantic analysis. *)

   <* FIELDS OF M3AST_AS.TYPE_SPEC
        tmp_type_code: INTEGER; *>

(* To support the detection of recursive definitions, it is useful to
   flag a defining identifier as recursive (or not). *)

   <* FIELDS OF M3AST_AS.DEF_ID
        tmp_recursive: BOOLEAN; *>

(* To support setting of type attributes, instances of "Type_id" which have
   a "Named_type" as their "M3TYPE" have an attribute to allow the chain to be 
   followed.  A similar attribute is needed for "Exc_id". *)

   <* FIELDS OF M3AST_AS.Type_id
        tmp_type: M3AST_AS.M3TYPE_NULL *>

   <* FIELDS OF M3AST_AS.Exc_id
        tmp_type: M3AST_AS.M3TYPE_NULL *>

(* To set the type of an method override, and to check coverage of
   defaults, we need to get from a method or override to the "Object_type". *)

   <* FIELDS OF M3AST_AS.METHOD_OVERRIDE
        tmp_type: M3AST_AS.Object_type; *>

(* The attributes defined on an "M3AST_SM.Opaque_type_Revln in "M3AST_SM"
   only capture revelations local to an AST. The followwing attributes
   capture those revelation inherited from the imported interfaces. *)


   <* FIELDS OF M3AST_SM.Opaque_type_Revln
        tmp_count := 0;
        tmp_concrete_rev: M3AST_SM.TYPE_SPEC_UNSET; 
        tmp_opaque_rev_s := SeqM3AST_AS_TYPE_SPEC.Null; *>

(* In any scope, there is the concept of the "most revealing" revelation,
   which is given by the following attribute. *)

   <* FIELDS OF M3AST_AS.Opaque_type
        tmp_rev_type_spec: M3AST_SM.TYPE_SPEC_UNSET; *>

(* We build a hash table of DEF_IDs in interfaces for fast lookup. *)

   <* FIELDS OF M3AST_AS.Interface
        tmp_def_id_table: IntRefTable.T; *>

(* IMPORT I AS J; the "J" has a back pointer to "I", so that we
   can go through the level of indirection. *)

   <* FIELDS OF M3AST_AS.Interface_AS_id
        tmp_used_id: M3AST_AS.Used_interface_id; *>

(* to support efficient handling of RAISES clauses, we have an attribute
   that says whether a check is needed. We also mark whether a procedure
   needs a check for a missing return. *)

   <* FIELDS OF M3AST_AS.Proc_decl 
        tmp_needs_raises: BOOLEAN;
        tmp_needs_return_check: BOOLEAN; *> 
 
   <* FIELDS OF M3AST_AS.Raise_st
        tmp_fatal: BOOLEAN; *>

END M3AST_TM.
