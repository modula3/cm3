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

INTERFACE M3AST_PL;

IMPORT M3AST_AS;

(* This view defines extra attributes to support the pre-linker. *)

(* It is important to know (quickly) which modules export a given
interface.  We also record those procedures that were unimplemented,
for the backend to allow a stub to be generated. *)

   <* FIELDS OF M3AST_AS.Interface_id
        pl_isexportedby_s: SeqM3AST_AS_Module_id.T;
        pl_missing_proc_s: SeqM3AST_AS_Proc_id.T; *>

(* The {\em depends-on} set is computed for each module. *)

   <* FIELDS OF M3AST_AS.Module
        pl_dependson_s: SeqM3AST_AS_Module.T; *>


(* The set of subtypes of each object type is computed.  The set of
"Method_id/Proc_id" pairs (method table) is computed for each object
type. *)

   <* FIELDS OF M3AST_AS.Object_type
        pl_subtype_s: SeqM3AST_AS_Object_type.T;
        pl_method_table: MethodTable; *>

TYPE
  MethodTable = REF ARRAY OF RECORD
    method_id: M3AST_AS.Method_id;
    proc_id: M3AST_AS.Proc_id;
  END;

END M3AST_PL.
