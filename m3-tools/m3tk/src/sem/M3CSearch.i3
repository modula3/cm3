INTERFACE M3CSearch;

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

(* This module provides specialised forms of name resolution not handled by
'M3CScope'. 'M3CScope' handles the problems of lexical scope for the main
block structure of the program. 'M3CSearch' provides, for example, searching
to see if a given name is a field of a given record type.
  The procedures in this module take two arguments. The first argument provides
the context for the search. The second argument is a 'Used_id', which is the
item to be looked up in the context. If the search succeeds the 'Used_id' is
updated so that it points to its defining occurence. If the search fails the
'Used_id' is left unchanged and an error message may be given.
  An error message need not always be given if the search fails because the
implementation may keep track of previous failed searches in the same context.
In this case the implementation could avoid giving repeated error messages for
identical failed searches. Note that the implementation does not guarantee to
do this but it does mean that the user of the interface should not rely on
getting an error message for every failed lookup *)

IMPORT M3AST_AS;



PROCEDURE Export(
    int: M3AST_AS.Interface;
    id: M3AST_AS.USED_ID)
    RAISES {};
(* Searches the declarations of 'int' looking for 'id'. Assumes that 'int' has
already had all its internal names resolved *)

PROCEDURE Member(
    enum: M3AST_AS.Enumeration_type;
    id: M3AST_AS.USED_ID)
    RAISES {};
(* Searches the members of 'enum' looking for 'id' *)

PROCEDURE Field(
    record: M3AST_AS.Record_type;
    id: M3AST_AS.USED_ID)
    RAISES {};
(* Searches the fields of 'record' looking for 'id' *)

PROCEDURE FieldOrMethod(
    object: M3AST_AS.Object_type;
    methodsOnly: BOOLEAN;
    id: M3AST_AS.USED_ID)
    RAISES {};
(* Searches the methods and optionally the fields of 'object' looking for
'id' *)

PROCEDURE Formal(
    proc: M3AST_AS.Procedure_type;
    id: M3AST_AS.USED_ID)
    RAISES {};
(* Searches the formal parameters of 'proc' looking for 'id' *)

END M3CSearch.
