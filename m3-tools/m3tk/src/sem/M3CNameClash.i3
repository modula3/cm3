INTERFACE M3CNameClash;

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

IMPORT M3AST_AS;


PROCEDURE Enumeration(e: M3AST_AS.Enumeration_type) RAISES {};
PROCEDURE Record(r: M3AST_AS.Record_type) RAISES {};
PROCEDURE Object(o: M3AST_AS.Object_type) RAISES {};
PROCEDURE Procedure(p: M3AST_AS.Procedure_type) RAISES {};
(* each of the above checks the specified type for duplicate names (e.g.
duplicate member names for an enumeration, duplicate field names for a record).
  This module reuses the 'Definitions' lists associated with identifier hash
ids. These lists are first used by 'M3CScope'. By the time any of the above
procedures is called it is assumed that:
a) all entries due to local scopes, imports etc have been removed and hence..
b) the only entries in the 'Definitions' lists are those for the standard
   identifiers
If these assumptions are found to be incorrect a fatal error will be raised *)

END M3CNameClash.
