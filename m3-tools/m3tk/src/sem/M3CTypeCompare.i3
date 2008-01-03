INTERFACE M3CTypeCompare;

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

IMPORT M3AST_AS, M3AST_SM;


PROCEDURE Identical(t1, t2: M3AST_SM.TYPE_SPEC_UNSET): BOOLEAN RAISES {};
(* are the given types identical? Also succeeds if either type is unset *)

PROCEDURE Similar(t1, t2: M3AST_AS.TYPE_SPEC): BOOLEAN RAISES {};
(* are the given types similar? Two types are similar if they have the
same form (e.g. both records with same number of fields, with the same names
and defaults) and their component types have the same type code. 'Similar' is
used by the type equivalence/minimisation algorithm *) 

END M3CTypeCompare.
