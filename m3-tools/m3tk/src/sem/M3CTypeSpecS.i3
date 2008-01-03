INTERFACE M3CTypeSpecS;

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

IMPORT M3AST, M3AST_AS;


PROCEDURE Set(an: M3AST.NODE; unit: M3AST_AS.UNIT_WITH_BODY) RAISES {};
(* If 'an' is a reference type adds it to the 'sm_type_spec_s' 
list of 'unit'. Does not depend on any other semantic attributes *)

PROCEDURE TCTag(ts: M3AST_AS.TYPE_SPEC): INTEGER RAISES {};
(* If 'ts' is on the 'sm_type_spec' list of owning unit, return its typecode
   tag, else a checked runtime error.
*)

PROCEDURE TCUnit_id(ts: M3AST_AS.TYPE_SPEC): M3AST_AS.UNIT_ID RAISES {};
(* Returns UNIT_ID of owning unit. *)

END M3CTypeSpecS.
