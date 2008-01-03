INTERFACE M3CSM;

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

PROCEDURE Check(cu: M3AST_AS.Compilation_Unit) RAISES {};
(* Handles the semantic analysis, except the computation of the
'sm_export_s' attribute which must have been handled already.
In order to allow "extension" of the compiler, the current
revelations for opaque types in 'cu' are left in place, i.e
M3CTypesMisc.Reveal(opaque_type) will return the revelation
that is appropriate for the scope defined by 'cu'. After
any extensions have been called, a client must call
'FinishUp' to reset the state.
*)

PROCEDURE FinishUp(cu: M3AST_AS.Compilation_Unit) RAISES {};
(* Finish up the semantic analysis, resetting the opaque
type revelations. *)

END M3CSM.

