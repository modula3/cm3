INTERFACE M3CTypeCheck;

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

IMPORT AST, M3AST_AS, ASTWalk;


TYPE
  Handle <: REFANY;


PROCEDURE Node(
    handle: Handle;
    any: AST.NODE;
    v: ASTWalk.VisitMode)
    RAISES {};
(* Type check the given node. This routine is intended for use by the tree
walker. It assumes that all type specs and expression values (if constant)
have been set. 'handle' contains information needed in order to do type
checking - see 'NewHandle' below *)

PROCEDURE NewHandle(safe: BOOLEAN; in: M3AST_AS.Proc_decl): Handle RAISES {};
(* Before the tree walk starts some information is required:
'safe'     is the tree walk occuring in a safe module?
'in'       is the tree walk starting inside a procedure body? If so 'in' should
           be the declaration for the procedure, otherwise it should be NIL.
           This knowledge is required for RETURN expression checking.
*)

END M3CTypeCheck.
