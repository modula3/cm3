INTERFACE M3CChkNarrow;

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

IMPORT AST, M3AST_AS;
IMPORT ASTWalk;


TYPE
  Handle <: ASTWalk.Closure;

PROCEDURE NewHandle(cu: M3AST_AS.Compilation_Unit): Handle RAISES {};
(* Create a new handle for use with the 'Node' procedure. Since
it is a subtype of 'ASTWalk.Closure', it can be passed directly to
the tree walker.  *)

PROCEDURE Node(h: Handle; n: AST.NODE; vm: ASTWalk.VisitMode) RAISES {};
(* This procedure is intended to be called from the tree walker (in
entry and exit mode) so that it is called on every node in a section of AST. 
It checks for implicit NARROW calls. It is pointless to call this procedure 
on nodes which are in an interface. *)

END M3CChkNarrow.
