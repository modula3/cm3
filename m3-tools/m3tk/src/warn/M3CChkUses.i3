INTERFACE M3CChkUses;

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
  Handle <: ASTWalk.Closure;

PROCEDURE NewHandle(cu: M3AST_AS.Compilation_Unit;
  ignoreFORvars := FALSE): Handle RAISES {};
(* Create a new handle with 'Node' as the callback procedure. 
If 'ignoreFORvars := FALSE', FOR loop control variables will not
be considered unused, even if they are! Since a 'Handle' is
a subtype of 'ASTWalk.Closure' it can be passed directly to
the tree walker. Alternatively, 'Node' can be called explicitly
from some other tree walk. *)

PROCEDURE Node(h: Handle; n: AST.NODE; vm: ASTWalk.VisitMode) RAISES {};
(* This procedure is to be called from the tree walker (in entry and exit mode)
so that it is called on every node in an AST. It checks for unused declarations
and imports. It observes the UNUSED pragma. An interface containing a REVEAL 
statement is considered to be used irrespective of whether the revelation is
necessary. *)

PROCEDURE CloseHandle(h: Handle) RAISES {};
(* generate error messages for unused variables associated with the
analysis using 'h'. *)

END M3CChkUses.
