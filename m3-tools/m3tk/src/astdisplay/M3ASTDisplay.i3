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

INTERFACE M3ASTDisplay;

IMPORT Wr, M3AST, ASTWalk;

(* Specialized walk of the tree which pretty prints. *)

TYPE
  Closure <: ASTWalk.Closure; (* may be subtyped by client to hold state. *)

PROCEDURE Nodes(
    n: M3AST.NODE;
    s: Wr.T) 
    RAISES {Wr.Failure};
(* The default walk never calls back. *)

PROCEDURE ModeNodes(
    n: M3AST.NODE;
    c: Closure;
    vm : ASTWalk.VisitModeControl;
    s: Wr.T)
    RAISES {Wr.Failure};
(* This one supplies a callback *)

PROCEDURE IgnoreChildren(c: Closure) RAISES {};
(* Can be called from an entry visit to suppress visiting the children
   of this node.  Once control moves to another node, the suppression
   is disabled. *)

END M3ASTDisplay.
