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

INTERFACE AST_CopyRep;

(* This is not intended as a client interface, but as support for the
implementation of a tree copy interface. "ASTCopy" is one such
interface.  All AST nodes must provide an implementation of the "copy"
method.  *)

IMPORT AST, AST_WalkRep;

TYPE
  NODE = AST_WalkRep.NODE OBJECT
    METHODS
      copy(handle: Handle): AST.NODE RAISES ANY := Null;
    END;

REVEAL AST.NODE <: NODE;

TYPE
  Handle <: Handle_public;
  Handle_public = OBJECT
    METHODS
      Copy(n: AST.NODE): AST.NODE RAISES ANY;
  END;

PROCEDURE Null(n: NODE; handle: Handle): AST.NODE RAISES {};
(* returns "NIL"; used as the default method *)

END AST_CopyRep.

(* The "copy" method returns a copy of some part of the tree rooted at
the node making the call. At this level of abstraction "copy" is very
loosely defined. Precisely which attributes are copied, and whether
attributes are {\em shallow-copied} or {\em deep-copied} is left to
specific AST interfaces to define.

To aid in the creation of flexible copying interfaces, the "copy"
method takes a "Handle" argument, in a similar style to the "walk"
method defined in "AST_WalkRep". Each implementation of the "copy"
method should create a new instance of itself and then apply
"handle.Copy" to each of its children in turn, children, assigning the
results to the corresponding child attributes of the new instance. *)
