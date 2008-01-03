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

(* "ASTCopy" provides a way to copy all or part of an AST. *)

INTERFACE ASTCopy;

IMPORT AST, ASTWalk;

TYPE
  Closure <: Closure_public;
  Closure_public = OBJECT
  METHODS
    callback(n, ncopy: AST.NODE;
             vm: ASTWalk.VisitMode) RAISES ANY;
    init(): Closure;
  END;

PROCEDURE Nodes(
    n: AST.NODE;
    ): AST.NODE RAISES {};
(* Copies the tree rooted at "n" and returns the root of the copy. *)

PROCEDURE ModeNodes(
    n: AST.NODE;
    c: Closure;
    vm : ASTWalk.VisitModeControl;
    ): AST.NODE RAISES ANY;
(* Similar to "Nodes" but supports a per-node callback. *)

PROCEDURE IgnoreChildren(c: Closure) RAISES {};
(* Suppress the copy of the children of the current node. *)

END ASTCopy.

(* The "ASTCopy" interface allows an AST to be copied, with provision
for the caller to be called back at each node, using a similar
mechanism to the "ASTWalk" interface. The callback can be used to copy
or share additional attributes that are not copied by the underlying
"copy" method, for example attributes that cause the AST to form a
graph rather than a tree. 

The "Nodes" procedure provides for a simple copy of the tree rooted at
its argument, "n", without any callbacks.

The "ModeNodes" takes a closure argument that should be created with a
call of "NEW(Closure, callback := YourCallback).init()". The closure can be
subtyped to provide for shared state to be accessed during the
callbacks. The "vm" argument can be used to control when the callback
are made in the way described in the "ASTWalk" interface. On an
{\em entry} callback to a node, the value of the "ncopy" will be "NIL".
On an {\em exit} callback, "ncopy" will refer to the newly created copy.

The "IgnoreChildren" procedure can be called to suppress the copying
of a node's children. Once control leaves the node from which the call
was made, the suppression is disabled. It is only effective when
called in {\em entry} mode. *)
