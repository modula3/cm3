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

INTERFACE ASTWalk;

(* "ASTWalk" provides a way of systematically visiting every node in an AST. *)

IMPORT AST;
  
EXCEPTION Aborted;

TYPE
  VisitMode = {Entry, Exit};
  VisitModeControl = SET OF VisitMode;
  
  Closure <: Closure_public;
  Closure_public = OBJECT  
  METHODS
    callback(n: AST.NODE; vm := VisitMode.Entry) RAISES ANY;
    init(): Closure;
  END;

  NodeCallbackProc = PROCEDURE(n: AST.NODE) RAISES ANY;

CONST
  OnEntry = VisitModeControl{VisitMode.Entry};
  OnExit = VisitModeControl{VisitMode.Exit};
  OnEntryAndExit = VisitModeControl{VisitMode.Entry,
                                    VisitMode.Exit};

PROCEDURE VisitNodes(n: AST.NODE; vc: Closure) RAISES ANY;
(* Walk tree "n", applying "vc.callback(n, VisitMode.Entry)" *)


PROCEDURE ModeVisitNodes(n: AST.NODE; vc: Closure; 
    vm: VisitModeControl) RAISES ANY;
(* Similar to "VisitNodes", but apply the callback on entry, exit or both. *)

PROCEDURE IgnoreChildren(vc: Closure);
(* Suppress the visit of the children of the current node. *)

PROCEDURE Abort() RAISES {Aborted};
(* Abort current walk by raising the "Aborted" exception. *)

PROCEDURE NodeProcClosure(p: NodeCallbackProc): Closure;

END ASTWalk.

(* The "ASTWalk" interface provides a way to visit every node in an
AST, applying a user-supplied method at each node.  This process is
often referred to a {\em tree walk} or an {\em AST walk}.

The children of a node of some type "T" are visited in the order
defined by the implementation of the "walk" method for "T". Typically
this is defined to be the {\em natural} order, that is, corresponding
to reading the program text.

The caller can choose whether the callback method is applied on entry
to the node, on exit from the node, or both.  A walk may be aborted at
any time.  All the state associated with a walk is captured in the
"Closure" argument. So walks may be nested or performed in parallel,
using different closure arguments. The caller should subtype the
"Closure" type in order to support argument passing or the return of
results from the callback method. The closure should be created
by a call of the form "NEW(Closure, callback := YourCallback).init()".

The callback may raise any exception, including "Aborted". The latter
is caught by "VisitNodes" and "ModeVisitNodes", and aborts the
walk, returning control to the caller. Any other exception
also aborts the walk and is propagated to the caller.

The "VisitNodes" procedure supports the most common type of walk,
which is to vis to apply the callback method on {\em entry} to the
node, that is before the children are visited (pre-order).

"ModeVisitNodes" takes a "VisitModeControl" argument that allows
the callback method to be applied on entry, exit or both. The
"OnExit" value can be used to perform a post-order walk, that is,
applying the method after all the children have been visited.

It is sometimes convenient to suppress the visit of the children of a
given node, and this can be achieved by calling "IgnoreChildren" from
the callback method. Once control leaves the node from which the call
was made, the suppression is disabled. A typical use is to mix
pre-order and post-order walks by, say, starting a pre-order walk,
calling "IgnoreChildren" at some node and from the callback begin a
post-order walk of the children.  

The "NodeProcClosure" provides a simple way to create a closure
value for a callback method that neither requires any state nor
cares about the "VisitMode" argument. *)
