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

INTERFACE AST_WalkRep;

IMPORT AST;
IMPORT AST_Iter AS Previous_View;

(* This is not intended as a client interface, but as support for the
implementation of various tree walk models. The "ASTWalk" interface
provides one such model. All AST nodes, other than leaf nodes, must
provide an implementation of the "walk" method.  *)

TYPE NODE = Previous_View.NODE OBJECT
  METHODS
    walk(handle: Handle) RAISES ANY := Null;
  END;

REVEAL AST.NODE <: NODE;

TYPE
  Handle <: Handle_public;
  Handle_public = OBJECT
    METHODS
      Visit(n: AST.NODE) RAISES ANY;
  END;

PROCEDURE Null(n: NODE; handle: Handle) RAISES {};
(* returns immediately; used as default method *)

END AST_WalkRep.

(* The "walk" method visits the children of "self", in some order
determined by the concrete method. The connection to the "callback"
interface provided by "ASTWalk" is through "handle", which provides
the "Visit" method. Each implementation of the "walk" method should
apply "Visit" to its children. The "Visit" method encapsulates the
callback and the application of "walk" to the child nodes. 

Since the connection to "ASTWalk" is decoupled by the "handle" type,
it is possible to devise alternate implementations of the "Visit"
method without altering this interface or the implementations of
the "walk" methods. *)
