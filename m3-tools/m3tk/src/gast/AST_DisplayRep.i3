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

INTERFACE AST_DisplayRep;

IMPORT AST, AST_CopyRep;

(* Clients must not apply the "display" method directly, but use some
   wrapper procedure provided by a given AST implementation.
   New AST nodes should provide an implementation of the "display" method.
*)

TYPE
  NODE = AST_CopyRep.NODE OBJECT
    METHODS
      display(handle: Handle) RAISES ANY := Null;
      (* An (opaque) handle object is passed to each node display method,
      for use in communication with the display kernel and state holding. *)
  END;

REVEAL AST.NODE <: NODE;

TYPE
  Handle <: ROOT;

PROCEDURE Null(n: NODE; handle: Handle) RAISES {};
(* returns immediately; used as default method *)

END AST_DisplayRep.
