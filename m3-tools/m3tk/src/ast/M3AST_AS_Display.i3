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
(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3AST_AS_Display;

(* This interface contains the definitions of the default display
procedures for the syntactic nodes. All subtypes of the
"M3AST_LX.SRC_NODE_C" class have a single display procedure that
simple calls "display" for all the childern in the "lx_node_s"
sequence. All other subtypes of "M3AST_LX.SRC_NODE", e.g.
"M3AST_LX.ID", have specific display procedures. *)

IMPORT M3AST_AS_F;
IMPORT AST_DisplayRep;

PROCEDURE SRC_NODE_C(n: M3AST_AS_F.SRC_NODE_C; h: AST_DisplayRep.Handle
    ) RAISES ANY;

PROCEDURE ID(
    n: M3AST_AS_F.ID; h: AST_DisplayRep.Handle
    ) RAISES ANY;

PROCEDURE LITERAL(
    n: M3AST_AS_F.LITERAL; h: AST_DisplayRep.Handle
    ) RAISES ANY;

PROCEDURE Whitespace(
    n: M3AST_AS_F.Whitespace; h: AST_DisplayRep.Handle
    ) RAISES ANY; 
        
PROCEDURE Comment(
    n: M3AST_AS_F.Comment; h: AST_DisplayRep.Handle
    ) RAISES ANY;

PROCEDURE Pragma(
    n: M3AST_AS_F.Pragma; h: AST_DisplayRep.Handle
    ) RAISES ANY;

PROCEDURE BadChar(
    n: M3AST_AS_F.BadChar; h: AST_DisplayRep.Handle
    ) RAISES ANY;

PROCEDURE Token(
    n: M3AST_AS_F.Token; h: AST_DisplayRep.Handle
    ) RAISES ANY;

END M3AST_AS_Display.
