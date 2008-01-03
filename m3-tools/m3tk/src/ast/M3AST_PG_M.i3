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


INTERFACE M3AST_PG_M;

IMPORT AST_DisplayRep;
IMPORT M3AST_PG, M3AST_PG_F;
IMPORT M3AST_PG_Copy;

TYPE
  Inline = M3AST_PG_F.Inline OBJECT
  OVERRIDES
    copy := M3AST_PG_Copy.Inline;
  END;
  External = M3AST_PG_F.External OBJECT
  OVERRIDES
    copy := M3AST_PG_Copy.External;
  END;

REVEAL
  M3AST_PG.Inline <: Inline;
  M3AST_PG.External <: External;

END M3AST_PG_M.
