INTERFACE M3CId;

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

IMPORT Text;
IMPORT M3AST_LX, M3CHash, M3CReservedWord;

TYPE
  Definitions <: REFANY;
  T = M3AST_LX.Symbol_rep;

REVEAL
  T = M3CHash.Id BRANDED OBJECT
    defs: Definitions := NIL;
    atom: INTEGER; (* a unique value that identifies this id *)
  END;

<*INLINE*> PROCEDURE Table(): M3CReservedWord.Table RAISES {};
<*INLINE*> PROCEDURE ToText(id: T): Text.T RAISES {};
<*INLINE*> PROCEDURE Enter(text: Text.T): T RAISES {};
<*INLINE*> PROCEDURE Lookup(text: Text.T; VAR id: T): BOOLEAN RAISES {};

END M3CId.
