MODULE M3CSrcPos;

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

CONST
  PackingFactor = 1024;
  MaxOffset = PackingFactor - 1;
  


<*INLINE*> PROCEDURE Pack(line, offset: CARDINAL): T RAISES {}=
  BEGIN
    RETURN line * PackingFactor + MIN(MaxOffset, offset);
  END Pack;


<*INLINE*> PROCEDURE Unpack(t: T; VAR offset: CARDINAL): CARDINAL RAISES {}=
  BEGIN
    offset := t MOD PackingFactor;
    RETURN t DIV PackingFactor;
  END Unpack;


PROCEDURE Compare(pos1, pos2: T): INTEGER RAISES {}=
  BEGIN
    RETURN pos1 - pos2;
  END Compare;


BEGIN

END M3CSrcPos.
