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

INTERFACE M3CReservedWord;

IMPORT M3CToken, M3CHash;

TYPE
  Table <: M3CHash.Table;
  Id <: M3CHash.Id;

PROCEDURE Token(id: Id): M3CToken.T RAISES {};

PROCEDURE New(
    size: CARDINAL;
    idCreator: M3CHash.IdCreator := NIL)
    : Table
    RAISES {};
(* Returns a hash table which is like a normal 'M3CHash.Table' but already
contains entries of type 'Id' for all the reserved words. Thus looking up
a reserved word in this table will return an object of type 'Id'; 'Token' can
then be used to return the reserved word token.
  If 'Id' is used as a supertype e.g.
    MyId = M3CReservedWord.Id OBJECT ... END;
then when an object of type 'MyId' is allocated it will be set up specially so
that 'Token(myId)' will return 'M3CToken.T.Identifier' *)

END M3CReservedWord.
