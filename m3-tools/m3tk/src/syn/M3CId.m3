MODULE M3CId;

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
IMPORT M3CHash, M3CReservedWord;

(* This implementation uses a global hash table *)

VAR
  table_g := M3CReservedWord.New(10240, NewIdCreator());
  atom_g := 0;

PROCEDURE NewId(<*UNUSED*> c: M3CHash.IdCreator;
                <*UNUSED*> text: Text.T): M3CHash.Id RAISES {}=
  BEGIN
    INC(atom_g);
    RETURN NEW(T, atom := atom_g);
  END NewId;


PROCEDURE NewIdCreator(): M3CHash.IdCreator RAISES {}=
  TYPE
    IdCreator = M3CHash.IdCreator OBJECT OVERRIDES new := NewId END;
  BEGIN
    RETURN NEW(IdCreator);
  END NewIdCreator;


<*INLINE*> PROCEDURE Table(): M3CReservedWord.Table RAISES {}=
  BEGIN
    RETURN table_g;
  END Table;


<*INLINE*> PROCEDURE ToText(id: T): Text.T RAISES {}=
  BEGIN
    RETURN id.toText();
  END ToText;


<*INLINE*> PROCEDURE Enter(text: Text.T): T RAISES {}=
  BEGIN
    RETURN table_g.enter(text);
  END Enter;


<*INLINE*> PROCEDURE Lookup(text: Text.T; VAR id: T): BOOLEAN RAISES {}=
  VAR
    hashId: M3CHash.Id;
  BEGIN
    IF table_g.lookup(text, hashId) THEN
      TYPECASE hashId OF
      | T(ident) => id := ident; RETURN TRUE;
      ELSE
        RETURN FALSE;
      END;
    ELSE
      RETURN FALSE;
    END;
  END Lookup;


BEGIN

END M3CId.
