MODULE M3CReservedWord;

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
IMPORT M3CHash, M3CToken;


REVEAL
  Id = M3CHash.Id BRANDED OBJECT token := M3CToken.Identifier END;
  Table = M3CHash.Table BRANDED OBJECT END;


TYPE
  IdCreator = M3CHash.IdCreator OBJECT
    r: M3CToken.ReservedWord;
  OVERRIDES
    new := NewId;
  END;


PROCEDURE NewId(c: IdCreator; <*UNUSED*> t: Text.T): M3CHash.Id RAISES {}=
  BEGIN
    RETURN NEW(Id, token := ORD(c.r));
  END NewId;


PROCEDURE Token(id: Id): M3CToken.T RAISES {}=
  BEGIN
    RETURN id.token;
  END Token;


PROCEDURE New(
    size: CARDINAL;
    idCreator: M3CHash.IdCreator := NIL)
    : Table
    RAISES {}=
  VAR
    c := NEW(IdCreator);
    v := NEW(M3CHash.Value).init();
    table := NEW(Table).init(size, c);
  BEGIN
    FOR r := FIRST(M3CToken.Texts) TO LAST(M3CToken.Texts) DO
      c.r := r;
      WITH word = M3CToken.Texts[r] DO
        v.reset();
        FOR i := 0 TO Text.Length(word) - 1 DO
          v.addCharToValue(Text.GetChar(word, i));
        END;
        EVAL table.enterTextWithValue(v, word);
      END;
    END;
    EVAL table.setCreator(idCreator);
    RETURN table;
  END New;


BEGIN
END M3CReservedWord.
