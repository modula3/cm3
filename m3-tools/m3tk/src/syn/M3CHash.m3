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


MODULE M3CHash;

IMPORT Text, Word;

REVEAL
  Table = TablePublic BRANDED OBJECT
    size: CARDINAL;
    idCreator: IdCreator;
    ids: REF ARRAY OF Id;
  OVERRIDES
    init := InitTable;
    enter := Enter;
    lookup := Lookup;
    setCreator := SetCreator;
    enterCharsWithValue := EnterCharsWithValue;
    enterTextWithValue := EnterTextWithValue;
  END;

  Id = IdPublic BRANDED OBJECT
    next: Id;
    text: Text.T;
  OVERRIDES
    toText := ToText;
  END;

  Value = ValuePublic BRANDED OBJECT
    sum, sumOfSums := 0;
  OVERRIDES
    init := InitValue;
    reset := ResetValue;
    addCharToValue := AddCharToValue;
  END;


<*INLINE*> PROCEDURE InitValue(v: Value): Value=
  BEGIN
    RETURN v;
  END InitValue;


<*INLINE*> PROCEDURE ResetValue(v: Value)=
  BEGIN
    v.sum := 0; v.sumOfSums := 0;
  END ResetValue;


<*INLINE*> PROCEDURE AddCharToValue(v: Value; ch: CHAR)=
  BEGIN
    v.sum := Word.Plus(v.sum, ORD(ch));
    v.sumOfSums := Word.Plus(v.sumOfSums, v.sum);
  END AddCharToValue;


<*INLINE*> PROCEDURE Create(
    t: Table;
    text: Text.T;
    VAR list: Id)
    : Id=
  BEGIN
    WITH new = t.idCreator.new(text) DO
      new.next := list;
      new.text := text;
      list := new;
      RETURN new;
    END;
  END Create;


<*INLINE*> PROCEDURE Equal(
    t: Text.T;
    READONLY chars: ARRAY OF CHAR)
    : BOOLEAN
   =
  BEGIN
    FOR i := 0 TO LAST(chars) DO
      IF Text.GetChar(t, i) # chars[i] THEN RETURN FALSE END;
    END;
    RETURN TRUE;
  END Equal;


<*INLINE*> PROCEDURE FindChars(
    READONLY chars: ARRAY OF CHAR;
    id: Id)
    : Id=
  BEGIN
    WHILE id # NIL DO
      IF Text.Length(id.text) = NUMBER(chars) AND Equal(id.text, chars) THEN
        EXIT;
      ELSE
        id := id.next;
      END;
    END;
    RETURN id;
  END FindChars;


PROCEDURE EnterCharsWithValue(
    t: Table;
    v: Value;
    READONLY chars: ARRAY OF CHAR)
    : Id
   =
  BEGIN
    WITH id = t.ids[v.sumOfSums MOD t.size], found = FindChars(chars, id) DO
      IF found # NIL THEN
        RETURN found;
      ELSE
        RETURN Create(t, Text.FromChars(chars), id);
      END;
    END;
  END EnterCharsWithValue;


<*INLINE*> PROCEDURE FindText(text: Text.T; id: Id): Id=
  BEGIN
    WHILE id # NIL DO
      IF Text.Equal(id.text, text) THEN
        EXIT;
      ELSE
        id := id.next;
      END;
    END;
    RETURN id;
  END FindText;


PROCEDURE EnterTextWithValue(t: Table; v: Value; text: Text.T): Id=
  BEGIN
    WITH id = t.ids[v.sumOfSums MOD t.size], found = FindText(text, id) DO
      IF found # NIL THEN
        RETURN found;
      ELSE
        RETURN Create(t, text, id);
      END;
    END;
  END EnterTextWithValue;


<*INLINE*> PROCEDURE TextValue(text: Text.T): Value=
  VAR
    v := NEW(Value).init();
  BEGIN
    FOR i := 0 TO Text.Length(text) - 1 DO
      AddCharToValue(v, Text.GetChar(text, i));
    END; (* for *)
    RETURN v;
  END TextValue;


<*INLINE*> PROCEDURE Enter(t: Table; text: Text.T): Id=
  BEGIN
    RETURN EnterTextWithValue(t, TextValue(text), text);
  END Enter;


<*INLINE*> PROCEDURE Lookup(t: Table; text: Text.T; VAR id: Id): BOOLEAN=
  VAR
    tempId := FindText(text, t.ids[TextValue(text).sumOfSums MOD t.size]);
  BEGIN
    IF tempId # NIL THEN id := tempId; RETURN TRUE ELSE RETURN FALSE END;
  END Lookup;


TYPE
  DefaultIdCreator = IdCreator OBJECT OVERRIDES new := DefaultNewId END;


<*INLINE*> PROCEDURE DefaultNewId(<*UNUSED*> c: IdCreator;
                                  <*UNUSED*> t: Text.T): Id=
  BEGIN
    RETURN NEW(Id);
  END DefaultNewId;


<*INLINE*> PROCEDURE NewDefaultIdCreator(): DefaultIdCreator=
  BEGIN
    RETURN NEW(DefaultIdCreator);
  END NewDefaultIdCreator;


VAR
  gDefaultIdCreator := NewDefaultIdCreator();


PROCEDURE InitTable(
    init: Table;
    size: CARDINAL;
    idCreator: IdCreator := NIL): Table=
  BEGIN
    IF idCreator = NIL THEN idCreator := gDefaultIdCreator END;
    init.size := size;
    init.idCreator := idCreator;
    init.ids := NEW(REF ARRAY OF Id, size);
    RETURN init;
  END InitTable;


<*INLINE*> PROCEDURE SetCreator(t: Table; idCreator: IdCreator): IdCreator=
  VAR
    old := t.idCreator;
  BEGIN
    t.idCreator := idCreator;
    RETURN old;
  END SetCreator;


<*INLINE*> PROCEDURE ToText(id: Id): TEXT=
  BEGIN
    RETURN id.text;
  END ToText;


BEGIN

END M3CHash.
