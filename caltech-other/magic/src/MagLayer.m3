(* $Id$ *)

MODULE MagLayer;
IMPORT Word;

VAR id := 0;

(* this is really just a place-holder type... *)

REVEAL
  T = Public BRANDED Brand OBJECT id : Word.T; OVERRIDES init := Init END;

PROCEDURE Equal(a, b : T) : BOOLEAN = 
  BEGIN 
    RETURN a.id = b.id 
  END Equal;

PROCEDURE Hash(a : T) : Word.T = BEGIN RETURN a.id END Hash;

PROCEDURE Init(a : T) : T = BEGIN a.id := id; INC(id); RETURN a END Init;

BEGIN END MagLayer.
