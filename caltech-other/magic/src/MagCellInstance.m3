(* $Id$ *)

MODULE MagCellInstance;
IMPORT MagCell, MagTransform, Word, Text;

REVEAL 
  T = Public BRANDED Brand OBJECT
    definition : MagCell.T; 
    id : TEXT; 
    transform : MagTransform.T;
    parent : T ; 
  OVERRIDES
    init := Init;
    getDef := GetDef;
    getTransform := GetTransform;
  END;

PROCEDURE GetDef(self : T) : MagCell.T = 
  BEGIN RETURN self.definition END GetDef;

PROCEDURE GetTransform(self : T) : MagTransform.T = 
  BEGIN RETURN self.transform END GetTransform;

PROCEDURE Init(self : T;
               definition : MagCell.T; id : TEXT; 
               transform : MagTransform.T; parent : T) : T =
  BEGIN 
    self.definition := definition; 
    self.id := id; 
    self.transform := transform; 
    self.parent := parent;
    RETURN self
  END Init;

PROCEDURE Hash(a : T) : Word.T = BEGIN RETURN Text.Hash(a.id) END Hash;
PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN a = b END Equal;

BEGIN END MagCellInstance.
