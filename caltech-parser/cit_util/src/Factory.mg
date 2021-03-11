(* $Id$ *)

GENERIC MODULE Factory(Of);
IMPORT Word, Text;
IMPORT ObjectFactory AS Super;
IMPORT ObjectFactoryClass;

REVEAL
  T = Public BRANDED Brand OBJECT OVERRIDES
    build := Build;
    buildT := BuildT;
    hash := Hash;
    init := Init;
    equal := Equal;
  END;

PROCEDURE Hash(<*UNUSED*>a : T) : Word.T = 
  BEGIN RETURN Text.Hash(Brand) END Hash;

PROCEDURE Equal(a : T;  b : Super.T) : BOOLEAN = 
  BEGIN RETURN a.code = b.code END Equal;

PROCEDURE Build(<*UNUSED*>a : T) : REFANY = BEGIN RETURN NEW(Of.T) END Build;

PROCEDURE BuildT(a : T) : Of.T = BEGIN RETURN a.build() END BuildT;

PROCEDURE Init(self : T) : Super.Public = 
  BEGIN self.code := TYPECODE(Of.T); RETURN self END Init;

BEGIN END Factory.


 
