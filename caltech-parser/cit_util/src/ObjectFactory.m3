(* $Id$ *)
MODULE ObjectFactory;
IMPORT ObjectFactoryClass;
IMPORT Word;
IMPORT RTAllocator;

REVEAL
  T = ObjectFactoryClass.Private BRANDED Brand OBJECT END;

  Default = PublicDefault BRANDED "Default " & Brand OBJECT
  OVERRIDES
    hash := HashDefault;
    equal := EqualDefault;
    build := BuildDefault;
    init := InitDefault
  END;

PROCEDURE Hash(a : T) : Word.T = BEGIN RETURN a.hash() END Hash;
PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN a.equal(b) END Equal;

PROCEDURE InitDefault(def : Default; code : Word.T) : T =
  BEGIN def.code := code; RETURN def END InitDefault;

PROCEDURE HashDefault(def : Default) : Word.T = 
  BEGIN RETURN def.code END HashDefault;

PROCEDURE EqualDefault(def : Default; a : T) : BOOLEAN =
  BEGIN RETURN def.code = a.code END EqualDefault;

PROCEDURE BuildDefault(def : Default) : REFANY =
  BEGIN RETURN RTAllocator.NewTraced(def.code) END BuildDefault;

BEGIN END ObjectFactory.
