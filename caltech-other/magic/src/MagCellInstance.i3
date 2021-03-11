(* $Id$ *)

INTERFACE MagCellInstance;
IMPORT MagCell, MagTransform;
IMPORT Word;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(definition : MagCell.T; id : TEXT; transform : MagTransform.T;
         parent : T) : T;
    getDef() : MagCell.T;
    getTransform() : MagTransform.T;
  END;

CONST Brand = "MagCellInstance";
PROCEDURE Hash(a : T) : Word.T;
PROCEDURE Equal(a, b : T) : BOOLEAN;

END MagCellInstance.
