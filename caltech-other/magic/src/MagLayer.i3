(* $Id$ *)

INTERFACE MagLayer;
IMPORT Word;

TYPE 
  T <: Public;

  Public = OBJECT METHODS init() : T; END;

CONST
  Brand = "Layer";

PROCEDURE Equal(a, b : T) : BOOLEAN;
PROCEDURE Hash(a : T) : Word.T;

END MagLayer.
