(* $Id$ *)

INTERFACE BaseLayer;
IMPORT Tint;
IMPORT PaintLayer;
IMPORT Word;

TYPE 
  T <: Public;

  Public = PaintLayer.T OBJECT tint : Tint.T; priority : CARDINAL END;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;
PROCEDURE Hash(READONLY a : T) : Word.T;
PROCEDURE Compare(READONLY a, b : T) : [-1..1];

CONST Brand = "BaseLayer";

END BaseLayer.
