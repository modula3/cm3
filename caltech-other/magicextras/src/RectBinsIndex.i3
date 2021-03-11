(* $Id$ *)

INTERFACE RectBinsIndex;
IMPORT MagLayer AS Layer, Word;
IMPORT IntPair;

(* indexing used for the RectBins *)

TYPE
  T = RECORD c : IntPair.T; l : Layer.T END;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;
PROCEDURE Hash(READONLY a : T) : Word.T;

PROCEDURE Format(READONLY a : T) : TEXT;

CONST Brand = "RectBinsIndex";

END RectBinsIndex.
