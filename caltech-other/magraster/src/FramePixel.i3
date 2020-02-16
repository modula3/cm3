(* $Id$ *)

INTERFACE FramePixel;
IMPORT Word;

(* a pixel at layout precision, just a bit vector *)

TYPE T = ARRAY OF Word.T;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;
PROCEDURE Hash(READONLY a : T) : Word.T;
PROCEDURE Format(READONLY a : T) : TEXT;

(* Return an uninitialized FramePixel.T of the same size as a *)
PROCEDURE Clone(READONLY a : T) : REF T;

CONST Brand = "FramePixel";

END FramePixel.
