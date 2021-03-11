(* $Id$ *)

INTERFACE RectSet;
IMPORT MagLayerRectSetDef;
IMPORT MagRect;
IMPORT Word;

TYPE 
  T <: Public;

  Public = MagLayerRectSetDef.T OBJECT METHODS
    clip(READONLY against : MagRect.T) : T;
    (* clip the rects against a frustrum *)

    keepOverlapping(READONLY against : MagRect.T) : T;
    (* same as above, but keep rects intact *)
  END;

CONST Brand = "RectSet";

PROCEDURE Distance(READONLY a, b : T) : LONGREAL;
PROCEDURE Equal(a, b : T) : BOOLEAN;
PROCEDURE Hash(a : T) : Word.T; (* XXX inefficient *)

END RectSet.

