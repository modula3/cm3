(* $Id$ *)

INTERFACE AlphaBlend;
IMPORT FramePixel, Tint, LayerIntTbl;

PROCEDURE Blend(READONLY pixel : FramePixel.T; pens : LayerIntTbl.Default) : Tint.T;

(* color of "base" (white), alpha channel is ignored *)
CONST BaseTint = Tint.White;

END AlphaBlend.
