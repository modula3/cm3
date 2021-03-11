(* $Id$ *)

(* this interface defines a LayerIntTbl as an extension of a generically
   instantiated hash table.  The extension is to add a mapping from pixels
   to tints.  This is to allow alpha blending *)

INTERFACE LayerIntTbl;
IMPORT LayerIntImplTbl AS Impl;
IMPORT PixelTintTbl;
IMPORT MagLayer;
IMPORT FramePixel, Tint;

TYPE T = Impl.T;
TYPE 
  Default <: DefaultPublic;
  DefaultPublic = Impl.Default OBJECT 
    pixelTintTbl : PixelTintTbl.T;

    (* these may be used for caching the last value.  See AlphaBlend.Blend *)
    cachedKey : REF FramePixel.T := NIL;
    cachedVal : Tint.T; 
  METHODS
    getLayer(n : CARDINAL) : MagLayer.T;
  END;

END LayerIntTbl.
