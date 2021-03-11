(* $Id$ *)

INTERFACE FrameBuffer;
IMPORT MagPoint, MagRect;
IMPORT PaintLayer, TextMagLayerTbl AS TextLayerTbl;

(* A FrameBuffer.T is a drawable region.  It clips automatically. *)

(* XXX to improve the efficiency of the garbage collector, the frame buffer
   contains an UNTRACED drawable region.  It is not garbage collected.
   Do not let FrameBuffer.T's go out of scope unless you want a memory leak.

   Future modification: use WeakRef to DISPOSE buffer when the parent
   object goes out of scope. 
*)

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(tbl : TextLayerTbl.T; maxBytes , width : CARDINAL;
         VAR height : CARDINAL; cellHeight : CARDINAL) : T;
    
    (* set ll of buffer *)
    setOffset(ll : MagPoint.T);

    getExtent() : MagRect.T;

    (* clear buffer *)
    clear();

    (* call destroy before letting object go out of scope *)
    (* XXX as yet (possibly forever) unimplemented, see above *)
    destroy();

  END;

  (* DrawRect draws a rectangle at the specified location, 
     in global coords *)
PROCEDURE DrawRect(frameBuffer : T; 
                   READONLY at : MagRect.T; READONLY layer : PaintLayer.T);

CONST Brand = "FrameBuffer";

END FrameBuffer.
