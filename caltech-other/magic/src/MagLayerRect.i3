(* $Id$ *)

INTERFACE MagLayerRect;
IMPORT MagRect, MagLayer, Word;

(* a MagLayerRect.T is a complete description of a layout rect. *)
(* it includes a MagRect + a MagLayer for complete identification *)

TYPE T = RECORD rect : MagRect.T; layer : MagLayer.T END;

CONST Brand = "MagLayerRect";

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

PROCEDURE Hash(READONLY a : T): Word.T;

(* Two LayerRect.T's are electrically connected if this is true. *)
(* Note that the layers must be RouteLayer.T's, or a NARROW will fail. *)
PROCEDURE ElectricallyConnected(READONLY a, b : T) : BOOLEAN;

(* Manhattan distance from a to b; layerPenalty is added if the layers differ *)
PROCEDURE Distance(READONLY a, b : T; layerPenalty : CARDINAL := 0) : CARDINAL;

PROCEDURE Format(READONLY a : T) : TEXT;

END MagLayerRect.
