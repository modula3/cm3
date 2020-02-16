(* $Id$ *)
INTERFACE ForbiddenVias;
IMPORT MagCell, GridPoint, MagRect;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(layout : MagCell.T) : T;
    isForbidden(viaRect : MagRect.T; level : GridPoint.Layer) : BOOLEAN;
  END;

CONST 
  Brand = "ForbiddenVias";
    
END ForbiddenVias.
