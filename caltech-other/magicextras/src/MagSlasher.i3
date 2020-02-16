(* $Id$ *)

INTERFACE MagSlasher;
IMPORT MagCell;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(c : MagCell.Labelled) : T;

    slash(label : TEXT; VAR slashDotted : TEXT) : BOOLEAN;
    (* convert a dotted or slash-dotted label into a label whose arcs
       are connected by slashes to denote cells and dots to denote arcs
       within instance names.

       "slashDotted" is not touched and FALSE is returned if not found.
    *)
  END;

CONST Brand = "MagSlasher";

END MagSlasher.

    
