(* $Id$ *)
INTERFACE MagTile;
IMPORT Word;
IMPORT MagCell;
IMPORT TextTextTbl;

TYPE 
  T <: Public;

  Public = OBJECT METHODS

    (* constructors *)
    makeFromMagCell(src : MagCell.Labelled) : T;
    (* construct from a MagCell *)

    makeHorizontally(READONLY a : ARRAY OF T) : T;
    (* construct by tiling horizontally *)

    makeVertically(READONLY a : ARRAY OF T) : T;
    (* construct by tiling vertically *)

    makeByRotateCCW(old : T) : T;
    (* construct by rotating *)

    makeSideways(old : T) : T;
    (* construct by flipping sideways *)

    makeUpsideDown(old : T) : T;
    (* construct by flipping upside down *)

    makeByOverlay(old1 : T; old2 : T) : T;
    (* construct by overlaying another *)

    makeEmpty(w, h : CARDINAL) : T;
    (* construct an empty one *)

    dump(typeName : TEXT := NIL) : MagCell.T;
    (* dumper -- turn back into a MagCell.
                 Set typeName if you want a specific name for the cell's
                 type; else the routine picks an arbitrary name (perhaps
                 not very useful).
    *)

    changeLabels(a, b : TEXT) : T;
    (* change labels a to b *)

    changeLabelSet(labels : TextTextTbl.T) : T;
    (* change a whole set of labels
       each key label will be changed into the entry label *)

    (* attributes *)
    getHeight() : CARDINAL;
    getWidth() : CARDINAL;
  END;

  
CONST Brand = "MagTile";

PROCEDURE Equal(a, b : T) : BOOLEAN;
PROCEDURE Hash(a : T) : Word.T;

END MagTile.
