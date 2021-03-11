(* $Id$ *)

INTERFACE MagCellExtendable;
IMPORT MagCell, Word;
IMPORT MagSubCell AS SubCell, MagRect, MagLayer AS Layer;
IMPORT MagTimeStamp;
IMPORT MagLabel;
IMPORT MagLabelList AS LabelList;
IMPORT MagLayerRect, MagLayerRectSet;
IMPORT TextCellInstanceTbl;
IMPORT TextSet;

REVEAL
  MagCell.T <: Public;
  MagCell.Labelled <: PublicLabelled;

EXCEPTION
  ReadOnlyTimeStamp; (* the cell's timestamp is single-assignment *)
  NoSuchSubcell;     (* named subcell doesn't exist *)

(* A CellWriter is a handle on a "session": open a writer using the MagCell;
   then writes can be undone per session *)

TYPE 
  CellWriter <: PublicCellWriter;

  PublicCellWriter = OBJECT METHODS
    iterate() : LayerRectIterator;
    rectSet() : MagLayerRectSet.T;
    id() : Word.T;
  END;

  LayerRectIterator <: PublicLayerRectIterator;

  PublicLayerRectIterator = OBJECT METHODS
    next(VAR rect : MagLayerRect.T) : BOOLEAN
  END;

(* WARNING!
   ========

   bbox isn't kept completely up to date.

   it is only guaranteed to be at least as large as the actual bbox.

   using rollbacks or deletions may cause it to be larger than that. 

   In the future, we should add a routine that fixes up the bboxes of
   an entire layout tree. *)

TYPE
  Public = MagCell.Public OBJECT
  METHODS
    (* should have methods here to add rects, subcells, arrays of subcells *)

    (* if layerList is NIL, then the rect isn't added, but it still extends
       the bbox *)
    addRect(READONLY rect : MagRect.T; layer : Layer.T);
    addLayerRect(READONLY layerRect : MagLayerRect.T);
    addLayerRectSet(layerRectSet : MagLayerRectSet.T);

    newSession() : CellWriter;
    sessAddRect(wr : CellWriter; READONLY rect : MagRect.T; layer : Layer.T);
    sessAddLayerRect(wr : CellWriter; READONLY layerRect : MagLayerRect.T);
    sessAddLayerRectSet(wr : CellWriter; layerRectSet : MagLayerRectSet.T);
    rollbackSession(wr : CellWriter);

    addSub(READONLY sub : SubCell.T);
    addLabel(READONLY lab : MagLabel.T);
    setTimeStamp(timeStamp : MagTimeStamp.T) RAISES { ReadOnlyTimeStamp };

    getSubCell(useId : TEXT; VAR sub : SubCell.T) : BOOLEAN;
    
    delSub(useId : TEXT) RAISES { NoSuchSubcell };
(* delete subcell named useId; raises NoSuchSubcell if it doesn't exist,
   recomputes bounding box (may be slow!!!) *)

    flattenSubOneLevel(useId : TEXT;
                       okToRename : TextSet.T) RAISES { NoSuchSubcell };
    (* take everything inside indicated subcell and bring it up one level.
       It is permissible to rename any cells with useIds in the set okToRename. *)
                      

    flatten() : TextCellInstanceTbl.T;

  END;


  PublicLabelled = MagCell.T OBJECT METHODS
    (* stuff to deal with labels *)
    getLabels(name : TEXT) : LabelList.T;
  END;

END MagCellExtendable.
