(* $Id$ *)

INTERFACE MagCell;
IMPORT Thread, Rd, Wr;
IMPORT MagRect, MagLayer AS Layer, MagLayerSet AS LayerSet, TextMagLayerTbl AS TextLayerTbl;
IMPORT MagLayerRect;
IMPORT MagTransform;
IMPORT OSError;
IMPORT MagMergeDB;
IMPORT MagLabel;
IMPORT MagRectSet;
IMPORT Word;

EXCEPTION
  NotFound(TEXT (* name of cell missing *));
  SyntaxError;

TYPE
  (* Can't add labels to a T *)
  T <: Public;

  (* Can add labels to a Labelled *)
  Labelled <: T;

  RectProc = PROCEDURE (rect : MagRect.T; layer : Layer.T; args : REFANY);

  LayerRectProc = PROCEDURE (rect : MagLayerRect.T; args : REFANY);
  LabelProc = PROCEDURE (label : MagLabel.T; args : REFANY);

  (* no-special-args versions *)
  LayerRectProc2 = PROCEDURE (rect : MagLayerRect.T);
  LabelProc2 = PROCEDURE (label : MagLabel.T);

  CellProc = PROCEDURE (subCell : T; 
                        id : TEXT; transform : MagTransform.T; 
                        parentId : TEXT; args : REFANY) RAISES ANY;

  Public = OBJECT METHODS
    
    empty() : BOOLEAN;
    (* empty returns TRUE if the cell has no: rectangles, labels, or subcells
       in it *)
    
    havePaint() : BOOLEAN;
    (* returns TRUE if any paint *)
    
    haveSubCells() : BOOLEAN;
    (* returns TRUE if any subcells *)
       
    haveLabels() : BOOLEAN;
    (* returns TRUE if any labels *)
       
    getBBox() : MagRect.T;
    (* get bounding box in cell's own coordinate system *)

    (* map all rects in flattened version of cell thru rectProc *) (*
       will "best effort" clip against the clip rectangle; it does not
       necessarily guarantee that rectangles outside the view frustrum
       are clipped *) 
    (* if clip is NIL, then no clipping is performed *)
    (* clip is interpreted w.r.t. the top-level cell *)
    flatClipMap(rectProc : RectProc;
                args     : REFANY        := NIL ; (* extra args for rectProc *)
                clip     : REF MagRect.T := NIL );

    flatClipMap2(rectProc : LayerRectProc;
                 args     : REFANY       := NIL;
                 clip     : REF MagRect.T := NIL );

    (* map all rects---paint as well as labels---through passed-in procs *)
    (* a proc not needed can be passed as NIL *)
    flatMapAll(rectProc : LayerRectProc;
               labelProc: LabelProc;
               args : REFANY := NIL);

    flatMapAllNoArgs(rectProc : LayerRectProc2;
                     labelProc: LabelProc2);

    flatMapLabels(labelProc: LabelProc;
               args : REFANY := NIL);

    (* map all the cells past; it also unrolls arrays, etc. *)
    subCellMap(cellProc : CellProc; args : REFANY := NIL) RAISES ANY;

    (* get a blank cell *)
    init(newName : TEXT) : T;

    initByFlattening(newName : TEXT; toFlatten : T) : T;
    (* initialize a new & newly-named cell by flattening an existing
       cell; all rects and labels are kept where they were; no
       subcells are kept *)
    (* timestamp is set to the timestamp of the source *)

    lookup(name : TEXT; 
           layerDB : TextLayerTbl.T;
           fillInLayers := FALSE;
           quiet := FALSE) : T RAISES { NotFound, 
                                             Thread.Alerted, 
                                             Rd.Failure,
                                             SyntaxError };
    (* initialize from a file *)
    (* if fillInLayers is TRUE, lookup will automatically fill in any 
       missing layers in the layerDB.

       Note that the layerDB will be accessed under a mutex even if 
       lookup is called from several threads, so a single layerDB may 
       be shared between concurrent lookups. 
    *)

    getName() : TEXT;

    write(fileName : TEXT := NIL; mergeDB : MagMergeDB.T := NIL) RAISES { Thread.Alerted, Wr.Failure, OSError.E };
    (* write the cell to its file *)
    (* if the fileName is NIL, then the default one is used *)
    (* ".mag" is NOT automatically added *)
    
    writeToWriter(wr : Wr.T; mergeDB : MagMergeDB.T := NIL) RAISES { Thread.Alerted, Wr.Failure };
    (* write the cell to a writer.
       when done, CLOSES wr *)

    writeRecursivelyToDirectory(dirName : TEXT) RAISES { Thread.Alerted, Wr.Failure, OSError.E };
    (* write everything to a directory *)


    getDebugPath() : TEXT;
    
    debugDumpData();
    (* for debugging---dump contents of cell on terminal in human-readable(?)
       format *)
       

    layerRectsBbox(layer : Layer.T; VAR bbox : MagRect.T) : BOOLEAN;
    (* search for rects of given layer; update bbox and return TRUE if any
       exist; else return FALSE *)

    layerSetRectsBbox(layerSet : LayerSet.T; VAR bbox : MagRect.T) : BOOLEAN;

    (* find the named labels.  if set is non-NIL, they will be inserted
       into that set; else a set will be allocated.  in either case the
       set will be returned *)
    findLabels(named : TEXT; set : MagRectSet.T) : MagRectSet.T;

    tightenBBox();
    (* tighten up the bbox so it is exact.  the bbox remains exact until
       objects are removed from the cell; it ALWAYS covers all of the cell,
       but sometimes more. *)
  END;

CONST Brand = "MagCell";

PROCEDURE Equal(a, b : T) : BOOLEAN; (* pointer equality *)
PROCEDURE Hash(a : T) : Word.T;

END MagCell.
