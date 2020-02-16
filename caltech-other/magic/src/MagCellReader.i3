(* $Id$ *)

INTERFACE MagCellReader;
IMPORT MagCell, Thread, Rd, TextMagLayerTbl AS TextLayerTbl;

PROCEDURE ReadFromRd(res : MagCell.T; rd : Rd.T; 
                     path : TEXT;
                     layerDB : TextLayerTbl.T;

                     (* if fillInLayers is TRUE, then we will automatically
                        add previously unknown layers to the layerDB

                        Note that even if ReadFromRd is called from 
                        multiple threads, only a single thread at a time
                        will attempt to update the layerDB, so layerDB
                        need not include locking.
                     *)
                     fillInLayers : BOOLEAN;
                     quiet : BOOLEAN := FALSE) : MagCell.T RAISES { MagCell.NotFound, 
                                                       Thread.Alerted, 
                                                       Rd.Failure,
                                                       MagCell.SyntaxError};

END MagCellReader.
