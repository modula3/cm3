(* $Id$ *)

INTERFACE Conf;
IMPORT Rd, TextMagLayerTbl AS TextLayerTbl;
IMPORT Thread;
IMPORT MagLayer AS Layer;
IMPORT IntList;
IMPORT MagMergeDB;

EXCEPTION 
  LayerNotFound((* layer name *) TEXT);
  ParseError((* line # *) CARDINAL);

PROCEDURE ReadConf(rd : Rd.T) : TextLayerTbl.T RAISES { LayerNotFound, Rd.Failure, Thread.Alerted, ParseError } ;

PROCEDURE RouteLevelLookup(layer : Layer.T) : INTEGER RAISES { LayerNotFound };
PROCEDURE LayerLookup(level : INTEGER) : Layer.T RAISES { LayerNotFound };

(* the client may not modify the return values of this procedure *)
PROCEDURE GetObstructedRoutingLayers(layer : Layer.T) : IntList.T;

PROCEDURE ViaLookup(from, to : Layer.T) : Layer.T;

PROCEDURE MergeDB() : MagMergeDB.T;

END Conf.
