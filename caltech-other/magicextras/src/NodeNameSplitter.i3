(* $Id$ *)

INTERFACE NodeNameSplitter;

(* A NodeNameSplitter will take a period-delimited node name and split *)
(* it into successive possible variants of node name and cell name *)
(* The process starts from the longest possible cell name and goes to *)
(* the longest possible node name.  This ensures that we match the node *)
(* in the leaf cell rather than futilely searching the top-level cells *)
(* for nonexistent nodes *)

TYPE 
  T <: Public;

  Public = OBJECT METHODS
    init(name : TEXT) : T;
    next(VAR cellName, nodeName : TEXT) : BOOLEAN
  END;

CONST Brand = "NodeNameSplitter";

END NodeNameSplitter.
