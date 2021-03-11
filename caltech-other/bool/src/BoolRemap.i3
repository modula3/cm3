(* $Id$ *)

INTERFACE BoolRemap;
IMPORT BoolBoolTbl;
IMPORT Bool, BoolFormatter;

TYPE 
  Map = T;

  T <: Public;

  Public = BoolBoolTbl.Default OBJECT METHODS
    format(bf : BoolFormatter.T) : TEXT; (* debugging *)

    (* FOR each variable pair b0, b1 in the map, change the occurrence of
       b0 to that of b1.  If check is TRUE, then abort if there are variables
       (except True and False) that have no mapping in the map. *)
    remap(b0 : Bool.T; check := FALSE) : Bool.T;

    (* merge m2 into map and return map *)
    mergeD(m2 : T) : T;

    (* call the parent method... *)
    init() : T;
  END;

(* create new T and merge m1 and m2 into it *)
PROCEDURE Merge(m1, m2 : Map) : Map;

(* the empty map *)
PROCEDURE Empty() : Map;

CONST Brand = "BoolRemap";

END BoolRemap.
