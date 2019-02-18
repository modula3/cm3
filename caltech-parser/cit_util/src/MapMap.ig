(* $Id$ *)

GENERIC INTERFACE MapMap(Map1, Map2);

(* A MapMap.T is a Map that has type Map2.T but delegates to a type of
   Map1.T *)

TYPE
  T <: Public;

  Public = Map2.T OBJECT METHODS
    init(map1 : Map1.T) : T;
  END;

PROCEDURE Wrap(map1 : Map1.T) : T;
  (* NEW(T).init(map1) *)

CONST Brand = "MapMap(" & Map1.Brand & "," & Map2.Brand & ")";

END MapMap.

  
