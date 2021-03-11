(* $Id$ *)

GENERIC INTERFACE TblMap(Tbl, Map);

(* 
   A TblMap.T is a wrapper for a Tbl, an object that obeys the generic
   Table interface at least to the extent of its get method, and that
   allows the Table to be accessed as a Map 
*)

IMPORT MapError;

TYPE
  T <: Public;

  Public = Map.T OBJECT METHODS
    tbl() : Tbl.T; (* allow access to underlying tbl, probably best to use
                      only during initialization *)

    error(operand : Map.Argument) RAISES { MapError.E };
    (* default just raises error(NIL) *)
  END;

  Table = Tbl.T;

PROCEDURE Wrap(tbl : Tbl.T; t : T := NIL) : Map.T; 
  (* return a Map for which a nonexistent mapping is an error *)

PROCEDURE WrapWithDefault(tbl : Tbl.T; default : Map.Result) : Map.T; 
  (* return a Map for which a nonexistent mapping is an error *)

CONST Brand = "TblMap(" & Tbl.Brand & "," & Map.Brand & ")";

(* for further generics *)

END TblMap.
