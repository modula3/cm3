(* $Id$ *)

GENERIC INTERFACE Memo(Map,Tbl);

(* A Memo.T is a wrapper for a Map.T that memoizes the results of 
   calculations using a Table *)

TYPE
  T <: Public;

  Public = Map.T OBJECT METHODS
    init(tbl : Tbl.T := NIL) : T; (* if tbl NIL, will allocate Tbl.Default *)
    realEval(x : Map.Argument) : Map.Result; (* must override this *)
  END;

CONST Brand = "Memo(" & Map.Brand & "," & Tbl.Brand & ")";

END Memo.
