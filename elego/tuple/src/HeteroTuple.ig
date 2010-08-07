(*--------------------------------------------------------------------------*)
GENERIC INTERFACE HeteroTuple (FstElem, ScdElem);
(* generic object type for heterogeneous tuples *)

(*--------------------------------------------------------------------------*)
CONST Brand = FstElem.Brand & ScdElem.Brand & "Tuple";

(*--------------------------------------------------------------------------*)
TYPE
  T <: Public;

  Public = OBJECT METHODS
 
    init (fst : FstElem.T; scd : ScdElem.T) : T;
    (* initialize first and second components *)

    getFst () : FstElem.T ;
    getScd () : ScdElem.T
    (* get first or second component 

       Error Conditions
         self = NIL *)

  END;

(*--------------------------------------------------------------------------*)
END HeteroTuple.
