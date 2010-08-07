(*--------------------------------------------------------------------------*)
GENERIC INTERFACE Tuple(Elem);
(* generic object type for tuples *)

(*--------------------------------------------------------------------------*)
CONST Brand = Elem.Brand & "Tuple";

(*--------------------------------------------------------------------------*)
TYPE
  T <: Public;

  Public = OBJECT METHODS
 
    init (fst, scd : Elem.T) : T;
    (* initialize first and second components *)

    getFst () : Elem.T ;
    getScd () : Elem.T
    (* get first or second component 

       Error Conditions
         self = NIL *)

  END;

(*--------------------------------------------------------------------------*)
END Tuple.
