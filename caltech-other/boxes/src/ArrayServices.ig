GENERIC INTERFACE ArrayServices (Elem);

(* Elem must include a Brand *)
(* Elem.T must be a reference type---the default value is NIL *)
(* an "ungotten" element is always NIL *)

TYPE
  T <: ROOT;
  

PROCEDURE Get(arr : T; READONLY index : INTEGER) : Elem.T;

PROCEDURE Set(arr : T; READONLY index : INTEGER; READONLY elem : Elem.T);

CONST
  Brand = "ArrayServices(" & Elem.Brand & ")";

END ArrayServices.
