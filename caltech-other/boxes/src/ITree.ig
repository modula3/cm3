GENERIC INTERFACE ITree(Elem);

(* Elem.T must be a reference type *)
(* Must have Elem.Brand *)

TYPE
  T <: REFANY;

  Interval = RECORD lo, hi : INTEGER; END;

CONST
  Brand = "ITree of" & Elem.Brand;

PROCEDURE New() : T;
PROCEDURE MarkInterval( x : T; interval : Interval; with : Elem.T);
PROCEDURE UnMarkInterval( x : T; interval : Interval);
PROCEDURE Search(x : T; for : INTEGER) : Elem.T;

END ITree.
