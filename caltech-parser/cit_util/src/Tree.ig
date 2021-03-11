(* $Id$ *)

GENERIC INTERFACE Tree(Elem);

(* Elem must implement Compare : [-1..1]  
                       Brand
                       Equal : BOOLEAN

                       Equal must imply result of Compare = 0 (but not v.v.)
                       
 *)

TYPE 
  T = OBJECT METHODS
    insert(e : Elem.T);

    copy() : T;
(* copy the tree, copying the elements with := *)

    iterate() : Iterator;
(* iterate in any order *)

    iterateOrdered() : Iterator;
(* iterate in sorted order *)

    lowerBound(e : Elem.T; VAR lowerBound : Elem.T) : BOOLEAN;
(* get lower bound of e in T *)

    successor(e : Elem.T; VAR successor : Elem.T) : BOOLEAN;
(* get next element after e in T *)

    delete(e : Elem.T) : BOOLEAN;
(* delete an element Equal to e and return TRUE if succeeded *)
  END;

  Iterator <: PublicIterator;

  PublicIterator = OBJECT METHODS
    next(VAR next : Elem.T) : BOOLEAN;
  END;

  PublicDefault = T OBJECT METHODS
    init() : T;
  END;

  Default <: PublicDefault; (* unbalanced impl. *)

CONST 
  Brand = "Tree of " & Elem.Brand;

END Tree.

