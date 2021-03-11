(* $Id$ *)
GENERIC INTERFACE MST(Elem, ElemSeq);
IMPORT CardPairSet;

(* compute minimum spanning tree between nodes in a Sequence of Elem.T's *)
(* The Elem interface must contain a procedure Distance(a, b : Elem.T) : LONGREAL  and a TEXT CONSTant Brand *)

CONST Brand = "MST of " & Elem.Brand;

TYPE
  T <: Public;

  (* NOTE CAREFULLY:

     if graph given is not connected (in edges or edgeAr), what comes
     out has more edges than the MST...

     there can obviously be no MST in that case, but sometimes the
     results may be useful, anyhow... maybe? *)

  Public = OBJECT METHODS
    init(seq : ElemSeq.T; 
         (* optional edge lists... *)
         
         (* Alternative I. *)
         edges : CardPairSet.T := NIL; (* set of pairs of integers *)

         (* Alternative II. *)
         edgeAr : REF ARRAY OF ARRAY OF CARDINAL := NIL;(* array of edges *)
         edgeCounts : REF ARRAY OF CARDINAL := NIL     (* no. of entries/vert*)
    ) : T;
    (* edges should be carefully checked so it has no duplicates.. the 
       pairs are really unordered! *)

    maxLinkLength() : LONGREAL;

    (* this returns the number of EDGES, 1 less than the no. of vertices.. *)
    size() : CARDINAL;
    
    (* get the Elem's that constitute the endpoints of link i *)
    (* it is a checked run-time error to give an index > size - 1 *)
    getLink(index : CARDINAL; VAR from, to : Elem.T; VAR length : LONGREAL); 

    (* total weight of MST *)
    totalWeight() : LONGREAL;
  END;


END MST.
