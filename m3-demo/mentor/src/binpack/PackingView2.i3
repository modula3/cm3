(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Fri Aug  7 17:25:18 PDT 1992 by mhb *)

INTERFACE PackingView2;

IMPORT BinpackViewClass, GraphVBT;

(* Displays the packing of the weights within each bin.  The view
   is non-pickable.  However, unlike PackingView1, this view does
   respond to the "RepackBin" update event.  As such, the
   implementation maintains a list of the display objects
   corresponding to each weight.  This interface exposes a TYPE
   for this view, in order to be subclassed.  The implementation
   guarantees that Filter.Child(v:T) is a GraphVBT.T that is
   returned by the "createGraph" method, and each weight is
   displayed as an object of type Weight in the GraphVBT.T. *)

TYPE
  Weight = GraphVBT.Vertex OBJECT
             id : CARDINAL;
             amt: REAL
           END;

  T <: Public;
  Public = BinpackViewClass.T OBJECT
             curr: Weight;      (* set by oeSetup *)
           METHODS
             createGraph (nBins, nWts: INTEGER): GraphVBT.T;
             (* Called in oeSetup method to return a newly
                allocated and initialized GraphVBT.  LL=0. *)
             createWeight (id: INTEGER; amt: REAL): Weight;
             (* Called in oeNewWeight method to return a newly
                created and initialized Weight.  LL=0. *)
           END;

END PackingView2.


