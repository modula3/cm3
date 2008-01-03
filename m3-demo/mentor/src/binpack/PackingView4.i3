(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Sat Aug  8 00:22:16 PDT 1992 by mhb *)

INTERFACE PackingView4;

IMPORT PackingView3;

(* Displays the packing of weights within each bin, and allows
   the user to click on a weight to be deleted from the packing.
   When the user clicks on a weight, it is highlighted.  When the
   mouse is released, the algorithm is notified of the
   user-request to delete a weight.  The user can change which
   weight is to be deleted by dragging the mouse.  The user can
   cancel the action by chording or lifting up while not on a
   weight. *)

TYPE 
  T <: PackingView3.T;

END PackingView4.


