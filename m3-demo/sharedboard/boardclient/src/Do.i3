(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

(* The "Do" interface tracks the creations and deletions of items in a
   "Win.T" and allows them to be undone later. *)

INTERFACE Do;

IMPORT Win, Item;

TYPE T <: Public;
     Public = OBJECT METHODS
       init (wn: Win.T): T;
       createItems (its: Item.TArray);
       deleteItems (its: Item.TArray);
       undo () RAISES {NoInfo};
     END;

EXCEPTION NoInfo;

(* The method "createItems(its)" records the information and calls
   "View.createItems". 
   The method "deleteItems(its)" records the information and calls
   "View.deleteItems" with the IDs of "its". 
   
   Note that there is no provision for undoing modificatoins to the
   items. This is because "Win.T" often modifies items in place (as
   in moving items). But this can be easily fixed.

   Calling "undo" undoes the last recorded "createItems" or
   "deleteItems" and removes that record. The "undo" itself is not
   recorded and cannot be undone. If there is no record, the method raises
   the "NoInfo" exception.
*)   

END Do.
