(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

(* The "RuleItem" interface defines an item for a rectangular blob
   parallel to the board axes.
*)

INTERFACE RuleItem;

IMPORT Color,
       Item;

TYPE T <: Public;
     Public = Item.T OBJECT
       color := Color.Black;
     END;

(* A "RuleItem.T" is visible as a rectangle given by its "box" (see "Item.T").
   The rectangled is filled with the color "color".
*)

END RuleItem.
