(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

(* The "ItemClass" interface reveals methods useful to paint an "Item.T". 
*)

INTERFACE ItemClass;

IMPORT VBT, 
       PointR, Item, Focus;

REVEAL Item.T = Item.Public BRANDED "Item" OBJECT
  METHODS
    paint (v: VBT.T; focus: Focus.T);
    hilite (v: VBT.T; focus: Focus.T);
    unhilite (v: VBT.T; focus: Focus.T);
    move (delta: PointR.T);
  END;

(* The call "paint (v, focus)" causes the item to paint itself on the
   window "v". The "focus" information is provided by the window to
   transform the figure according to its focus.
   
   The method "hilite" causes the item to highlight itself in some
   way; it is typically used to display items that have been selected by the
   user. The method  "unhilite" switches off the highlighting.

   The "move" method causes the item to relocate its position
   dependent attributes, including its bounding box, by "delta". It
   does not cause the item to paint itself.
*)

END ItemClass.

