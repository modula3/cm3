(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

(* The "TextItem" interface defines an item for a single line of text.
*)

INTERFACE TextItem;

IMPORT Color,
       Item, PointR, ItemFont;

TYPE T <: Public;
     Public = Item.T OBJECT
       text: TEXT;
       rp: PointR.T;
       font: ItemFont.T;
       color := Color.Black;
     END;

(* The "text" is a single line of visible characters.
   The "rp" is the reference point of the text in the board coordinates.
   The "font" is the spceially designed font format in board coordinates.
   The "color" is the foreground color in which the text will be painted.
*)

END TextItem.
