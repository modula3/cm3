(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: PSReaderGuts.i3,v 1.2 2001-09-19 15:30:31 wagner Exp $ *)

INTERFACE PSReaderGuts;
IMPORT DrawContext;
IMPORT LinoText;
TYPE
  T <: Public;
  Public = OBJECT METHODS
    init(to: DrawContext.T): T;
    

    (* the methods below are called as PS file is parsed *)

    save();
    restore();
    scale(h, v: REAL);
    translate(h, v: REAL);
    
    setLineWidth(w: REAL);
    newPath();
    moveTo(h, v: REAL);
    lineTo(h, v: REAL);
    closePath();
    stroke();

    scaleFont(s: REAL);
    show(t: TEXT; attach: LinoText.Attach);
  END;
END PSReaderGuts.
