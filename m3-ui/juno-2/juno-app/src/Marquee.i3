(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sun Oct 25 13:40:46 PST 1992 by heydon                       *)
(*      modified on Mon Oct 19 10:22:51 1992 by gnelson                      *)
(*      modified on Fri Aug  7 21:51:55 PDT 1992 by myers                    *)

(* A "Marquee.T" displays a vertical marquee of lines of the form

    name arg1 arg2 ... argN

where "name" and the "arg"'s are texts.  The lines are indented so that
the spaces after "name" in each line are vertically aligned.
The window has a fixed height and width.  *)

INTERFACE Marquee;

IMPORT VBT, Font;

TYPE 
  T <: Public;
  Public = Private OBJECT METHODS
    init(height := 8; nameWidth := 10; argWidth := 12; font := Font.BuiltIn): T;
    putName(nm: TEXT);
    putArg(nm: TEXT);
    remArg();
    erase();
    newline();
  END;
  Private <: VBT.T;

(* The call "m.erase()" erases the current line.

   The call "m.newline()" ends the current line and advances to
   a new blank one.

   The call "m.putName(nm)" requires that "m" be at the beginning
   of a line; it adds the name "nm".  

   The call "m.putArg(nm)" adds the argument "nm" to the current
   line; it requires that the current line have a name.

   The call "m.remArg()" removes the last added argument; it
   requires that this argument exist.

   The arguments to "init" give the fixed height of the window,
   the width allowed for the name part, and the width allowed
   for the argument part.  The units of the dimensions are
   the widths of a character in the specified font, which is
   assumed to be fixed-width. 
   
   It is required that height be at least two. *)

END Marquee.
   
    
