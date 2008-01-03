(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu May  5 22:03:31 PDT 1994 by najork                   *)
(*       Created on Wed Feb 16 17:12:53 PST 1994 by najork                   *)

(* "X_PEX_Base.T" is a subtype of \type{GraphicsBase}{T}. An object of 
   this type provides an abstract interface to X windows and PEX. *)

INTERFACE X_PEX_Base;

IMPORT GraphicsBase;

TYPE 
  T <: Public;
  Public = GraphicsBase.T OBJECT
  METHODS
    init (title : TEXT; win_x, win_y := 10; win_w, win_h := 500) : T
        RAISES {GraphicsBase.Failure};
    changeTitle (title : TEXT);
    awaitDelete ();
    destroy ();
  END;

(* "gb.init" initializes a new graphics base and returns it. As a side
   effect, it creates a window "win_w" by "win_h" pixels in size, and
   "win_x","win_y" pixels offset from the upper left corner of the screen. 

   "gb.changeTitle(title)" changes the title of the X window associated
   with "gb" to "title". 

   Calling "gb.awaitDelete ()" suspends the calling thread until the window
   associated with "gb" gets destroyed. 

   Calling "gb.destroy()" destroys the X window. *)

PROCEDURE Available () : BOOLEAN;
(* "Available()" returns "TRUE" if PEX is supported by the current X server. *)

END X_PEX_Base.
