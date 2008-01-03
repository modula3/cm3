(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu Jun 23 14:39:56 PDT 1994 by najork                   *)
(*       Created on Fri Feb  4 15:21:43 PST 1994 by najork                   *)

(* An "AmbientLightGO.T" is a geometric object that describes a source of
   ambient light. *)


INTERFACE AmbientLightGO;

IMPORT Color, LightGO;

TYPE
  T <: Public;
  Public = LightGO.T OBJECT
  METHODS
    init () : T;
  END;
(* "l.init()" initializes a new ambient light source "l" and returns it. *)


PROCEDURE New (c : Color.T) : T;
(* "New (c)" creates a new ambient light source "l" and returns it.
   It also attaches the following properties to "l":
   \begin{verbatim}
      "(LightGO.Colour,ColorProp.NewConst(c))" 
      "(LightGO.Switch","BooleanProp.NewConst(TRUE))"
   \end{verbatim} 
*)

END AmbientLightGO.
