(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Mar 27 19:32:18 PST 1996 by heydon                   *)

INTERFACE JunoZeus;

IMPORT RemoteView, JunoConfig, View, FormsVBT, VBT;

TYPE
  T <: Public;
  Public = RemoteView.T OBJECT
    w: VBT.T
  METHODS
    init(fv: FormsVBT.T; rt: View.Root; origin := JunoConfig.Origin.SW): T
  END;

(* The "init" method sets "rt.animView" to a new animation view, and
   initializes the "w" field to a "DblBufferVBT.T" over that view. The origin
   of the animation view is set according to "origin". *)

END JunoZeus.
