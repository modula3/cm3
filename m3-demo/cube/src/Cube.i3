(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun 16 18:30:07 PDT 1992 by muller     *)
(*      modified on Tue Apr  7 14:17:09 PDT 1992 by sclafani   *)
(*      modified on Wed Nov 20 23:13:53 PST 1991 by mhb        *)
(* Created by Marc H. Brown before Mar 1988                    *)

INTERFACE Cube;

IMPORT VBT;

TYPE
  T <: REFANY;

PROCEDURE New (vbt: VBT.T): T;
  (* Create a new cube. *)

PROCEDURE SetStyle (cube: T; style: INTEGER);
  (* Set the viewing style.  Doesn't change any matrices. *)

PROCEDURE SetSpin (cube: T; degree: INTEGER);
  (* Set the number of degrees to spin the cube at each step.  /degree/
     should be a factor of 360.  Re-orients the cube to its initial
     position. *)

PROCEDURE SetView (cube: T; mu, theta, phi: REAL);
  (* Set the viewing parameters. *)

PROCEDURE SetProjection (cube: T; persp: BOOLEAN; d: REAL);
  (* Set the viewing projection.  If perspective, then d is the distance to
     the screen. *)

PROCEDURE SetImage (cube: T; dblBuffer: BOOLEAN; w: REAL);
  (* Viewing screen is (-w,-w) to (w,w) and it will go to VBT.Domain.  This
     should be called whenever /w/, VBT.Domain, or the double-buffering
     option is changed. *)

PROCEDURE Advance (cube: T);
  (* Rotate the cube one notch.  If the cube has made a complete revolution,
     use the initial position of the cube to avoid accumulation of rounding
     errors. *)

PROCEDURE Display (cube: T);
  (* Draw the current cube.  This is the only routine that actually does
     the transformation of the cube. *)

END Cube.
