
(* Copyright 1997 Critical Mass, Inc. All Rights Reserved.
   See file COPYRIGHT for full description. *)

INTERFACE ClockVBT;
IMPORT VBT, Font, PaintOp;
IMPORT FmtTime, Time, Date;

(* "ClockVBT" interface displays a text of the updating
   time. *)

TYPE 
  T <: Public;
  Public = Private OBJECT METHODS
    init (font: Font.T := Font.BuiltIn;
          halign : REAL := 0.5;
          bgFg: PaintOp.ColorQuad := NIL; 
          proc: FmtProc := FmtTime.Long): T;
  END;
  Private <: VBT.Leaf;

(* The call to "init" initializes the clock, similar
   to a "TextVBT", with the exception of the "proc"
   parameter, the defaults for which can be overriden
   to adhere to specialized conventions, e.g., military
   time format. *)

TYPE
  FmtProc = PROCEDURE (t: Time.T; z: Date.TimeZone := NIL): TEXT;

(* "FmtProc" is the same signature as the time display
   routines in the "FmtTime" interface. *)

  
END ClockVBT.

