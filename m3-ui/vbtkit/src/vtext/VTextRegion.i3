(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified On Sun Mar 21 16:29:17 PST 1993 by meehan *)
(*      modified On Tue Jun 16 13:12:31 PDT 1992 by muller *)
(*      modified On Fri Mar 20 10:25:07 PST 1992 by jdd    *)


(* This file includes the VText operations that involve Regions. These will
   change when VText supports Regions using Trestle subwindows. *)

INTERFACE VTextRegion;

IMPORT Rd, Rect, Thread;
IMPORT VTDef, VTextDef;

TYPE
  T = VTextDef.T; (* A vtext *)
  Pixels = VTDef.Pixels; (* A screen coordinate *)
  ErrorCode = VTDef.ErrorCode;
  I = VTDef.I;
  Region = VTextDef.Region;


EXCEPTION
  Error (ErrorCode)(*! = VTDef.Error !*);


PROCEDURE SplitRegion (vtext : T;
                       r     : Region;
                       v     : Pixels;
                       scroll: BOOLEAN  := TRUE)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};

PROCEDURE MergeRegion (vtext: T; i, j: Region; scroll: BOOLEAN := TRUE)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};

PROCEDURE Move (         vtext             : T;
                READONLY newRect, savedRect: Rect.T;
                READONLY dividers          : ARRAY OF Pixels;
                         scroll            : BOOLEAN         )
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};

PROCEDURE SetupRegion
  (vtext: T; r: Region; north: Pixels; height: CARDINAL; startIndex: I)
   RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted};

PROCEDURE Bad (vtext: T; READONLY where: Rect.T) RAISES {};

PROCEDURE UpdateDividers (vtext: T) RAISES {};

END VTextRegion.
