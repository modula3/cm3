(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified On Mon Dec 21 18:36:11 PST 1992 by meehan                   *)
(*      modified On Tue Jun 16 13:12:47 PDT 1992 by muller                   *)
(*      modified On Tue Sep 15 01:28:26 1987 by jdd                          *)
<* PRAGMA LL *>

INTERFACE VT;

IMPORT MText, Rd, Thread, VTDef;

PROCEDURE New (mtext: MText.T): VTDef.T RAISES {VTDef.Error};

PROCEDURE Replace (vt: VTDef.T; begin, end: CARDINAL; text: TEXT)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted};

PROCEDURE ReplaceChars (         vt        : VTDef.T;
                                 begin, end: CARDINAL;
                        READONLY str       : ARRAY OF CHAR)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted};

PROCEDURE ReplaceFile (vt        : VTDef.T;
                       begin, end: CARDINAL;
                       file      : Rd.T;
                       start     : CARDINAL   := 0;
                       numChars  : CARDINAL   := LAST (CARDINAL))
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted};

PROCEDURE Close (vt: VTDef.T); <* LL = vt.mutex *>

PROCEDURE Invalidate (vt: VTDef.T; b, e, l: CARDINAL)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted};

END VT.
