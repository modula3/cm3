(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified On Fri Nov 20 22:32:19 PST 1992 by meehan *)
(*      modified On Tue Jun 16 13:12:38 PDT 1992 by muller *)
(*      modified On Fri Mar 20 10:50:07 PST 1992 by jdd *)
(*      modified On Tue May 15 17:04:57 PDT 1990 by mcjones *)

MODULE VTRd;

IMPORT MTextRd, Rd, Thread;

PROCEDURE InitReaderIx (vt: T; index: I) RAISES {Rd.Failure, Thread.Alerted} =
  BEGIN
    IF vt.rdDirty THEN
      IF vt.rd = NIL THEN vt.rd := NEW (MTextRd.T) ELSE Rd.Close (vt.rd) END;
      EVAL vt.rd.init (vt.mtext, index);
      vt.rdDirty := FALSE;
    ELSE
      Rd.Seek (vt.rd, index);
    END;
  END InitReaderIx;

PROCEDURE Rev (vt: T) RAISES {Rd.Failure, Thread.Alerted} =
  BEGIN
    IF vt.rrd = NIL THEN vt.rrd := NEW (MTextRd.T) ELSE Rd.Close (vt.rrd) END;
    EVAL vt.rrd.init (vt.mtext, Rd.Index (vt.rd), reverse := TRUE)
  END Rev;

BEGIN END VTRd.

