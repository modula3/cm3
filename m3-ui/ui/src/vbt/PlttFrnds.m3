(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Fri Jan 29 16:49:11 PST 1993 by msm      *)
(*      modified on Tue Mar 10 19:05:26 1992 by steveg   *)
(*      modified on Mon Feb 24 13:57:29 PST 1992 by muller   *)
(*      modified on Tue Oct 22 22:45:40 PDT 1991 by gnelson  *)

<*PRAGMA LL*>

MODULE PlttFrnds;

IMPORT ScrnPaintOp, ScrnFont, ScrnCursor, ScrnPixmap;

BEGIN
  noOp := NEW(ScrnPaintOp.T, id := 0);
  noFont := NEW(ScrnFont.T, id := 0);
  noCursor := NEW(ScrnCursor.T, id := 0);
  noPixmap := NEW(ScrnPixmap.T, id := 0);
  con := NEW(Context)
END PlttFrnds.
