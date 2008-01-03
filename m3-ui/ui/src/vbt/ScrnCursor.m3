(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Feb 24 13:58:02 PST 1992 by muller   *)
(*      modified on Fri Sep 13  1:42:21 PDT 1991 by msm      *)
(*      modified on Thu Apr  4 23:33:28 PST 1991 by gnelson  *)
(*      modified on Thu Apr 12 16:45:18 PDT 1990 by steveg   *)
<*PRAGMA LL*>

MODULE ScrnCursor;

REVEAL 
  T = Public BRANDED OBJECT END;
  Private = BRANDED OBJECT END;

BEGIN DontCare := NEW(T) END ScrnCursor.

