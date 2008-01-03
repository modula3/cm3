(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sun Sep 25 18:54:35 PDT 1994 by heydon  *)
(*      modified on Mon Jun  6 04:18:06 PDT 1994 by msm     *)
<*PRAGMA LL*>

INTERFACE STypeMap;

IMPORT ScreenType, ScrnPixmap;

CONST Brand = "STypeMap";

TYPE T = RECORD st: ScreenType.T; pm: ScrnPixmap.T END;

END STypeMap.
