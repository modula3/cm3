(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(* Last modified on Fri Jan 28 10:56:03 PST 1994 by detlefs   *)

<*EXTERNAL*> INTERFACE Csort;

IMPORT Ctypes;

TYPE
  CompareProc = PROCEDURE(x,y: Ctypes.void_star): Ctypes.int;

PROCEDURE qsort(base: Ctypes.void_star; nel, width: Ctypes.unsigned_int;
  p: CompareProc);

END Csort.
