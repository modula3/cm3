(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sat Feb 27 00:17:41 PST 1993 by msm      *)
<*PRAGMA LL*>

INTERFACE ShTrestle;

IMPORT Filter, VBT;

(* A "ShTrestle.T" implements the shared Trestle instance above every
   VBT, supporting attachment

TYPE
  T <: Public;
  Public = Trestle.T OBJECT METHODS init (ch: VBT.T) END;

END ShTrestle.
