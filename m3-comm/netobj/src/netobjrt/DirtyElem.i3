(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(* Last modified on Sun Sep 25 18:46:26 PDT 1994 by heydon     *)
(*      modified on Tue Apr 13 18:08:55 PDT 1993 by wobber     *)
(*      modified on Wed Sep  2 16:55:03 PDT 1992 by evers      *)

INTERFACE DirtyElem;

IMPORT StubLib;

CONST Brand = "DirtyElem";

TYPE
  T = RECORD 
    dirty: BOOLEAN;
    keep:  BOOLEAN;
    ts: ARRAY [0..1] OF StubLib.Int32; (* lsw..msw *)
         (* really a SpecialObj.EventID *)
  END;

END DirtyElem.
