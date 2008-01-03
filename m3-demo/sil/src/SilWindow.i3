(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Tue Nov  8 09:41:22 PST 1994 by kalsow    *)

INTERFACE SilWindow;

IMPORT WinDef, SilRd;

TYPE
  T <: Tx;
  Tx = OBJECT
    dc: WinDef.HDC;
  METHODS
    init (title: TEXT): T;
    run ();
  END;

TYPE ReadProc = PROCEDURE (rd: SilRd.T): REFANY;

PROCEDURE RegisterReader (min, max: CHAR;  r: ReadProc);

END SilWindow.


