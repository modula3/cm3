(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Sep  7 11:09:03 PDT 1994 by heydon                   *)

INTERFACE RemoteView;

IMPORT NetObj, Thread;

EXCEPTION Error(TEXT);

TYPE
  T = NetObj.T BRANDED "RemoteView 2.0" OBJECT METHODS
    startrun() RAISES {NetObj.Error, Thread.Alerted};
    endrun() RAISES {NetObj.Error, Thread.Alerted};
    event(tfactor: REAL; nm, args: TEXT)
      RAISES {Error, NetObj.Error, Thread.Alerted}
  END;

END RemoteView.
