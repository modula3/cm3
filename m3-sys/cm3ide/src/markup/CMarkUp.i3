(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Dec  8 09:54:21 PST 1994 by kalsow                   *)

INTERFACE CMarkUp;

IMPORT Buf, Marker, Wx, Wr, Thread;

PROCEDURE Annotate (buf: Buf.T;  wx: Wx.T;  ins: Marker.LineInsertion)
  RAISES {Wr.Failure, Thread.Alerted};
(* Copy the C source in "buf" to "wx" adding HTML annotations
   and inserting any text in "ins". *)

END CMarkUp.
