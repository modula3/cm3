(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Dec  8 10:46:37 PST 1994 by kalsow                   *)

MODULE CMarkUp;

IMPORT Buf, Wx;

PROCEDURE Annotate (buf: Buf.T;  wx: Wx.T) =
  BEGIN
    Wx.PutText (wx, "<PRE>\n");
    Wx.PutStr  (wx, buf^);
    Wx.PutText (wx, "/n</PRE>\n");
  END Annotate;

BEGIN
END CMarkUp.
