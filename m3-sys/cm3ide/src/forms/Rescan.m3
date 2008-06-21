(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

MODULE Rescan;

IMPORT Thread, Wr;
IMPORT BrowserDB, Form, HTML, Node, Wx;

PROCEDURE Init () =
  BEGIN
    Form.Register ("rescan", DoRescan);
  END Init;

PROCEDURE DoRescan (self: Node.T;  <*UNUSED*>data: Node.FormData;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    HTML.BeginXX (self, wx, "CM3-IDE: Package scan");
    wx.flush ();
    BrowserDB.Refresh (wx);
    HTML.End (wx);
  END DoRescan;

BEGIN
END Rescan.
