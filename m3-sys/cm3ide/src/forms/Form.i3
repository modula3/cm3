(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

INTERFACE Form;

IMPORT Thread, Wr;
IMPORT Node, Wx;

TYPE Handler = PROCEDURE (n: Node.T;  data: Node.FormData;  wx: Wx.T)
                 RAISES {Wr.Failure, Thread.Alerted};

PROCEDURE Register (name: TEXT;  handler: Handler);

PROCEDURE Init ();

END Form.
