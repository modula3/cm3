(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

INTERFACE Dir;

IMPORT Thread, Wr;
IMPORT Node, OS, Wx;

TYPE
  T <: Tx;  Tx = Node.Named_T OBJECT
    scanned  : OS.FileTime  := OS.NO_TIME;
    contents : Node.Named_T := NIL;
  END;

PROCEDURE GenContents (t: T;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted};

PROCEDURE GenReadmeInfo (path: TEXT;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted};

PROCEDURE Init ();

END Dir.
