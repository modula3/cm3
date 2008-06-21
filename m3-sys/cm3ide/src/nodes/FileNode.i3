(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

INTERFACE FileNode;

IMPORT Node, Thread, Wr, Wx;

TYPE
  T <: Tx;  Tx = Node.Named_T OBJECT
    path : TEXT;
  END;

PROCEDURE EmitFile (n: Node.T;  path: TEXT;  wx: Wx.T): BOOLEAN
  RAISES {Wr.Failure, Thread.Alerted};
(* If "path" has a recognized extension, emit the file with the
   proper HTTP tags and return "TRUE".  Otherwise, return "FALSE". *)

END FileNode.
