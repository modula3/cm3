(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

INTERFACE Pkg;

IMPORT Thread, Wr;
IMPORT Dir, Node, Wx;

TYPE
  T <: Tx; Tx = Dir.T OBJECT
  END;

PROCEDURE IsBuildable (t: T): BOOLEAN;

PROCEDURE GenFileNote (path: TEXT;  wx: Wx.T;  is_dir: BOOLEAN)
  RAISES {Wr.Failure, Thread.Alerted};

PROCEDURE GenBuildNote (src: Node.Named_T;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted};

PROCEDURE GenActionButtons (src: Node.Named_T;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted};

PROCEDURE Home (nd: Node.T): T;
(* Returns the package node containing "nd", otherwise "NIL". *)

PROCEDURE Rescan (t: T): T
  RAISES {Thread.Alerted};
(* Rescan package "t" and return the corresponding new node. *)

PROCEDURE Init ();

END Pkg.
