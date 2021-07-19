(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

INTERFACE Source;

IMPORT Thread, Wr;
IMPORT ID, Node, Wx;

TYPE
  T <: Tx; Tx = Node.Named_T OBJECT
    kind: Kind;
  END;

TYPE
  Kind = { I3, M3, IG, MG, C, CPP, H, Quake, Other };

CONST
  Ext    = ARRAY Kind OF TEXT { ".i3", ".m3", ".ig", ".mg", ".c", ".cpp", ".h", NIL, NIL };
  ExtLen = ARRAY Kind OF INTEGER { 3, 3, 3, 3, 2, 4, 2, 0, 0 };

CONST
  NodeClass = ARRAY Kind OF Node.Class {
    Node.Class.Interface, Node.Class.Module, Node.Class.GenericInterface,
    Node.Class.GenericModule, Node.Class.CSource, Node.Class.CSource, Node.Class.HSource,
    Node.Class.QuakeSource, Node.Class.MiscSource };

PROCEDURE EmitPage (t: T;  wx: Wx.T;  action: ID.T;
                    data: Node.FormData;  target_decl: TEXT)
                    RAISES {Wr.Failure, Thread.Alerted};

PROCEDURE Init ();

END Source.
