(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

INTERFACE ClassDir;

IMPORT Node;

TYPE
  T <: Tx;  Tx = Node.Named_T OBJECT
    kind: Node.Class := Node.Class.Unknown;
  END;

PROCEDURE Init ();

END ClassDir.
