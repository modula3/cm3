(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

INTERFACE FileDir;

IMPORT Node;

TYPE
  T <: Tx;  Tx = Node.Named_T OBJECT
    path : TEXT;
  END;

END FileDir.
