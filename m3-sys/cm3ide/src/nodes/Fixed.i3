(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

INTERFACE Fixed;

IMPORT Node;

TYPE T <: Node.T;

PROCEDURE Find (name: TEXT): T;

PROCEDURE Init ();

END Fixed.
