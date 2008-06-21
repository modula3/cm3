(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

INTERFACE PkgRoot;

IMPORT Dir, Node;

TYPE
  T <: Tx; Tx = Dir.T OBJECT
    path      : TEXT    := NIL;
    buildable : BOOLEAN := FALSE;
    kind      : Node.Class;
  END;

PROCEDURE Add (name, path: TEXT;  buildable: BOOLEAN);

PROCEDURE First (): T;

PROCEDURE Init ();
(* Register any existing roots *)

PROCEDURE Reset ();
(* Remove and unregister any existing roots *)

END PkgRoot.
