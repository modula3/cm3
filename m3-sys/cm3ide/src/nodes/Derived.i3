(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

INTERFACE Derived;

IMPORT Node, OS, Loc, ID, Source;

TYPE
  T <: Tx; Tx = Node.Named_T OBJECT
    scanned   : OS.FileTime := OS.NO_TIME;
    is_pgm    : BOOLEAN     := FALSE;  (* otherwise LIB *)
    n_elts    : INTEGER     := 0;
    contents  : NodeRefSet  := NIL;
    seen      : ARRAY Source.Kind OF BOOLEAN;
  END;

  NodeRefSet = REF ARRAY OF NodeRef;

  NodeRef = RECORD
    loc  : Loc.T;
    file : ID.T;
  END;

PROCEDURE FixName (t: T);

PROCEDURE PgmPath (t: T): TEXT;

PROCEDURE Init ();

END Derived.
