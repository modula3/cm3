(* $Id$ *)

INTERFACE DebugStream;
IMPORT Wr;

TYPE
  T = RECORD
    wr            : Wr.T;
    flushAlways := TRUE;
  END;

CONST Brand = "DebugStream";

CONST Equal : PROCEDURE(READONLY a, b : T) : BOOLEAN = NIL;

END DebugStream.
