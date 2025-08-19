(* $Id: DebugStream.i3,v 1.1 2009/05/24 22:19:29 mika Exp $ *)

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
