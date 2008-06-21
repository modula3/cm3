(* Copyright 1996, Critical Mass, Inc.   All rights reserved *)

INTERFACE Marker;

TYPE
  LineInsertion = REF RECORD
    next   : LineInsertion := NIL;
    line   : INTEGER       := 0;   (* line offset into file *)
    insert : TEXT          := NIL;
  END;

TYPE
  CharInsertion = REF RECORD
    next   : CharInsertion := NIL;
    count  : CARDINAL      := 0;
    insert : ARRAY [0..253] OF CharInsert;
  END;

  CharInsert = RECORD
    offset : INTEGER       := 0;   (* byte offset into file *)
    txt    : TEXT          := NIL;
    start  : INTEGER       := 0;
    length : INTEGER       := 0;
  END;

END Marker.
