(* Copyright (C) 1993, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*| Last modified on Wed Jun  9 09:35:13 PDT 1993 by kalsow  *)
(*|      modified on Wed Jun  2 15:00:17 PDT 1993 by muller  *)
(*|      modified on Wed Apr 21 13:14:37 PDT 1993 by mcjones *)
(*|      modified on Wed Mar 10 11:01:47 PST 1993 by mjordan *)
(*|      modified on Tue Mar  9 08:45:18 PST 1993 by jdd     *)

UNSAFE MODULE RTHeap;

IMPORT RTType, RTMisc;

(* If "r" is a traced reference, GetDataAdr returns the address of "r^"'s
   data bytes.  If "r" is a traced object, GetDataAdr returns the address
   of "r"'s data record's bytes.  It is a checked runtime error if "r" is
   "NIL".  Note that the address can subsequently change unless object
   mobility is disabled using "RTCollector". *)

PROCEDURE GetDataAdr (r: REFANY): ADDRESS =
  VAR def := RTType.Get(TYPECODE(r));
  BEGIN
    IF r = NIL THEN
      Die (); <*ASSERT FALSE*>
    ELSIF def.defaultMethods # NIL THEN
      RETURN LOOPHOLE(r, ADDRESS) + ADRSIZE(ADDRESS);
    ELSIF def.nDimensions # 0 THEN
      RETURN LOOPHOLE(r, UNTRACED REF ADDRESS)^;
    ELSE
      RETURN LOOPHOLE(r, ADDRESS);
    END;
  END GetDataAdr;

(* If "r" is a traced reference, GetDataSize returns the number of "r^"'s
   data bytes.  If "r" is a traced object, GetDataSize returns the number
   of "r"'s data record's bytes.  It is a checked runtime error if "r" is
   "NIL". *)

PROCEDURE GetDataSize (r: REFANY): CARDINAL =
  VAR
    def := RTType.Get(TYPECODE(r));
    sizes: UNTRACED REF INTEGER;
    n: INTEGER;
  BEGIN
    IF r = NIL THEN
      Die (); <*ASSERT FALSE*>
    ELSIF def.defaultMethods # NIL THEN
      RETURN def.dataSize - BYTESIZE(ADDRESS);
    ELSIF def.nDimensions = 0 THEN
      RETURN def.dataSize;
    ELSE (* an open array *)
      n := 1;
      sizes := LOOPHOLE(r, ADDRESS) + ADRSIZE(ADDRESS);
      FOR i := 0 TO def.nDimensions - 1 DO
        n := n * sizes^;
        INC(sizes, ADRSIZE(INTEGER));
      END;
      RETURN n * def.elementSize;
    END;
  END GetDataSize;

(* If "r" is a traced reference to an open array, GetArrayShape returns in
   "s[0 ..  n-1]" the size of each dimension of the n-dimensional open
   array "r^".  If "s" is too large, the extra elements are ignored; if
   it's too small, the extra sizes are discarded.  It is a checked runtime
   error if "r" is "NIL".  If "r" is not a reference to an open array, "s"
   is unchanged. *)

PROCEDURE GetArrayShape (r: REFANY; VAR s: ARRAY OF INTEGER) =
  VAR
    def := RTType.Get(TYPECODE(r));
    sizes: UNTRACED REF INTEGER := LOOPHOLE(r, ADDRESS) + ADRSIZE(ADDRESS);
  BEGIN
    FOR i := 0 TO MIN(NUMBER(s), def.nDimensions) - 1 DO
      s[i] := sizes^;
      INC(sizes, ADRSIZE(sizes^));
    END;
  END GetArrayShape;

PROCEDURE Die () =
  BEGIN
    RTMisc.FatalError (NIL, 0, "NIL ref passed to RTHeap.GetData");
  END Die;

BEGIN
END RTHeap.
