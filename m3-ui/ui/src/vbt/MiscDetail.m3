(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Oct  2 11:39:28 PDT 1992 by msm     *)
(*      modified on Mon Feb 24 13:57:19 PST 1992 by muller  *)
(*      modified on Fri Sep  6 17:25:31 PDT 1991 by gnelson *)
<*PRAGMA LL*>

MODULE MiscDetail;

VAR mu := NEW(MUTEX);
    tbl := NEW(REF ARRAY OF REFANY, 0);

PROCEDURE FromRef (ra: REFANY): INTEGER =
  BEGIN
    IF ra = NIL THEN RETURN -1 END;
    LOCK mu DO
      FOR i := 0 TO LAST(tbl^) DO
        IF tbl[i] = NIL THEN tbl[i] := ra; RETURN i END
      END;
      VAR
        n   := NUMBER(tbl^);
        new := NEW(REF ARRAY OF REFANY, MAX(4, 2 * n));
      BEGIN
        SUBARRAY(new^, 0, n) := tbl^;
        FOR i := n TO LAST(new^) DO new[i] := NIL END;
        new[n] := ra;
        tbl := new;
        RETURN n
      END
    END
  END FromRef;

PROCEDURE ToRef (i: INTEGER): REFANY =
  BEGIN
    LOCK mu DO
      IF i < 0 OR i > LAST(tbl^) THEN RETURN NIL END;
      RETURN tbl[i]
    END
  END ToRef;

PROCEDURE Delete(i: INTEGER) =
  BEGIN
    LOCK mu DO
      IF i < 0 OR i > LAST(tbl^) THEN RETURN END;
      tbl[i] := NIL
    END
  END Delete;

BEGIN
END MiscDetail.


