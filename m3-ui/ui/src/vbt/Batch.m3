(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Oct  4 11:35:17 PDT 1993 by sfreeman *)
(*      modified on Tue Mar 10 19:02:43 1992 by steveg   *)
(*      modified on Mon Feb 24 13:56:35 PST 1992 by muller   *)
(*      modified on Wed Sep 11 15:22:03 PDT 1991 by msm      *)
<*PRAGMA LL*>

UNSAFE MODULE Batch;

IMPORT VBTTuning, BatchRep, PictureRep, Rect, BatchUtil;

VAR
  freemu := NEW(MUTEX);
  free: T;
  batchSize := VBTTuning.BatchSize;

PROCEDURE New(len: INTEGER := -1): T =
  VAR res: T;
  BEGIN
    IF len = -1 THEN len := batchSize END;
    IF len > batchSize THEN
      res := NEW(T);
      res.b := NEW(BatchRep.Array, (3 * len) DIV 2)
    ELSE
      LOCK freemu DO
        res := free;
        IF res # NIL THEN free := free.link END
      END;
      IF res = NIL THEN
        res := NEW(T);
        res.b := NEW(BatchRep.Array, batchSize)
      END
    END;
    res.link := NIL;
    res.clipped := BatchUtil.ClipState.Tight;
    res.next := ADR(res.b[0]);
    res.scrollSource := Rect.Empty;
    res.clip := Rect.Empty;
    res.containsPicture := FALSE;
    RETURN res
  END New;

PROCEDURE Free (VAR ba: T) =
  BEGIN
    IF ba.containsPicture THEN PictureRep.DecrementBatch(ba); END;
    LOCK freemu DO
      (* put ba on free list if it is standard size *)
      IF NUMBER(ba.b^) = batchSize THEN
        ba.link := free;
        free := ba
      ELSE
        DISPOSE(ba.b);
        DISPOSE(ba)
      END
    END;
    ba := NIL
  END Free;
        
BEGIN
END Batch.
