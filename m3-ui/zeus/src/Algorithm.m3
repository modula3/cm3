(* Copyright 1992 Digital Equipment Corporation.           *)
(* Distributed only by permission.                         *)
(* Last modified on Tue Jan 31 13:29:36 PST 1995 by kalsow *)
(*      modified on Sat Feb  6 01:04:25 PST 1993 by johnh  *)
(*      modified on Wed May 13 07:34:39 1992 by mhb        *)


MODULE Algorithm;

IMPORT AlgorithmClass, FormsVBT, Rd, Wr, ZeusClass, ZeusUtil;
<* PRAGMA LL *>

REVEAL
  T = AlgorithmClass.T BRANDED OBJECT
      OVERRIDES
        run               := DefaultRun;
        init              := DefaultInit;
        snapshot          := DefaultSnapshot;
        restore           := DefaultRestore;
        updateEventCounts := DefaultUpdateCts;
      END;


EXCEPTION FatalError;
<*FATAL FatalError*>

PROCEDURE DefaultRun (<*UNUSED*> v: T) = 
<* LL = {} *>
  BEGIN
    RAISE FatalError
  END DefaultRun;

PROCEDURE DefaultInit (v: T): T =
<* LL = VBT.mu *>
  BEGIN
    v.evtMu := NEW(MUTEX);
    v.reactivity(FALSE);
    RETURN v
  END DefaultInit;

PROCEDURE DefaultSnapshot (v: T; wr: Wr.T) RAISES {ZeusClass.Error} =
  <* LL = VBT.mu *>
  BEGIN
    TRY
      Wr.PutText(wr, "(")
    EXCEPT
    ELSE
      RAISE ZeusClass.Error("Algorithm.DefaultSnapshot write error")
    END;
    IF v.data # NIL THEN
      TYPECASE v.data OF
      | FormsVBT.T (fv) =>
          TRY
            fv.snapshot(wr);
          EXCEPT
            FormsVBT.Error (msg) =>
              RAISE ZeusClass.Error(
                      "Algorithm.DefaultSnapshot FV error: " & msg);
          ELSE
            RAISE ZeusClass.Error("Algorithm.DefaultSnapshot error");
          END;
      ELSE
      END;
    END;
    ZeusClass.T.snapshot(v, wr);
    TRY
      Wr.PutText(wr, ")")
    EXCEPT
    ELSE
      RAISE ZeusClass.Error("Algorithm.DefaultSnapshot write error")
    END;
  END DefaultSnapshot;


PROCEDURE DefaultRestore (v: T; rd: Rd.T) RAISES {ZeusClass.Error} =
<* LL = VBT.mu *>
  BEGIN
    IF rd = NIL THEN RETURN END;
    IF NOT ZeusUtil.EatChar(rd, '(') THEN
      RAISE ZeusClass.Error("Algorithm.DefaultRestore read error")
    END;
    IF v.data # NIL THEN
      TRY
        TYPECASE v.data OF
        | FormsVBT.T (fv) =>
            fv.restore(rd);
        ELSE
        END;
      EXCEPT
      ELSE
        RAISE ZeusClass.Error("Algorithm.DefaultRestore error");
      END;
    END;
    ZeusClass.T.restore(v, rd);
    IF NOT ZeusUtil.EatChar(rd, ')') THEN
      RAISE ZeusClass.Error("Algorithm.DefaultRestore read error")
    END;
  END DefaultRestore;

PROCEDURE DefaultUpdateCts (<*UNUSED*> v: T; <*UNUSED*> reset: BOOLEAN) =
  <* LL = VBT.mu *>
  BEGIN
  END DefaultUpdateCts;
  
BEGIN END Algorithm.

