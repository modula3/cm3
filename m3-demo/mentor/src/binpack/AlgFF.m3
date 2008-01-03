(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
<* PRAGMA LL *>

MODULE AlgFF;

IMPORT Algorithm, BinpackAlgClass, BinpackIE, FormsVBT, IntList,
       Random, RealList, Thread, VBT, ZeusPanel;

<* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>

TYPE
  T =
    BinpackAlgClass.T BRANDED OBJECT
      amts: REF ARRAY OF REAL;       (* amts[i] is the val of i'th *)
      locs  : REF ARRAY OF INTEGER;  (* bin locs[i] holds i'th *)
      totals: REF ARRAY OF REAL;     (* b'th bin has totals[b] *)
      wts: REF ARRAY OF IntList.T;   (* wts[b] is wts in b'th bin *)
    OVERRIDES
      <* LL=0 *>
      run                 := Run;
      <* LL=VBT.mu *>
      reactivity          := Reactivity;
      feTryToDeleteWeight := TryToDeleteWeight;
      feTryToEmptyBin     := TryToEmptyBin;
    END;

CONST
   NotInPacking = -1;

PROCEDURE New (): Algorithm.T =
  VAR
    fv  := ZeusPanel.NewForm("binpackinput.fv");
    alg := NEW(T, data := fv).init();
  BEGIN
    FormsVBT.AttachProc(fv, "toDelete", ToDelete, alg);
    FormsVBT.AttachProc(fv, "toEmpty", ToEmpty, alg);
    RETURN alg
  END New;

PROCEDURE Run (alg: T) RAISES {Thread.Alerted} =
  VAR
    B  : INTEGER;                (* number of bins *)
    N  : INTEGER;                (* number of weights *)
    bin: INTEGER;                (* index into array of bins *)
  BEGIN
    WITH amts   = alg.amts,
         locs   = alg.locs,
         totals = alg.totals,
         wts    = alg.wts,
         rand   = NEW(Random.Default).init() DO
      LOCK VBT.mu DO
        N := FormsVBT.GetInteger(alg.data, "N");
        B := FormsVBT.GetInteger(alg.data, "B");
      END;
      BinpackIE.Setup(alg, B, N);
      amts := NEW(REF ARRAY OF REAL, N);
      locs := NEW(REF ARRAY OF INTEGER, N);
      totals := NEW(REF ARRAY OF REAL, B);
      wts := NEW(REF ARRAY OF IntList.T, B);
      FOR b := 0 TO B - 1 DO totals[b] := 0.0; wts[b] := NIL END;
      FOR w := 0 TO N - 1 DO locs[w] := NotInPacking END;
      FOR w := 0 TO N - 1 DO
        amts[w] := rand.real() / 2.0;
        BinpackIE.NewWeight(alg, w, amts[w]);
        bin := 0;
        BinpackIE.Probe(alg, bin, totals[bin]);
        WHILE (bin < B) AND (totals[bin] + amts[w] > 1.0) DO
          INC(bin);
          IF bin < B THEN
            BinpackIE.Probe(alg, bin, totals[bin])
          END;
        END;
        IF bin = B THEN
          BinpackIE.Ignore(alg);
        ELSE
          totals[bin] := totals[bin] + amts[w];
          locs[w] := bin;
          wts[bin] :=
            IntList.AppendD(wts[bin], IntList.List1(w));
          BinpackIE.Pack(alg, bin, totals[bin])
        END
      END
    END
  END Run;

PROCEDURE TryToEmptyBin (alg: T; bin: INTEGER) =
  <* LL = VBT.mu *>
  <* FATAL Thread.Alerted *>
  VAR w, old: IntList.T;
  BEGIN
    IF bin < 0 OR bin > LAST(alg.wts^) THEN RETURN END;
    old := alg.wts[bin];
    w := alg.wts[bin];
    WHILE w # NIL DO
      alg.locs[w.head] := NotInPacking;
      w := w.tail
    END;
    alg.wts[bin] := NIL;
    alg.totals[bin] := 0.0;
    BinpackIE.RepackBin(alg, bin, old, NIL, NIL);
  END TryToEmptyBin;

PROCEDURE TryToDeleteWeight (alg: T; id: INTEGER) =
  <* LL = VBT.mu *>
  <* FATAL Thread.Alerted *>
  VAR
    w, old, new: IntList.T;
    amts       : RealList.T;
    bin        : INTEGER;
  BEGIN
    IF id < 0 OR id > LAST(alg.locs^) THEN RETURN END;
    bin := alg.locs[id];
    IF bin = NotInPacking THEN RETURN END;
    old := alg.wts[bin];
    IntListDelete(alg.wts[bin], id);
    alg.totals[bin] := alg.totals[bin] - alg.amts[id];
    alg.locs[id] := NotInPacking;
    w := alg.wts[bin];
    WHILE w # NIL DO
      new := IntList.Cons(w.head, new);
      amts := RealList.Cons(alg.amts[w.head], amts);
      w := w.tail
    END;
    BinpackIE.RepackBin(
      alg, bin, old, IntList.ReverseD(new), RealList.ReverseD(amts));
  END TryToDeleteWeight;

PROCEDURE IntListDelete (VAR list: IntList.T; item: INTEGER) =
  VAR l, t: IntList.T;
  BEGIN
    LOOP
      IF list = NIL THEN
        RETURN
      ELSIF list.head = item THEN
        list := list.tail
      ELSE
        l := list;
        LOOP
          t := l.tail;
          IF t = NIL THEN
            RETURN
          ELSIF t.head = item THEN
            l.tail := t.tail
          ELSE
            l := t
          END
        END
      END
    END
  END IntListDelete;


PROCEDURE ToDelete (             fv   : FormsVBT.T;
                    <* UNUSED *> event: TEXT;
                                 cl   : REFANY;
                    <* UNUSED *> time : VBT.TimeStamp) =
  <* LL=VBT.mu *>
  BEGIN
    WITH alg = NARROW(cl, T),
         id  = FormsVBT.GetInteger(fv, "toDelete") DO
      TryToDeleteWeight(alg, id)
    END
  END ToDelete;

PROCEDURE ToEmpty (             fv   : FormsVBT.T;
                   <* UNUSED *> event: TEXT;
                                cl   : REFANY;
                   <* UNUSED *> time : VBT.TimeStamp) =
  <* LL=VBT.mu *>
  BEGIN
    WITH alg = NARROW(cl, T),
         bin = FormsVBT.GetInteger(fv, "toEmpty") DO
      TryToEmptyBin(alg, bin)
    END
  END ToEmpty;

PROCEDURE Reactivity (alg: T; enable: BOOLEAN) =
  <* LL=VBT.mu *>
  BEGIN
    IF enable THEN
      FormsVBT.MakeActive(alg.data, "runtimeOpts")
    ELSE
      FormsVBT.MakePassive(alg.data, "runtimeOpts");
    END
  END Reactivity;

BEGIN
  ZeusPanel.RegisterAlg(New, "First-Fit", "Binpack");
END AlgFF.

