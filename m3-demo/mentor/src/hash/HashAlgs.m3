(* Copyright 1992 Digital Equipment Corporation.             *)
(* Distributed only by permission.                           *)
(* Last modified on Tue Jan 31 14:24:33 PST 1995 by kalsow   *)
(*      modified on Thu Jan  5 21:10:57 PST 1995 by najork   *)
(*      modified on Wed Mar 10 12:15:23 PST 1993 by mhb      *)
(*      modified on Wed Sep  9 10:13:01 PDT 1992 by swart    *)
(*      modified on Mon Jul 27  2:44:37 PDT 1992 by sclafani *)

MODULE HashAlgs;

IMPORT Algorithm, Fmt, FormsVBT, HashAlgClass, HashIE, IntList, Random, 
       Text, TextList, Thread, VBT, ZeusCodeView, ZeusPanel;

<* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>


TYPE
  HashAlgType = {Double, Linear, LinearSmartDelete};
  Buckets = REF ARRAY OF TEXT;
  T = HashAlgClass.T BRANDED OBJECT
        type: HashAlgType;
        buckets: Buckets;           (* array of bins *)
      OVERRIDES
        run := Run;
        feReportFind := ReportFind;
        feStopReportFind := StopReportFind;
      END;

CONST Multiplier = -1664117991; 
              (* = LOOPHOLE( Round( .6125423371 * 2^32 ), INTEGER ) *)

PROCEDURE TextHash(t: TEXT): INTEGER =
  VAR hash: INTEGER := 0;
  BEGIN
    FOR i := 0 TO Text.Length(t) - 1 DO
      hash := hash * Multiplier + ORD(Text.GetChar(t, i));
    END;
    RETURN hash;
  END TextHash;


TYPE
  OpType = {Insert, Delete, Find};
  Data = REF ARRAY OF
               RECORD
                 operation: OpType;
                 item     : TEXT;
               END;
 
EXCEPTION Fatal;

PROCEDURE GetData (alg: T): Data =

  PROCEDURE Delete (l : TextList.T; t : TEXT) : TextList.T =
    <* FATAL Fatal *>
    BEGIN
      IF l = NIL THEN
        RAISE Fatal;
      ELSIF Text.Equal (l.head, t) THEN
        RETURN l.tail;
      ELSE
        RETURN TextList.Cons (l.head, Delete (l.tail, t));
      END;
    END Delete;

  VAR
    Operations     := FormsVBT.GetInteger(alg.data, "Ops");
    Inserts        := FormsVBT.GetInteger(alg.data, "Inserts");
    Deletes        := FormsVBT.GetInteger(alg.data, "Deletes");
    NBuckets       := FormsVBT.GetInteger(alg.data, "Buckets");
    SuccFinds      := FormsVBT.GetInteger(alg.data, "SuccFinds");
    InitialInserts := FormsVBT.GetInteger(alg.data, "InitInserts");
    InitialDeletes := FormsVBT.GetInteger(alg.data, "InitDeletes");
    RandomElements := FormsVBT.GetBoolean(alg.data, "Random");
    fixedrand      := FormsVBT.GetBoolean(alg.data, "FixedRandom");
    r              := NEW (Random.Default).init (fixedrand);
    data           := NEW(Data, Operations + InitialInserts + InitialDeletes);
    l              : TextList.T := NIL;
  BEGIN
    FOR i := 0 TO InitialInserts - 1 DO
      data[i].operation := OpType.Insert;
      IF RandomElements THEN
        data[i].item := Fmt.Int (r.integer (0, LAST(CARDINAL)));
      ELSE
        data[i].item := Fmt.Int(i);
      END;
      l := TextList.Cons (data[i].item, l);
    END;
    FOR i := InitialInserts TO InitialInserts + InitialDeletes - 1 DO
      data[i].operation := OpType.Delete;
      WITH elt = r.integer (0, TextList.Length(l) - 1) DO
        data[i].item := TextList.Nth (l, elt);
        l := Delete (l, data[i].item);
      END;
    END;
    FOR i := InitialInserts + InitialDeletes TO LAST(data^) DO
      WITH value = r.integer (0, 99) DO
        IF (value < Inserts) AND (TextList.Length(l) < NBuckets) THEN
          data[i].operation := OpType.Insert;
          IF RandomElements THEN
            data[i].item :=
              Fmt.Int(r.integer (0, LAST(CARDINAL)));
          ELSE
            data[i].item := Fmt.Int(i);
          END;
          l := TextList.Cons (data[i].item, l);
        ELSIF (value < Inserts + Deletes) AND (TextList.Length(l) > 0) THEN
          data[i].operation := OpType.Delete;
          WITH elt = r.integer (0, TextList.Length(l) - 1) DO
            data[i].item := TextList.Nth (l, elt);
            l := Delete(l, data[i].item);
          END;
        ELSIF value < Inserts + Deletes + SuccFinds AND 
              TextList.Length(l) > 0 THEN
          data[i].operation := OpType.Find;
          WITH elt = r.integer (0, TextList.Length(l) - 1) DO
            data[i].item := TextList.Nth(l, elt);
          END;
        ELSE
          data[i].operation := OpType.Find;
          data[i].item := Fmt.Int(i);
        END;
      END;
    END;
    RETURN data;
  END GetData;

PROCEDURE ReportFind (alg: T; item: TEXT) =
  <*FATAL Thread.Alerted*>
  VAR
    hash2             : INTEGER;
    NBuckets          := NUMBER(alg.buckets^);
    bucket            := TextHash(item) MOD NBuckets;
    l                 : IntList.T  := NIL;
    count             := NBuckets;
  BEGIN
    bucket := TextHash(item) MOD NBuckets;
    IF (alg.type # HashAlgType.Double) OR (bucket = 0) THEN
      hash2 := 1;
    ELSE
      hash2 := NBuckets - bucket;
    END;
    LOOP
      l := IntList.Cons (bucket, l);
      DEC(count);
      IF (alg.buckets[bucket] = NIL)
           OR Text.Equal(item, alg.buckets[bucket]) OR (count = 0) THEN
        EXIT;
      END;
      bucket := (bucket - hash2) MOD NBuckets;
    END;
    HashIE.FindReport(alg, l);
  END ReportFind;

PROCEDURE StopReportFind (alg: T) =
  <*FATAL Thread.Alerted *>
  BEGIN
    HashIE.StopFindReport(alg);
  END StopReportFind;

PROCEDURE Run (alg: T) RAISES {Thread.Alerted} =
  PROCEDURE At (line: INTEGER) RAISES {Thread.Alerted} =
    BEGIN
      ZeusCodeView.Event(alg, line);
    END At;
  VAR
    NBuckets: INTEGER <* TRACE alg.varView.setIntegerL *>;
    (* number of bins *)
    pauseOnDeletes: BOOLEAN;
    item: TEXT <* TRACE alg.varView.setText *>;
    (* current to try packing; between [0..1] *)
    bucket: INTEGER <* TRACE alg.varView.setInteger *>;
    (* index into hash table *)
    hash2  : INTEGER;
    data   : Data;
  BEGIN
    LOCK VBT.mu DO
      data := GetData(alg);
      NBuckets := FormsVBT.GetInteger(alg.data, "Buckets");
    END;
    HashIE.Setup(alg, alg.data, NBuckets);
    alg.buckets := NEW(Buckets, NBuckets);
    FOR bucket := 0 TO NBuckets - 1 DO alg.buckets[bucket] := NIL END;
    FOR i := 0 TO LAST(data^) DO
      item := data[i].item;
      IF data[i].operation = OpType.Insert THEN
        ZeusCodeView.Event(alg, procedureName := "insert");
        At(1);
        HashIE.Insert(alg, item);
        At(2);
        bucket := TextHash(item) MOD NBuckets;
        IF (alg.type # HashAlgType.Double) OR (bucket = 0) THEN
          hash2 := 1;
        ELSE
          hash2 := NBuckets - bucket;
        END;
        LOOP
          HashIE.Compare(alg, bucket);
          At(4);
          IF (alg.buckets[bucket] = NIL) OR Text.Empty(alg.buckets[bucket]) THEN
            At(5);
            HashIE.AddToBucket(alg, item, bucket);
            alg.buckets[bucket] := item;
            At(6);
            EXIT;
          END;
          At(3);
          IF Text.Equal(item, alg.buckets[bucket]) THEN At(8); EXIT; END;
          At(7);
          bucket := (bucket - hash2) MOD NBuckets;
        END;
      ELSIF data[i].operation = OpType.Find THEN
        ZeusCodeView.Event(alg, procedureName := "find");
        At(1);
        HashIE.Find(alg, item);
        At(2);
        bucket := TextHash(item) MOD NBuckets;
        IF (alg.type # HashAlgType.Double) OR (bucket = 0) THEN
          hash2 := 1;
        ELSE
          hash2 := NBuckets - bucket;
        END;
        VAR count := NBuckets;
        BEGIN
          LOOP
            HashIE.Compare(alg, bucket);
            At(4);
            DEC(count);
            IF (alg.buckets[bucket] = NIL) OR Text.Equal(item, alg.buckets[bucket])
                 OR (count = 0) THEN
              EXIT;
            END;
            bucket := (bucket - hash2) MOD NBuckets;
          END;
        END;
      ELSE
        ZeusCodeView.Event(alg, procedureName := "delete");
        HashIE.Delete(alg, item);
        LOCK VBT.mu DO
          pauseOnDeletes := FormsVBT.GetBoolean(alg.data, "PauseOnDeletes");
        END;
        IF pauseOnDeletes THEN
          ZeusPanel.Pause(alg, "Pause on Delete");
        END;
        At(10);
        bucket := TextHash(item) MOD NBuckets;
        IF (alg.type # HashAlgType.Double) OR (bucket = 0) THEN
          hash2 := 1;
        ELSE
          hash2 := NBuckets - bucket;
        END;
        LOOP
          HashIE.Compare(alg, bucket);
          IF alg.buckets[bucket] = NIL OR Text.Equal(item, alg.buckets[bucket]) THEN
            EXIT;
          END;
          bucket := (bucket - hash2) MOD NBuckets;
        END;
        IF alg.buckets[bucket] # NIL THEN
          IF alg.type = HashAlgType.LinearSmartDelete THEN
            LOOP
              IF alg.buckets[bucket] = NIL THEN EXIT; END;
              WITH saveBucket = bucket + 0 DO
                HashIE.DeleteFromBucket(
                  alg, alg.buckets[saveBucket], saveBucket, FALSE);
                alg.buckets[saveBucket] := NIL;
                LOOP
                  bucket := (bucket - 1) MOD NBuckets;
                  HashIE.CheckDeletable(alg, bucket);
                  IF alg.buckets[bucket] = NIL THEN EXIT; END;
                  WITH r = TextHash(alg.buckets[bucket]) MOD NBuckets DO
                    HashIE.CheckHashPosition(alg, r);
                    IF ((r >= saveBucket) AND (saveBucket > bucket))
                         OR ((r >= saveBucket) AND (bucket > r))
                         OR ((bucket > r) AND (saveBucket > bucket)) THEN
                      (* r cyclically precedes saveBucket, this means that
                         we can put buckets[bucket] in position saveBucket
                         and it is an improvement in access. *)
                      HashIE.AddToBucket(alg, alg.buckets[bucket], saveBucket);
                      alg.buckets[saveBucket] := alg.buckets[bucket];
                      EXIT;
                    END;
                  END;
                END;
              END;
            END;
          ELSE
            HashIE.DeleteFromBucket(alg, item, bucket, TRUE);
            alg.buckets[bucket] := "";
          END;
        END;
      END;
    END;
  END Run;

PROCEDURE NewLinear (): Algorithm.T =
  VAR fv := ZeusPanel.NewForm("hashinput.fv");
  BEGIN
    RETURN
      NEW(
        T, type := HashAlgType.Linear, data := fv,
        varRsrc := "HashVar.fv").init()
  END NewLinear;

PROCEDURE NewLinearSmartDelete (): Algorithm.T =
  VAR fv := ZeusPanel.NewForm("hashinput.fv");
  BEGIN
    RETURN
      NEW(
        T, type := HashAlgType.LinearSmartDelete, data := fv,
        varRsrc := "HashVar.fv").init()
  END NewLinearSmartDelete;

PROCEDURE NewDouble (): Algorithm.T =
  VAR fv := ZeusPanel.NewForm("hashinput.fv");
  BEGIN
    RETURN
      NEW(
        T, type := HashAlgType.Double, data := fv,
        varRsrc := "HashVar.fv").init()
  END NewDouble;

BEGIN
  ZeusPanel.RegisterAlg(
    NewLinear, "Linear Open Addressing", "Hash");
  ZeusPanel.RegisterAlg(
    NewLinearSmartDelete,
    "Linear Open Addressing w/ Smart Deletion", "Hash");
  ZeusPanel.RegisterAlg(
    NewDouble, "Double Open Addressing", "Hash");
END HashAlgs.
