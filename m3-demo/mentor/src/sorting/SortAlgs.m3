(* Copyright 1989 Digital Equipment Corporation.             *)
(* Distributed only by permission.                           *)
(* File: SortAlgs.m3                                         *)
(* Last modified on Thu Jun 20 17:19:13 PDT 1996 by heydon   *)
(*      modified on Tue Jan 31 15:40:29 PST 1995 by kalsow   *)
(*      modified on Thu Jan  5 23:40:46 PST 1995 by najork   *)
(*      modified on Thu Sep 24 10:54:47 PDT 1992 by mhb      *)
(*      modified on Tue Sep  8 20:32:24 PDT 1992 by johnh    *)
(*      modified on Mon Jul 27  1:11:31 PDT 1992 by sclafani *)

MODULE SortAlgs;

IMPORT Algorithm, FormsVBT, RefList, Random, Sort, SortAlgClass, SortIE, 
       Thread, Word, ZeusCodeView, ZeusDataView, ZeusPanel;

(* If there's a FormsVBT error, we want to stop and fix it, not proceed. *)

<*FATAL FormsVBT.Error,FormsVBT.Unimplemented *>

REVEAL
  T = SortAlgClass.T BRANDED OBJECT
        a       : Sort.Keys;
        N       : CARDINAL;
        input   : FormsVBT.T;
        defaultN: CARDINAL;
      METHODS
        passes (): CARDINAL := DefaultPasses;
      OVERRIDES
        install     := Install;
        feChangeVal := ChangeVal;
      END;

PROCEDURE DefaultPasses(alg: T): CARDINAL =
  BEGIN 
    RETURN alg.N 
  END DefaultPasses;

TYPE InsertionSort = T OBJECT OVERRIDES run := InsertionRun; END;

TYPE SelectionSort = T OBJECT OVERRIDES run := SelectionRun; END;

TYPE BubbleSort = T OBJECT OVERRIDES run := BubbleRun; END;

TYPE ShakerSort = T OBJECT OVERRIDES run := ShakerRun; END;

TYPE
  ShellSort = T OBJECT
              OVERRIDES
                run    := ShellRun;
                passes := ShellPasses;
              END;

TYPE
  HeapSort = T OBJECT
             OVERRIDES
               passes := HeapPasses;
               run    := HeapRun;
             END;

TYPE
  RadixSort = T OBJECT
                b: Sort.Keys;
              OVERRIDES
                run    := RadixRun;
                passes := RadixPasses
              END;

TYPE BUMergeSort = T OBJECT OVERRIDES run := BUMergeRun; END;

TYPE QuickSort = T OBJECT OVERRIDES run := QuickRun; END;


(************************  Output events  ************************)

PROCEDURE SetVal (alg: T; i: INTEGER; key: Sort.Key) RAISES {Thread.Alerted} =
  BEGIN
    WITH a = alg.a DO
      a[i] := key;
      SortIE.SetVal(alg, i, a[i]);
    END
  END SetVal;

PROCEDURE SwapElts (alg: T; i, j: INTEGER) RAISES { Thread.Alerted } =
  BEGIN
    WITH a = alg.a DO
      VAR t := a[i]; BEGIN
        a[i] := a[j];
        a[j] := t;
      END;
      SortIE.SwapElts(alg, i, j);
    END
  END SwapElts;


(************************  Feedback events  ************************)

PROCEDURE ChangeVal (alg: T; i: CARDINAL; new: Sort.Key) =
  <*FATAL Thread.Alerted*>
  BEGIN
    alg.a[i] := new;
    SortIE.UpdateVal(alg, i, new)
  END ChangeVal;


(************************  GetData  ************************)

PROCEDURE GetData (alg: T) RAISES {Thread.Alerted} =
  VAR
    rand := NEW (Random.Default).init ();
  BEGIN
    WITH a = alg.a, N = alg.N, input = alg.data DO 
      N := FormsVBT.GetInteger(input, "data");
      IF FormsVBT.GetBoolean(input, "increasing") THEN
        FOR i := 1 TO N DO a[i] := i; END;
      ELSIF FormsVBT.GetBoolean(input, "decreasing") THEN
        FOR i := 1 TO N DO a[i] := N - i + 1; END;
      ELSE
        FOR i := 1 TO N DO alg.a[i] := i; END;
        FOR i := N TO 2 BY -1 DO
          VAR j := rand.integer (1, i); t := a[i]; 
          BEGIN a[i] := a[j]; a[j] := t; END;
        END;
      END;
      a[0] := 0;
      a[N + 1] := N + 1;
      SortIE.Init(alg, N, alg.passes());
      FOR i := 1 TO N DO SortIE.SetVal(alg, i, a[i]); END;
    END
  END GetData;

PROCEDURE Install (alg: T) =
  BEGIN
    FormsVBT.PutInteger(alg.data, "data", alg.defaultN);
    SortAlgClass.T.install (alg);
  END Install;


(*****************************  Insertion Sort  *****************************)

PROCEDURE InsertionNew (): Algorithm.T =
  BEGIN
    RETURN
      NEW(InsertionSort, defaultN := 32,
          data := ZeusPanel.NewForm("SortData.fv"),
          codeViews :=
            RefList.List2(
              RefList.List2("Modula-3 Code View", "Insertion.m3"),
              RefList.List2("Pascal Code View", "Insertion.pas")),
          varRsrc := "InsertionVar.fv").init();
  END InsertionNew;

PROCEDURE InsertionRun (alg: InsertionSort) RAISES {Thread.Alerted} =

  PROCEDURE At (line: INTEGER) RAISES {Thread.Alerted} = 
    BEGIN
      ZeusCodeView.Event(alg, line);
    END At;

  PROCEDURE ISort (
      VAR a: ARRAY OF Sort.Key <* TRACE alg.varView.setIntegerArray *>;
      N: INTEGER <* TRACE alg.varView.setInteger *>) RAISES {Thread.Alerted} = 
    VAR
      j: CARDINAL <* TRACE alg.varView.setInteger *>;
      v: Sort.Key <* TRACE alg.varView.setInteger *>;
    BEGIN
            FOR i <* TRACE alg.varView.setInteger *> := 2 TO N DO
At(1);        SortIE.StartPass(alg);
At(2);        v := a[i];
At(3);        j := i;
              WHILE a[j - 1] > v DO
At(4);
At(5);          a[j] := a[j - 1]; 
                SortIE.SetVal(alg, j, a[j]);
At(6);          DEC (j);
              END;
At(4);
At(7);        a[j] := v;
              SortIE.SetVal(alg, j, v);
            END
    END ISort;

  BEGIN
    GetData(alg);
    ZeusCodeView.Enter(alg, "Insertion");
    ISort(SUBARRAY(alg.a, 0, alg.N + 2), alg.N);
    (* ZeusCodeView.Exit(alg); commented out: leave code view up when done *)
  END InsertionRun;


(*****************************  Selection Sort  *****************************)

PROCEDURE SelectionNew (): Algorithm.T =
  BEGIN
    RETURN
      NEW(SelectionSort,
          defaultN := 32,
          data := ZeusPanel.NewForm("SortData.fv")).init()
  END SelectionNew;

PROCEDURE SelectionRun (alg: SelectionSort)
  RAISES {Thread.Alerted} =
  VAR mn: INTEGER;
  BEGIN
    GetData(alg);
    WITH a = alg.a, N = alg.N  DO
      FOR i := 1 TO N-1 DO
        SortIE.StartPass(alg);
        mn := i;
        (* SortIE.BestSoFar(alg, mn); *)
        FOR j := i + 1 TO N DO
          (* SortIE.Compare(alg, j, a[mn]); *)
          IF a[j] < a[mn] THEN
            mn := j;
            (* SortIE.BestSoFar(alg, mn); *)
          END
        END;
        SwapElts(alg, mn, i)
      END
    END
  END SelectionRun;


(*****************************  Bubble Sort  *****************************)

PROCEDURE BubbleNew (): Algorithm.T =
  BEGIN
    RETURN
      NEW(BubbleSort, 
          defaultN := 32,
          data := ZeusPanel.NewForm("SortData.fv")).init()
  END BubbleNew;

PROCEDURE BubbleRun (alg: BubbleSort) RAISES {Thread.Alerted} =
  BEGIN
    GetData (alg);
    WITH a=alg.a, N=alg.N DO
      FOR i := 1 TO N-1 DO
        SortIE.StartPass(alg);
        FOR j := 2 TO N DO
          IF a[j - 1] > a[j] THEN
            SwapElts(alg, j, j - 1);
          ELSIF j <= N - i + 1 THEN
            SetVal(alg, j, a[j]);
          END;
        END;
      END;
    END
  END BubbleRun;


(*****************************  Shaker Sort  *****************************)

PROCEDURE ShakerNew (): Algorithm.T =
  BEGIN
    RETURN NEW(
             ShakerSort,
             defaultN := 32,
             data := ZeusPanel.NewForm("SortData.fv")).init()
  END ShakerNew;

PROCEDURE ShakerRun (alg: ShakerSort) RAISES {Thread.Alerted} =
  VAR l, r: CARDINAL;
  BEGIN
    GetData (alg);
    WITH a=alg.a, N=alg.N DO
      l := 1;
      r := N;
      WHILE (l < r) DO
        SortIE.StartPass(alg);
        FOR j := l + 1 TO r DO
          IF a[j - 1] > a[j] THEN SwapElts(alg, j, j - 1); END;
        END;
        DEC(r);
        SortIE.StartPass(alg);
        FOR j := r - 1 TO l BY -1 DO
          IF a[j + 1] < a[j] THEN SwapElts(alg, j, j + 1); END;
        END;
        INC(l);
      END
    END
  END ShakerRun;


(*****************************  Shell Sort  *****************************)

PROCEDURE ShellNew (): Algorithm.T =
  BEGIN
    RETURN NEW(
             ShellSort, defaultN:=400,
             data := ZeusPanel.NewForm("SortData.fv")).init()
  END ShellNew;

PROCEDURE ShellPasses (alg: ShellSort): CARDINAL =
  VAR passes := 0; h := 1;
  BEGIN
    WITH N = alg.N DO
      REPEAT h := 3 * h + 1; UNTIL h > N;
      REPEAT
        h := h DIV 3;
        FOR i := h + 1 TO N DO INC(passes); END;
      UNTIL h = 1;
    END;
    RETURN passes
  END ShellPasses;

PROCEDURE ShellRun (alg: ShellSort) RAISES {Thread.Alerted} =
  VAR j, h: CARDINAL; v: Sort.Key;
  BEGIN
    GetData (alg);
    WITH a=alg.a, N=alg.N DO
      h := 1;
      REPEAT h := 3 * h + 1; UNTIL h > N;
      REPEAT
        h := h DIV 3;
        FOR i := h + 1 TO N DO
          SortIE.StartPass(alg);
          v := a[i];
          j := i;
          LOOP
            IF a[j - h] <= v THEN EXIT; END;
            SetVal(alg, j, a[j - h]);
            j := j - h;
            IF j <= h THEN EXIT; END;
          END;
          SetVal(alg, j, v);
        END;
      UNTIL h = 1;
    END
  END ShellRun;


(*****************************  Heapsort  *****************************)

PROCEDURE HeapNew (): Algorithm.T =
  BEGIN
    RETURN NEW(
             HeapSort,
             defaultN := 400,
             data := ZeusPanel.NewForm("SortData.fv")).init()
  END HeapNew;

PROCEDURE HeapPasses (alg: HeapSort): CARDINAL =
  BEGIN
    RETURN alg.N + alg.N DIV 2
  END HeapPasses;

PROCEDURE HeapRun (alg: HeapSort) RAISES {Thread.Alerted} =
  VAR H: INTEGER;

  PROCEDURE SiftDown (k: CARDINAL) RAISES {Thread.Alerted} =
    VAR j: CARDINAL; v: Sort.Key;
    BEGIN
      WITH a = alg.a DO
        v := a[k];
        LOOP
          IF (k > H DIV 2) THEN EXIT; END;
          j := k + k;
          IF (j < H) THEN
            IF a[j] < a[j + 1] THEN j := j + 1; END;
          END;
          IF v >= a[j] THEN EXIT; END;
          SetVal(alg, k, a[j]);
          k := j;
        END;
        SetVal(alg, k, v);
      END
    END SiftDown;

  BEGIN 
    GetData (alg);
    WITH N = alg.N DO
      H := N;
      FOR k := N DIV 2 TO 1 BY -1 DO 
        SortIE.StartPass(alg); 
        SiftDown(k); 
      END;
      ZeusPanel.Pause(alg, "Heap made; ready to sort...");
      FOR k := N TO 2 BY -1 DO
        SortIE.StartPass(alg); 
        SwapElts(alg, 1, H);
        H := H - 1;
        SiftDown(1);
      END;
    END 
  END HeapRun;


(*****************************  Radix Sort  *****************************)

PROCEDURE RadixNew (): Algorithm.T =
  BEGIN
    RETURN NEW(
             RadixSort,
             defaultN := 400,
             data := ZeusPanel.NewForm("SortData.fv")).init()
  END RadixNew;

PROCEDURE RadixPasses (<* UNUSED *> alg: RadixSort): CARDINAL =
  BEGIN
    RETURN 17
  END RadixPasses;

PROCEDURE RadixRun (alg: RadixSort) RAISES {Thread.Alerted} =
  CONST M=2; m=1;
  VAR pow: INTEGER; count: ARRAY[0..M-1] OF INTEGER;
  BEGIN
    GetData (alg);
    WITH a=alg.a, b=alg.b, N=alg.N DO
      pow := 1;
      FOR pass := 0 TO 15 DO
        SortIE.StartPass(alg);
        IF pow <= N THEN
          pow := pow + pow;
          FOR i := 1 TO N DO b[i] := a[i] END;
          FOR j := 0 TO M - 1 DO count[j] := 0; END;
          FOR i := 1 TO N DO
            WITH d = Word.Extract(b[i], pass * m, m) DO
              count[d] := count[d] + 1
            END
          END;
          FOR j := 1 TO M - 1 DO count[j] := count[j - 1] + count[j]; END;
          FOR i := N TO 1 BY -1 DO
            WITH d = Word.Extract(b[i], pass * m, m) DO
              SetVal(alg, count[d], b[i]);
              count[d] := count[d] - 1
            END
          END
        END
      END
    END
  END RadixRun;


(*****************************  Mergesort  *****************************)

PROCEDURE BUMergeNew (): Algorithm.T =
  BEGIN
    RETURN
      NEW(BUMergeSort, defaultN := 400,
          data := ZeusPanel.NewForm("SortData.fv")).init()
  END BUMergeNew;

CONST maxint = 999999;
TYPE Link = REF RECORD k: Sort.Key; next: Link END;

PROCEDURE mergelists(alg: BUMergeSort; a, b: Link; aX, bX, cX: INTEGER): Link
  RAISES {Thread.Alerted} =
  VAR c, head: Link;
  BEGIN
    head := NEW(Link);
    c := head;
    REPEAT 
      IF a.k < b.k THEN 
        c.next:=a; c:=a; a:=a.next; INC(aX);
      ELSE
        c.next:=b; c:=b; b:=b.next; INC(bX); 
      END;
   (* SortIE.Merge(alg, aX, bX); *)
      IF c.k # maxint THEN SetVal(alg, cX, c.k); INC(cX); END;
    UNTIL c.k=maxint;
    RETURN head.next
  END mergelists;


PROCEDURE BUMergeRun (alg: BUMergeSort) RAISES {Thread.Alerted} =
  VAR a,b,c,head,todo,t: Link;
      N,pp: INTEGER;
      z: Link;
  BEGIN
  GetData (alg);
  z:=NEW(Link); z.k := maxint; z.next := z;
  N:=1; head:=NEW(Link); t:=head;
  FOR i := 1 TO alg.N DO 
    t.next:=NEW(Link); t := t.next; t.k := alg.a[i]
  END; 
  t.next := z; z.next := z;
  REPEAT
    todo:=head.next; c:=head; pp:=1;
    REPEAT
      SortIE.StartPass(alg);
      t:=todo;
      a:=t; FOR i:=1 TO N-1 DO t:=t.next END;
      b:=t.next; t.next:=z;
      t:=b; FOR i:=1 TO N-1 DO t:=t.next END;
      todo:=t.next; t.next:=z;
      c.next:=mergelists(alg, a, b, pp,pp+N, pp);
      FOR i:=1 TO N+N DO c:=c.next END; pp:=pp+N+N
    UNTIL todo=z;
    N:=N+N;
  UNTIL (a=head.next) OR (b=head.next);
  END BUMergeRun;


(*****************************  Quick Sort  *****************************)

PROCEDURE QuickNew (): Algorithm.T =
  BEGIN
    RETURN NEW(
             QuickSort, defaultN := 400, 
             data := ZeusPanel.NewForm("SortData.fv")).init()
  END QuickNew;

PROCEDURE QuickRun (alg: QuickSort) RAISES {Thread.Alerted} =
  PROCEDURE quicksort(l, r: CARDINAL) RAISES {Thread.Alerted} =
    VAR
      i, j: CARDINAL; v: Sort.Key;
    BEGIN
      WITH a = alg.a DO
        IF r >= l THEN SortIE.StartPass(alg) END;
        IF r > l THEN
          v := a[r];
          i := l - 1;
          j := r;
          REPEAT
            REPEAT SetVal(alg, i, a[i]); i := i + 1; UNTIL a[i] >= v;
            REPEAT SetVal(alg, j, a[j]); j := j - 1; UNTIL a[j] <= v;
            SwapElts(alg, i, j);
          UNTIL j <= i;
          SwapElts(alg, i, j);
          SwapElts(alg, i, r);
          (* SortIE.SplitAt(me, i, Thread.Self());*)
          IF i - l > r - i THEN
            quicksort(l, i - 1);
            quicksort(i + 1, r);
          ELSE
            quicksort(i + 1, r);
            quicksort(l, i - 1);
          END;
         (* SortIE.JoinAt(me, i, Thread.Self());*)
        END;
      END
    END quicksort;
  BEGIN
    GetData (alg);
    quicksort(1, alg.N);
  END QuickRun;


(*****************************  Mainline  *****************************)
				       
BEGIN
  ZeusPanel.RegisterAlg(InsertionNew, "Insertion sort", "Sort");
  ZeusPanel.RegisterAlg(SelectionNew, "Selection sort", "Sort");
  ZeusPanel.RegisterAlg(BubbleNew, "Bubble sort", "Sort");
  ZeusPanel.RegisterAlg(ShakerNew, "Shaker sort", "Sort");
  ZeusPanel.RegisterAlg(ShellNew, "Shell sort", "Sort");
  ZeusPanel.RegisterAlg(HeapNew, "Heapsort", "Sort");
  ZeusPanel.RegisterAlg(RadixNew, "Radix sort", "Sort");
  ZeusPanel.RegisterAlg(BUMergeNew, "Bottom-Up Mergesort", "Sort");
  ZeusPanel.RegisterAlg(QuickNew, "Quicksort", "Sort");

  ZeusPanel.RegisterView(ZeusDataView.New, "Data View", "Sort");
END SortAlgs.
