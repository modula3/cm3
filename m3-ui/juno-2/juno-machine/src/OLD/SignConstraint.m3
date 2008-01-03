(* ----------------- Begin Sign-Constraint Code -------------------------------

  TYPE
    HeapRec = RECORD
      t: T;
      con: CARDINAL
    END;
    Heap = REF ARRAY OF HeapRec;
    (* "Heap" is a Floyd heap with the largest value for "t" at the root. *)

  PROCEDURE UpdateVars() =
  (* Update the "n" unknowns of "v" by "t * x" for "t" in "[0..1]" such that
     the result satisfies the sign constraint of each multiplicative
     constraint. *)
    VAR w := NEW(REF ARRAY OF T, n); badCnt := 0; BEGIN
      FOR i := 0 TO n - 1 DO
        w[i] := v[i] + x[i]
      END;
      FOR i := FIRST(c) TO LAST(c) DO
        TYPECASE c[i] OF
          Times(tm) =>
            tm.parity := FALSE;
            FOR j := 0 TO 2 DO
              IF w[tm.arg[j]] < 0.0 THEN tm.parity := NOT tm.parity END
            END;
            IF tm.parity THEN INC(badCnt) END
        ELSE (* SKIP *)
        END
      END;
      IF badCnt = 0 THEN
        SUBARRAY(v, 0, n) := w^;
        RETURN
      END;
      VAR heap := NEW(Heap, 3 * NUMBER(c)); heapCnt: CARDINAL := 0; BEGIN
        FOR i := FIRST(c) TO LAST(c) DO
          TYPECASE c[i] OF
            Times(tm) =>
              FOR j := 0 TO 2 DO
                VAR var := tm.arg[j]; t: T; BEGIN
                  IF x[var] # 0.0 THEN
                    t := -v[var] / x[var];
                    IF 0.0 < t AND t <= 1.0 THEN
                      Insert(heap, t, i, heapCnt)
                    END
                  END
                END
              END
          ELSE (* SKIP *)
          END
        END;
        VAR currT: T; BEGIN
          WHILE badCnt # 0 AND heapCnt > 0 DO
            currT := heap[0].t;
            WHILE heapCnt > 0 AND currT = heap[0].t DO
              VAR tm: Times := c[heap[0].con]; BEGIN
                tm.parity := NOT tm.parity;
                INC(badCnt, (2 * ORD(tm.parity)) - 1)
              END;
              Delete(heap, heapCnt)
            END
          END;
          IF heapCnt = 0 AND badCnt > 0 THEN
            currT := 0.1
          ELSIF heapCnt # 0 THEN
            currT := 0.5 * (currT + heap[0].t)
          ELSE
            currT := 0.5 * currT
          END;
          IF debug >= 3 THEN
            Wr.PutText(stderr, "       t: " &
              Fmt.Pad(Fmt.Real(currT, 3, Fmt.Style.Sci), 9) & "\n")
          END;
          FOR i := 0 TO n - 1 DO
            v[i] := v[i] + (currT * x[i])
          END;
        END
      END
    END UpdateVars;

  PROCEDURE Insert(
      hp: Heap;
      t: T;
      con: CARDINAL;
      VAR (*INOUT *) cnt: CARDINAL) =
    VAR i := cnt; BEGIN
      hp[i].t := t;
      hp[i].con := con;
      WHILE i # 0 DO
        VAR p := (i - 1) DIV 2; BEGIN
          IF hp[p].t < hp[i].t THEN
            VAR temp := hp[p]; BEGIN hp[p] := hp[i]; hp[i] := temp END;
            i := p
          ELSE
            EXIT
          END
        END
      END;
      INC(cnt)
    END Insert;

  PROCEDURE Delete(hp: Heap; VAR (*INOUT *) cnt: CARDINAL) =
    VAR i := 0; BEGIN
      DEC(cnt);
      hp[0] := hp[cnt];
      LOOP
        VAR iMax := i; BEGIN
          FOR j := 1 TO 2 DO
            VAR ix := 2 * i + j; BEGIN
              IF ix < cnt AND hp[ix].t > hp[iMax].t THEN
                iMax := ix
              END
            END
          END;
          IF i = iMax THEN EXIT END;
          VAR temp := hp[i]; BEGIN hp[i] := hp[iMax]; hp[iMax] := temp END;
          i := iMax
        END
      END
    END Delete;

  --------------------- End Sign-Constraint Code --------------------------- *)
