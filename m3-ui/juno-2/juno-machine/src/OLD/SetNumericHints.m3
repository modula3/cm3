(* Last modified on Thu Sep 24 22:25:15 PDT 1992 by heydon *)

(* This is the code from JunoSolve.m3 that was ripped out to be transferred to
   NonLinearSolve.m3.
*)

  Var = Public BRANDED OBJECT
    ..
    uses: UseList
  END;

PROCEDURE SetNumericHints(READONLY c: ARRAY OF Constraint) =
(* We say an equivalence class "ec" {\it occurs} in a constraint if "ec"
   is unhinted and contains a variable appearing in the constraint. 

   Q1: For each constraint in "c", "c.hintCnt" is the number of distinct
       equivalence classes occurring in "c". The constraints with "hintCnt" 1
       are linked via the "next" field, whose head is "hintReady".

   Q2: For any root "r" of an equivalence class occurring in some constraint
       in "c", "r.uses" is a list that contains an entry "ul" for each
       constraint in which "r"'s equivalence class occurs. The value "ul.c" is
       the constraint for the occurrence.

   Q3: The hint for an equivalence class appearing in a numeric
       constraint is either "JunoValue.Nil" or numeric.

   Q4: For any root "r" of an equivalence class occurring in some numeric
       constraint in "c", "r.hinted" is "TRUE" iff "r.val" was a non-NIL
       numeric value on entry to this procedure.
*)
  VAR hintReady: Constraint := NIL; BEGIN
    FOR i := FIRST(c) TO LAST(c) DO
      c[i].hintCnt := 0;
      FOR j := 0 TO NumericArgCnt(c[i]) - 1 DO
        VAR arg: Var := c[i].arg[j].find(); hint := arg.val; BEGIN
          IF hint # JunoValue.Nil AND ISTYPE(hint, REF T) THEN
            arg.hinted := TRUE
          ELSE
            arg.val := JunoValue.Nil;
            IF arg.uses = NIL OR arg.uses.c # c[i] THEN
              INC(c[i].hintCnt);
              arg.uses := NEW(UseList, c := c[i], next := arg.uses);
            END
          END
        END
      END;
      (* Add to "hintReady" queue if necessary *)
      IF c[i].hintCnt = 1 THEN
        c[i].next := hintReady;
        hintReady := c[i]
      END
    END;
    (* Q1, Q2, Q3, AND Q4 *)
    WHILE hintReady # NIL DO
      IF hintReady.hintCnt = 0 THEN
        (* Skip over constraints on the queue whose constraints were 1 when
           they were added, but have since been decremented to 0. *) 
        hintReady := hintReady.next
      ELSE
        <* ASSERT hintReady.hintCnt = 1 *>
	VAR con := hintReady; BEGIN
          hintReady := hintReady.next;
          SolveCon(con, hintReady)
        END
      END
    END
  END SetNumericHints;

PROCEDURE SolveCon(c: Constraint; VAR (*INOUT*) head: Constraint) =
  VAR
    mask := 0;
    known: ARRAY [0..2] OF T;
    unknown: Var;

  PROCEDURE SetHint(x: T) =
    BEGIN
      VAR v := NEW(REF T); BEGIN
        v^ := x;
        unknown.val := v
      END;
      VAR l := unknown.uses; BEGIN
        WHILE l # NIL DO
          DEC(l.c.hintCnt);
          IF l.c.hintCnt = 1 THEN
            (* Add "l.c" to front of the list at "head" *)
            l.c.next := head;
            head := l.c;
          END;
          l := l.next
        END
      END
    END SetHint;

  BEGIN
    FOR i := 0 TO NumericArgCnt(c) - 1 DO
      VAR v: Var := c.arg[i].find(); BEGIN
        IF v.val = JunoValue.Nil THEN
          INC(mask, Word.LeftShift(1, i));
          unknown := v
        ELSE
          known[i] := NARROW(v.val, REF T)^
        END
      END
    END;
    TYPECASE c OF <* NOWARN *>
      Plus =>
        CASE mask OF <* NOWARN *>
          2_001 => SetHint(known[1] + known[2])
        | 2_010 => SetHint(known[0] - known[2])
        | 2_100 => SetHint(known[0] - known[1])
        | 2_110 => SetHint(known[0] / 2.0)
        | 2_011, 2_101, 2_111 => SetHint(0.0)
        END
    | Times =>
        CASE mask OF <* NOWARN *>
          2_001 => SetHint(known[1] * known[2])
        | 2_010 =>
            IF known[2] # 0.0 THEN
              SetHint(known[0] / known[2])
            END
        | 2_100 =>
            IF known[1] # 0.0 THEN
              SetHint(known[0] / known[1])
            END
        | 2_110, 2_111 => (* SKIP *)
        | 2_011, 2_101 => SetHint(0.0)
        END
    | Sin =>
        CASE mask OF <* NOWARN *>
          2_01 => SetHint(JunoValue.Sin(known[1]))
        | 2_10 =>
            IF ABS(known[1]) <= 1.0
              THEN SetHint(JunoValue.Asin(known[1]))
              ELSE SetHint(0.0)
            END
        | 2_11 => SetHint(0.0)
        END
    | Cos =>
        CASE mask OF <* NOWARN *>
          2_01 => SetHint(JunoValue.Cos(known[1]))
        | 2_10 =>
             IF ABS(known[1]) <= 1.0 THEN
               SetHint(JunoValue.Acos(known[1]))
            END
        | 2_11 => SetHint(0.74)
        END
    | Atan =>
        CASE mask OF <* NOWARN *>
          2_01 => SetHint(JunoValue.Atan(known[1]))
        | 2_10 => SetHint(JunoValue.Tan(known[1]))
        | 2_11 => SetHint(0.0)
        END
    | Exp =>
        CASE mask OF <* NOWARN *>
          2_01 => SetHint(JunoValue.Exp(known[1]))
        | 2_10 =>
            IF known[1] > 0.0 THEN
              SetHint(JunoValue.Ln(known[1]))
            END
        | 2_11 => (* SKIP *)
        END
    END
  END SolveCon;
