GENERIC MODULE UnitDatabase(R);

IMPORT PhysicalUnit AS U;

PROCEDURE AddUnit(VAR db:T;
                  READONLY unit:ARRAY OF U.ExpType;
                  flags:UsualUnitFlagSet;
                  READONLY scales:ARRAY OF ScaledUnitInit)=
VAR
  uu := NEW(UsualUnit,unit:=U.FromArray(unit),flags:=flags);
BEGIN
  (* add new item to the list *)
  uu.next  := db.first;
  db.first := uu;

  VAR
    cnt : CARDINAL := 0;
  BEGIN
    FOR j:=0 TO LAST(scales) DO
      IF ScaledUnitFlags.isUnit IN scales[j].flags THEN
        INC(cnt);
      END;
    END;
    uu.scales:=NEW(REF ARRAY OF ScaledUnit,cnt);
  END;
  VAR
    k : CARDINAL := 0;
  BEGIN
    FOR j:=0 TO LAST(scales) DO
      IF ScaledUnitFlags.isUnit IN scales[j].flags THEN
        uu.scales[k].mag    := scales[j].mag;
        uu.scales[k].symbol := scales[j].symbol;
        IF ScaledUnitFlags.default IN scales[j].flags THEN
          uu.defScale := k;
        END;
        INC(k);
      END;
    END;
  END;
END AddUnit;

(*
PROCEDURE DecomposeUnit(READONLY db:T;unit:U.T):CompositeUnit=
VAR
  v      : VALUE;
  ucList : CompositeUnitPtr := NIL;
  uc     : UnitComponentPtr;
  maxUU,
  uu     : UsualUnitPtr := NIL;
  minDiff, diff,
  minExp,  minAbsExp, exp, maxExp,
  n      : INTEGER;
  remain : VALUE;

BEGIN
(*WriteString ("CreateCompositeUnit (memPool : PoolHeaderPtr; unit : UnitArrPtr) : CompositeUnitPtr;"+&10); *)
  ucList.Init();
  v.z.r  := 1;  (* # 0 *)
  v.unit := unit;
  IF NOT ValueOne.EqualUnit (v) THEN
    (*WriteFormat ("unit %ld, %lx, [%ld, %ld, %ld, %ld, %ld]"+&10, data := unit'RANGE, ANYPTR(unit), unit[0], unit[1], unit[2], unit[3], unit[4]); *)
    remain.New (unit'RANGE);
    v.CopyUnit (remain);
    ucList.factor := 1.;

    REPEAT
      minDiff := minDiff'MAX;
      maxExp  := 0;
      uu := FuncDict (funcDict).usualUnits.First();
      WHILE uu#NIL DO
        minAbsExp := minAbsExp'MAX;
        minExp    := 0;
        n:=remain.unit'RANGE;
        WHILE minAbsExp>0
          AND_WHILE n>0 DO
            DEC (n);
            IF uu.basicUnit[n]#0 THEN
              exp := remain.unit[n] DIV uu.basicUnit[n]; (* exp = 0 does not necessary mean that remain.unit[n] = 0 *)
              IF = OR ((minExp#0) AND ((minExp>0) # (exp>0))) THEN
                minAbsExp := 0;
                minExp    := 0;
              OR_IF (exp>0)
                AND_IF (minAbsExp>exp) THEN
                  minAbsExp := exp;
                  minExp    := exp;
                END
              OR_IF (exp<0)
                AND_IF (minAbsExp>-exp) THEN
                  minAbsExp := -exp;
                  minExp    :=  exp;
                END
              END;
            END;

          ELSE
            diff := 0;
            n := remain.unit'RANGE;
            WHILE n>0 DO
              DEC (n);
              IF remain.unit[n]>0 THEN
                INC (diff, remain.unit[n] - uu.basicUnit[n] * minExp);
              ELSE
                DEC (diff, remain.unit[n] - uu.basicUnit[n] * minExp);
              END;
            END;
            (*WriteFormat ("%s, minExp %ld, diff %ld, mindiff %ld, component? %ld"+&10, data := uu.mainUnit.name.data'ADR, minExp, diff, minDiff, CAST(SHORTINT,uu.isComponent)); *)

            IF ((diff<minDiff) OR
                ((maxExp<=0) AND (minExp>0) AND (diff<=minDiff))) AND
               (uu.isComponent OR ((ucList.first=NIL) AND (diff=0)))  THEN
              minDiff := diff;
              maxUU   := uu;
              maxExp  := minExp;
            END;

          END
        END;
        uu := FuncDict (funcDict).usualUnits.Next (uu);
      END;

      n:=remain.unit'RANGE;
      WHILE n>0 DO
        DEC (n);
        DEC (remain.unit[n], maxUU.basicUnit[n] * maxExp);
      END;

      memPool.NewPooled (uc);
      uc.uu  := maxUU;
      uc.exp := maxExp;
      (*the factor of the first unit is excluded for now,
        it will be considered when searching for an appropriate prefix*)
      IF ucList.first#NIL THEN
        ucList.factor := ucList.factor * maxUU.mainUnit.constant.value.z.r ^ LONGREAL(maxExp);
      END;
      ucList.InsertBottom (uc);

    UNTIL minDiff=0;
    (*WriteFormat ("total units %ld"+&10, data := ucList.Count()); *)
    (*WriteFormat ("unit list first %lx"+&10, data := ANYPTR(ucList.first)); *)

    IF ucList.first.exp < 0 THEN
      uc := ucList.first;
      WHILE uc#NIL
        AND_WHILE uc.exp<=0 DO
          uc := uc.next;
        ELSE
          ucList.InsertAfter (uc, ucList.RemoveFirst());
        END
      END;
    END;

  END;
  RETURN ucList;
END DecomposeUnit;
*)

BEGIN
END UnitDatabase.

