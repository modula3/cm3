GENERIC MODULE UnitDatabase(CU,CUList);

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
PROCEDURE NextItem(VAR uu:UsualUnit):BOOLEAN=
  BEGIN
    uu:=uu.next;
    RETURN uu#NIL;
  END NextItem;
*)

(*find the usual unit incl. exponent which matches best to the given unit*)
PROCEDURE FindBestUU(READONLY db:T;remain:U.T;isFirst:BOOLEAN):CU.T=
  VAR
    maxUU  : UsualUnit := NIL;
    uu     := db.first;
    minDiff: U.ExpType := LAST(U.ExpType);
    maxExp : U.ExpType := 0;
  BEGIN
    WHILE uu#NIL DO
      (*find the maximum sensible exponent for the current usual unit*)
      VAR
        minAbsExp  :U.ExpType:=LAST(U.ExpType);
        minExp     :U.ExpType:=0;
      BEGIN
        VAR
          it:=uu.unit.iterate();
          dim:INTEGER;
          uuExp,rmExp,
          exp        :U.ExpType;
        BEGIN
          WHILE minAbsExp>0 AND it.next(dim,uuExp) DO
            <*ASSERT dim>0*>
            rmExp := 0;
            EVAL remain.get(dim,rmExp);
            exp := rmExp DIV uuExp;
            (* exp = 0 does not necessary mean that rmExp = 0 *)
            IF exp=0 OR (minExp#0 AND (minExp>0) # (exp>0)) THEN
              minAbsExp := 0;
              minExp    := 0;
            ELSIF exp>0 THEN
              IF minAbsExp>exp THEN
                minAbsExp := exp;
                minExp    := exp;
              END
            ELSIF exp<0 THEN
              IF minAbsExp>-exp THEN
                minAbsExp := -exp;
                minExp    :=  exp;
              END
            END;
          END;
        END;

        IF minAbsExp>0 THEN
          VAR
            it:=remain.iterate();
            diff:U.ExpType:=0;
            dim:INTEGER;
            uuExp,rmExp:U.ExpType;
          BEGIN
            WHILE it.next(dim,rmExp) DO
              <*ASSERT dim>0*>
              uuExp:=0;
              EVAL uu.unit.get(dim,uuExp);
              IF rmExp>0 THEN
                INC (diff, rmExp - uuExp * minExp);
              ELSE
                DEC (diff, rmExp - uuExp * minExp);
              END;
            END;
            (*WriteFormat ("%s, minExp %ld, diff %ld, mindiff %ld, component? %ld"+&10, data := uu.mainUnit.name.data'ADR, minExp, diff, minDiff, CAST(SHORTINT,uu.isComponent)); *)

            IF (diff<minDiff OR
                (maxExp<=0 AND minExp>0 AND diff<=minDiff)) AND
               (NOT UsualUnitFlags.independent IN uu.flags OR
                (isFirst AND diff=0))  THEN
              minDiff := diff;
              maxUU   := uu;
              maxExp  := minExp;
            END;
          END;

        END;
      END;

      uu := uu.next;
    END;

    RETURN CU.T{uu := maxUU, exp := maxExp};
  END FindBestUU;


PROCEDURE DecomposeUnit(READONLY db:T;unit:U.T):CUList.T=
VAR
  ucList : CUList.T := NIL;
  remain := U.Copy(unit);

BEGIN
  WHILE NOT U.IsZero (remain) DO
    (* prepend the new unit *)
    ucList:=CUList.Cons(FindBestUU(db,remain,ucList=NIL),ucList);

    (* extract the found usual unit from the given one *)
    remain:=U.Sub(remain,U.Scale(ucList.head.uu.unit,ucList.head.exp));
  END;

  (*reverse order and
    sort usual units with negative exponent to the end*)
  VAR
    ucPos,ucNeg:CUList.T:=NIL;
    ucFirst:CUList.T:=NIL;
  BEGIN
    WHILE ucList#NIL DO
      ucFirst:=ucList;
      ucList:=ucList.tail;
      IF ucFirst.head.exp < 0 THEN
        ucNeg := CUList.Append(ucFirst,ucNeg);
      ELSE
        ucPos := CUList.Append(ucFirst,ucPos);
      END;
    END;
    ucList:=CUList.AppendD(ucPos,ucNeg);
  END;

  RETURN ucList;
END DecomposeUnit;

BEGIN
END UnitDatabase.

