GENERIC MODULE UnitDatabase(UU,UUList,CU,CUList);

IMPORT PhysicalUnit AS U;

IMPORT IO, Fmt;

PROCEDURE FmtUnit(unit:U.T):TEXT =
  VAR
    it:=unit.iterate();
    dim:INTEGER;
    exp:U.ExpType;
    res:TEXT:="{";
  BEGIN
    WHILE it.next(dim,exp) DO
      res:=res&"("&Fmt.Int(dim)&","&Fmt.Int(exp)&")";
    END;
    RETURN res&"}";
  END FmtUnit;


PROCEDURE AddUnit(VAR db:T;
                  READONLY unit:ARRAY OF U.ExpType;
                  flags:UU.FlagSet;
                  READONLY scales:ARRAY OF ScaledUnitInit)=
VAR
  newScales : REF ARRAY OF UU.ScaledUnit;
  defScale  : CARDINAL;
BEGIN
  VAR
    cnt : CARDINAL := 0;
  BEGIN
    FOR j:=0 TO LAST(scales) DO
      IF ScaledUnitFlags.isUnit IN scales[j].flags THEN
        INC(cnt);
      END;
    END;
    (*add no entry if none of the entries is usable as unit*)
    IF cnt=0 THEN
      RETURN;
    END;
    newScales:=NEW(REF ARRAY OF UU.ScaledUnit,cnt);
  END;
  VAR
    k : CARDINAL := 0;
  BEGIN
    FOR j:=0 TO LAST(scales) DO
      IF ScaledUnitFlags.isUnit IN scales[j].flags THEN
        newScales[k].mag    := scales[j].mag;
        newScales[k].symbol := scales[j].symbol;
        IF ScaledUnitFlags.default IN scales[j].flags THEN
          defScale := k;
        END;
        INC(k);
      END;
    END;
  END;

  (* add new item to the list *)
  db.first :=
    UUList.Cons(
      UU.T{unit:=U.FromArray(unit),flags:=flags,scales:=newScales,defScale:=defScale},
      db.first);
END AddUnit;

(*
PROCEDURE NextItem(VAR uu:UU.T):BOOLEAN=
  BEGIN
    uu:=uu.next;
    RETURN uu#NIL;
  END NextItem;
*)

PROCEDURE RemoveFirst(VAR l:CUList.T):CUList.T=
  VAR
    fst:CUList.T;
  BEGIN
    fst:=l;
    l:=l.tail;
    fst.tail:=NIL;
    RETURN fst;
  END RemoveFirst;

(*find the usual unit incl. exponent which matches best to the given unit*)
PROCEDURE FindBestUU(READONLY db:T;remain:U.T;isFirst:BOOLEAN):CU.T=
  VAR
    maxUU  : UUList.T := NIL;
    uu     := db.first;
    minDiff: U.ExpType := LAST(U.ExpType);
    maxExp : U.ExpType := 0;
  BEGIN
IO.Put("find unit closest to " & FmtUnit(remain) & "\n");
    WHILE uu#NIL DO
IO.Put(FmtUnit(uu.head.unit)&"  ");
      (*find the maximum sensible exponent for the current usual unit*)
      VAR
        minAbsExp  :U.ExpType:=LAST(U.ExpType);
        minExp     :U.ExpType:=0;
      BEGIN
        VAR
          it:=uu.head.unit.iterate();
          dim:INTEGER;
          uuExp,rmExp,
          exp        :U.ExpType;
          expPos:BOOLEAN;
        BEGIN
          WHILE minAbsExp>0 AND it.next(dim,uuExp) DO
            <*ASSERT dim>=0*> (*we cannot use CARDINAL, since it.next expects INTEGER*)
            <*ASSERT uuExp#0*>
            rmExp := 0;
            EVAL remain.get(dim,rmExp);
            (* divide and round to zero *)
            exp := ABS(rmExp) DIV ABS(uuExp);
            expPos := (rmExp>0) = (uuExp>0);
            (* exp = 0 does not necessary mean that rmExp = 0 *)
            IF exp=0 OR (minExp#0 AND (minExp>0) # expPos) THEN
              minAbsExp := 0;
              minExp    := 0;
            ELSIF expPos THEN
              IF minAbsExp>exp THEN
                minAbsExp := exp;
                minExp    := exp;
              END
            ELSE
              IF minAbsExp>exp THEN
                minAbsExp :=  exp;
                minExp    := -exp;
              END
            END;
          END;
        END;
IO.Put(Fmt.Int(minExp) & "\n");

        IF minAbsExp>0 THEN
          VAR
            it:=remain.iterate();
            diff:U.ExpType:=0;
            dim:INTEGER;
            uuExp,rmExp:U.ExpType;
          BEGIN
            WHILE it.next(dim,rmExp) DO
              <*ASSERT dim>=0*>
              <*ASSERT rmExp#0*>
              uuExp:=0;
              EVAL uu.head.unit.get(dim,uuExp);
              <*ASSERT uuExp=0 OR (rmExp>0)=(uuExp*minExp>0)*>
              IF rmExp>0 THEN
                INC (diff, rmExp - uuExp * minExp);
              ELSE
                DEC (diff, rmExp - uuExp * minExp);
              END;
            END;
IO.Put("   diff " & Fmt.Int(diff) & "\n");
            (*WriteFormat ("%s, minExp %ld, diff %ld, mindiff %ld, component? %ld"+&10, data := uu.mainUnit.name.data'ADR, minExp, diff, minDiff, CAST(SHORTINT,uu.isComponent)); *)

            IF (diff<minDiff OR
                (maxExp<=0 AND minExp>0 AND diff<=minDiff)) AND
               (NOT UU.Flags.independent IN uu.head.flags OR
                (isFirst AND diff=0))  THEN
              minDiff := diff;
              maxUU   := uu;
              maxExp  := minExp;
            END;
          END;

        END;
      END;

      uu := uu.tail;
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
IO.Put("best unit " & FmtUnit(ucList.head.uu.head.unit) & "\n");
IO.Put("scaled by " & Fmt.Int(ucList.head.exp) & ": " & FmtUnit(U.Scale(ucList.head.uu.head.unit,ucList.head.exp)) & "\n");
IO.Put("remain before Sub " & FmtUnit(remain) & "\n");
    remain:=U.Sub(remain,U.Scale(ucList.head.uu.head.unit,ucList.head.exp));
IO.Put("remain after Sub " & FmtUnit(remain) & "\n");
  END;

  (*reverse order and
    sort usual units with negative exponent to the end*)
  VAR
    ucPos,ucNeg:CUList.T:=NIL;
    ucFirst:CUList.T:=NIL;
  BEGIN
    WHILE ucList#NIL DO
      ucFirst:=RemoveFirst(ucList);
      IF ucFirst.head.exp < 0 THEN
        ucNeg := CUList.AppendD(ucFirst,ucNeg);
      ELSE
        ucPos := CUList.AppendD(ucFirst,ucPos);
      END;
    END;
    RETURN CUList.AppendD(ucPos,ucNeg);
  END;
END DecomposeUnit;

BEGIN
END UnitDatabase.
