GENERIC MODULE UnitDatabase(UU,UUList,CU,CUList);

IMPORT PhysicalUnit       AS U,
       PhysicalUnitFmtLex AS UF;

IMPORT IO, Fmt;

(*
PROCEDURE NextItem(VAR uu:UU.T):BOOLEAN=
  BEGIN
    uu:=uu.next;
    RETURN uu#NIL;
  END NextItem;
*)

(*should be part of the List module*)
PROCEDURE RemoveFirst(VAR l:CUList.T):CUList.T=
  VAR
    fst:CUList.T;
  BEGIN
    fst:=l;
    l:=l.tail;
    fst.tail:=NIL;
    RETURN fst;
  END RemoveFirst;


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

(*approximate a scaling for which the source unit matches the target unit best
  return the approximation error*)
PROCEDURE Approx (target, source : U.T; VAR exp:U.ExpType) : U.ExpType =
  VAR
    zeroDiff := U.Norm1(target);
    diff, lastDiff : U.ExpType;
  BEGIN
    <*ASSERT NOT U.IsZero(source)*>
    (*the mapping x->norm(target-x*source) is a norm along an affine function
      and thus this mapping is convex and has at most one interval
      where it is minimal,
      of all those x where norm(target-x*source) is minimal,
      choose the one with least absolute value*)
    diff := U.Norm1(U.Sub(target,source));
    IF zeroDiff > diff THEN
      exp := 1;
      REPEAT
        lastDiff := diff;
        INC(exp);
        diff := U.Norm1(U.Sub(target,U.Scale(source,exp)));
      UNTIL lastDiff <= diff;
      DEC(exp);
      RETURN lastDiff;
    ELSIF zeroDiff < diff THEN
      diff := zeroDiff;
      exp := 0;
      REPEAT
        lastDiff := diff;
        DEC(exp);
        diff := U.Norm1(U.Sub(target,U.Scale(source,exp)));
      UNTIL lastDiff <= diff;
      INC(exp);
      RETURN lastDiff;
    ELSE
      exp := 0;
      RETURN diff;
    END;
  END Approx;

(*find the usual unit incl. exponent which matches best to the given unit*)
PROCEDURE FindBestUU(READONLY db:T;remain:U.T;isFirst:BOOLEAN):CU.T=
  VAR
    bestUU  : UUList.T := NIL;
    uu      := db.first;
    exp,
    diff    : U.ExpType;
    minDiff : U.ExpType := LAST(U.ExpType);
    minExp  : U.ExpType := LAST(U.ExpType);
  BEGIN
IO.Put("find unit closest to " & UF.Fmt(remain) & "\n");
    WHILE uu#NIL DO
IO.Put(UF.Fmt(uu.head.unit)&"  ");
      diff := Approx(remain,uu.head.unit,exp);
IO.Put("   exp " & Fmt.Int(exp));
IO.Put("   diff " & Fmt.Int(diff));
IO.Put("   indep " & Fmt.Bool(UU.Flags.independent IN uu.head.flags));
IO.Put("   isFirst " & Fmt.Bool(isFirst) & "\n");
      IF (diff<minDiff OR
          (diff<=minDiff AND ABS(exp)<ABS(minExp))) AND
         (NOT UU.Flags.independent IN uu.head.flags OR
          (isFirst AND diff=0)) THEN
        minDiff := diff;
        bestUU  := uu;
        minExp  := exp;
      END;
      uu := uu.tail;
    END;

    RETURN CU.T{uu := bestUU, exp := minExp};
  END FindBestUU;


PROCEDURE DecomposeUnit(READONLY db:T;unit:U.T):CUList.T=
VAR
  ucList     : CUList.T := NIL;
  remainNorm := U.Norm1(unit);
  newNorm    :  U.ExpType;
  remain     := U.Copy(unit);

BEGIN
  WHILE NOT U.IsZero (remain) DO
    (* prepend the new unit *)
    ucList:=CUList.Cons(FindBestUU(db,remain,ucList=NIL),ucList);

    (* extract the found usual unit from the given one *)
IO.Put("best unit " & UF.Fmt(ucList.head.uu.head.unit) & "\n");
IO.Put("scaled by " & Fmt.Int(ucList.head.exp) & ": " & UF.Fmt(U.Scale(ucList.head.uu.head.unit,ucList.head.exp)) & "\n");
IO.Put("remain before Sub " & UF.Fmt(remain) & "\n");
    remain:=U.Sub(remain,U.Scale(ucList.head.uu.head.unit,ucList.head.exp));
IO.Put("remain after Sub " & UF.Fmt(remain) & "\n");
    newNorm := U.Norm1(remain);
    (*the database must contain all unit vectors so that every composed unit can be decomposed
      into unit vectors from the database*)
    <*ASSERT newNorm < remainNorm *>
    remainNorm := newNorm;
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
