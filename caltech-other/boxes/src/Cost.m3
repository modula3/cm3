(* $Id$ *)
MODULE Cost;
IMPORT GridPoint;
FROM GridPoint IMPORT Layer;
IMPORT Debug,Fmt;

CONST
  GreedySquareMaxSearch = 2000; (* largest "greedy square" to consider *)

(* must be neighbors *)
PROCEDURE Neighbors(from, to : GridPoint.T) : INTEGER =
  BEGIN 
    IF from.x # to.x THEN
      RETURN private_costs[from.l].xCost
    ELSIF from.y # to.y THEN
      RETURN private_costs[from.l].yCost
    ELSIF to.l > from.l THEN
      RETURN private_costs[from.l].upCost
    ELSIF to.l < from.l THEN
      RETURN private_costs[to.l].upCost  
    ELSE
      Debug.Warning("computing cost from point to itself!");
      RETURN 0
    END
  END Neighbors;

(* what is the cost of getting from src to dst if there are no obstacles *)
(* this computes a TIGHT bound---this is essential for speed!!! *)
PROCEDURE Greedy(READONLY src, dst : GridPoint.T) : INTEGER =
  VAR
    deltaX := ABS(dst.x - src.x);
    deltaY := ABS(dst.y - src.y);
  BEGIN

    WITH square = greedySquare[src.l,dst.l] DO
      IF deltaX > square AND deltaY > square THEN
        RETURN deltaX * bestXcost + deltaY * bestYcost + 
               bestContactCost[src.l,dst.l];
      END
    END;

    WITH a = alts[src.l,dst.l]^ DO
      VAR 
        res := Infty;
      BEGIN
        FOR i := FIRST(a) TO LAST(a) DO
          WITH

(*            greedy = ComputeGreedy(src,dst,a[i])*)
(* XXX inlined code follows *)

              aa = a[i],
              greedy = deltaX * aa.xCost + 
                       deltaY * aa.yCost +
                       aa.contactCost
          DO
            res := MIN(res,greedy)
          END
        END;
        <*ASSERT res >= 0 *>
        RETURN res
      END
    END
  END Greedy;

(* XXX since SRC's compiler doesn't inline, this routine is manually 
   inlined in Greedy, above *)
<*INLINE*>
PROCEDURE ComputeGreedy(READONLY p1, p2 : GridPoint.T; 
                        READONLY alt : Alternative) : INTEGER =
  VAR
    (* XXX dead code---must use cells now *)
    deltaX := ABS(p2.x - p1.x);
    deltaY := ABS(p2.y - p1.y);
  BEGIN
    RETURN
      deltaX * alt.xCost + 
        deltaY * alt.yCost +
        alt.contactCost
  END ComputeGreedy;

TYPE
  (* alt.contactCost is the cost of going from the starting layer, via
     lx and ly, to the ending layer, in the cheapest order *)
  Alternative = RECORD 
    lx, ly : Layer; 
    contactCost : CARDINAL; 
    xCost, yCost := LAST(CARDINAL); 
  END;
  AltArr = REF ARRAY OF Alternative;
  
VAR
  contactCost : ARRAY Layer OF ARRAY Layer OF INTEGER;

PROCEDURE BestLayersInRange(from, to : Layer ; VAR xl, yl : Layer) =
  BEGIN
    <* ASSERT from <= to *>
    xl := from;
    yl := from;
    FOR i := from TO to DO
      IF private_costs[i].xCost < private_costs[xl].xCost THEN xl := i END;
      IF private_costs[i].yCost < private_costs[yl].yCost THEN yl := i END
    END;
    Debug.Out("BestInRange: "  & Fmt.Int(from) & " -> " & Fmt.Int(to) & 
    " -- x: " & Fmt.Int(xl) & " y: " & Fmt.Int(yl))
  END BestLayersInRange;

PROCEDURE AddAlternative(VAR arr : AltArr; alt : Alternative) =
  VAR
    newAlts := NEW(REF ARRAY OF Alternative, NUMBER(arr^) + 1);
  BEGIN
    (* check if we already had it *)
    FOR i := FIRST(arr^) TO LAST(arr^) DO
      IF arr[i] = alt THEN RETURN END
    END;

    (* nope, extend alternatives.. *)
    SUBARRAY(newAlts^,0,NUMBER(arr^)) := arr^;
    newAlts[LAST(newAlts^)] := alt;
    arr := newAlts
  END AddAlternative;

PROCEDURE InitAlternative(i,j : CARDINAL; VAR alt : Alternative) =
  VAR
    minL := MIN(i,j);
    maxL := MAX(i,j);
    altMin := MIN(alt.lx, alt.ly);
    altMax := MAX(alt.lx, alt.ly);
    res := 0;
  BEGIN
    INC(res,contactCost[minL,altMin]);
    IF res > Infty THEN res := Infty END;
    INC(res,contactCost[altMin,altMax]);
    IF res > Infty THEN res := Infty END;
    INC(res,contactCost[altMax,maxL]);
    IF res > Infty THEN res := Infty END;
    alt.contactCost := res;
    alt.xCost := private_costs[alt.lx].xCost;
    alt.yCost := private_costs[alt.ly].yCost;
  END InitAlternative;

VAR
  alts : ARRAY Layer OF ARRAY Layer OF AltArr;
  bestXlayer, bestYlayer := FIRST(Layer);
  greedySquare : ARRAY Layer OF ARRAY Layer OF CARDINAL;
  bestContactCost : ARRAY Layer OF ARRAY Layer OF INTEGER;

PROCEDURE InitModule() =
  BEGIN 
    FOR i := FIRST(Layer) TO LAST(Layer) DO
      IF private_costs[i].xCost < private_costs[bestXlayer].xCost THEN
        bestXlayer := i
      END;
      IF private_costs[i].yCost < private_costs[bestYlayer].yCost THEN
        bestYlayer := i
      END;
    END;
    bestXcost := private_costs[bestXlayer].xCost;
    bestYcost := private_costs[bestYlayer].yCost;

    FOR i := FIRST(Layer) TO LAST(Layer) DO
      FOR j := FIRST(Layer) TO LAST(Layer) DO WITH a = alts[i,j] DO
        a := NEW(AltArr, 0);
        Debug.Out("Doing " & Fmt.Int(i) & " -> " & Fmt.Int(j));
        VAR 
          k, l : Layer; 
          ii := MIN(i,j);
          jj := MAX(i,j);
        BEGIN <*UNUSED*>VAR i, j := FALSE; BEGIN

          BestLayersInRange(ii,jj,k,l);
          AddAlternative(a,Alternative { k, l, LAST(INTEGER) });
          
          (* so we have one alternative.  now expand our gaze... *)
          IF ii # FIRST(Layer) THEN
            FOR f := FIRST(Layer) TO ii - 1 DO
              BestLayersInRange(f,jj,k,l);
              AddAlternative(a, Alternative { k , l, LAST(INTEGER) });
            END
          END;
          IF jj # LAST(Layer) THEN
            FOR f := jj + 1 TO LAST(Layer) DO
              BestLayersInRange(jj,f,k,l);
              AddAlternative(a, Alternative { k , l, LAST(INTEGER) });
            END
          END
        END END
      END END
    END;

    FOR i := FIRST(Layer) TO LAST(Layer) DO
      FOR j := FIRST(Layer) TO LAST(Layer) DO
        WITH a = alts[i,j]^ DO
          FOR k := FIRST(a) TO LAST(a) DO
            Debug.Out(Fmt.Int(i) & " -> " & Fmt.Int(j) & " -- possible x: " &
              Fmt.Int(a[k].lx) & " y: " & Fmt.Int(a[k].ly))
          END
        END
      END
    END;

    FOR i := FIRST(Layer) TO LAST(Layer) DO
      FOR j := i TO LAST(Layer) DO
        contactCost[i,j] := 0;
        contactCost[j,i] := 0;
        IF i # j THEN 
          FOR k := i TO j - 1 DO
            INC(contactCost[i,j], private_costs[k].upCost);
            INC(contactCost[j,i], private_costs[k].upCost)
          END
        END
      END
    END;

    FOR i := FIRST(Layer) TO LAST(Layer) DO
      FOR j := FIRST(Layer) TO LAST(Layer) DO
        FOR k := FIRST(alts[i,j]^) TO LAST(alts[i,j]^) DO
          InitAlternative(i,j,alts[i,j][k])
        END
      END
    END;

    (* compute "greedy square" *)
    (* the greedy square is the smallest square such that, *)
    (* for a given source layer l1 and target layer l2, the *)
    (* chosen layers will always be the cheapest X layer and the *)
    (* cheapest Y layer.  This is used to optimize Greedy, above. *)
    (* if the given delta is greater than the greedy square, then *)
    (* we can skip comparing the "alternatives" and just use the *)
    (* cheapest layers. *)
    FOR i := FIRST(Layer) TO LAST(Layer) DO
      FOR j := FIRST(Layer) TO LAST(Layer) DO
        greedySquare[i,j] := LAST(CARDINAL);
        FOR k := 1 TO GreedySquareMaxSearch DO
          VAR
            origin := GridPoint.T { 0 , 0, i };
            dst := GridPoint.T { k , k, j };
            bestLayers := Alternative { bestXlayer, bestYlayer, LAST(INTEGER) };
          BEGIN
            InitAlternative(i,j,bestLayers);
            IF ComputeGreedy(origin,dst,bestLayers) = Greedy(origin,dst) THEN
              greedySquare[i,j] := k;
              Debug.Out("greedySquare[ " & Fmt.Int(i) & ", " & Fmt.Int(j) & 
                " ] = " & Fmt.Int(k));
              EXIT
            END
          END
        END
      END
    END;

    (* now compute special contact cost array. *)
    (* bestContactCost is the cost of the contacts to go from layer 1 to *)
    (* layer 2 via the bestX layer and the best Y layer *)
    FOR i := FIRST(Layer) TO LAST(Layer) DO
      FOR j := i TO LAST(Layer) DO
        VAR
          res := 0;
        BEGIN
          INC(res, contactCost[MIN(i,j), MIN(bestXlayer, bestYlayer)]);
          INC(res, contactCost[MIN(bestXlayer, bestYlayer), 
                               MAX(bestXlayer, bestYlayer)]);
          INC(res, contactCost[MAX(bestXlayer, bestYlayer), MAX(i,j)]);
          bestContactCost[i,j] := res;
        END
      END
    END;

    FOR i := FIRST(Layer) + 1 TO LAST(Layer) DO
      FOR j := FIRST(Layer) TO i - 1 DO
        bestContactCost[i,j] := bestContactCost[j,i]
      END
    END
  END InitModule;

(* this prevents the thing from being multithreaded *)
VAR private_costs := DefaultCosts;
CONST DefaultMoveMoreThanOneVertically = TRUE;
VAR private_moveMoreThanOneVertically := DefaultMoveMoreThanOneVertically;

PROCEDURE costs() : Costs =
  BEGIN RETURN private_costs END costs;

PROCEDURE Set_costs(READONLY newCosts : Costs) =
  BEGIN
    IF newCosts # private_costs THEN
      private_costs := newCosts;
      InitModule()
    END
  END Set_costs;

PROCEDURE SetMoveMoreThanOneVertically(to : BOOLEAN) =
  BEGIN
    private_moveMoreThanOneVertically := to 
  END SetMoveMoreThanOneVertically;


(* N.B. when MoveMoreThanOneVertically returns FALSE, the 
   bounding function for the distance is no longer tight! *)
PROCEDURE MoveMoreThanOneVertically() : BOOLEAN =
  BEGIN
    RETURN private_moveMoreThanOneVertically
  END MoveMoreThanOneVertically;

BEGIN 
  MinRipUpCost := DefMinRipUpCost;
  RipUpIncrement := DefRipUpIncrement;
  MaxMaxCost := DefMaxMaxCost;
  BaseCost := DefBaseCost;
  MaxCostOverGreedy := DefMaxCostOverGreedy;
  OutOfBoundsIncreaseRate := DefOutOfBoundsIncreaseRate;
  RipUpMultiplier := DefRipUpMultiplier;

  InitModule() 
END Cost.




