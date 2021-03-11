(* $Id$ *)

MODULE SimpleStep;
IMPORT GridPoint;
FROM GridPoint IMPORT Layer;
IMPORT Cost;
IMPORT Filled;
IMPORT Fmt, Debug;
IMPORT Compass;
IMPORT Integer;

(* note carefully:
   for ripping up, we use a special trick so we don't have to compute
   the cost for ripping up (which might involve table lookups, etc.)
   too often.  We return a special placeholder record that is used to
   generate the actual rip-up record when it comes to the head of the
   queue.  For this to work, the cost attached to the placeholder
   record must be less than or equal to the minimum rip-up cost.

 *)

CONST OptPts = 0;
CONST MaxNeighbors = NUMBER(Compass.Dir) + 2 + OptPts; (* max. no. of neighbors a pt. can have *)

VAR DoDebug := Debug.DebugThis("Step");

(* return value is new idx *)
(* may change stateArr even if it fails *)

(*
   This routine generates candidate movement points in the plane to get
   from GridPoint.T from to its NEWS neighbors.

   There is no guarantee that it is in fact OK to move to the neighbors.
*)
REVEAL
  Iterator = PublicIter BRANDED OBJECT 
    array : ARRAY [0..MaxNeighbors-1] OF T;
    i, n : INTEGER := -1;
  OVERRIDES
    next := IterNext;
  END;
        
PROCEDURE IterNext(self : Iterator; VAR s : T) : BOOLEAN =
  BEGIN
    IF self.i = -1 THEN 
      <* ASSERT FALSE *> 
    ELSIF self.i = self.n THEN
      RETURN FALSE
    ELSE
      s := self.array[self.i];
      INC(self.i);
      RETURN TRUE
    END
  END IterNext;

PROCEDURE Neighbors(READONLY p : GridPoint.T; 
                    READONLY dst : GridPoint.T; 
                    VAR iter : Iterator) =

  (* cost between neighbors --- this routine is only used vertically now *)
  PROCEDURE SingleCost(from, to : GridPoint.T) : INTEGER =
    BEGIN

      WITH deltax = ABS(from.x - to.x), 
           deltay = ABS(from.y - to.y), 
           deltal = ABS(from.l - to.l) DO
        
        <* ASSERT deltax <= 1 *>
        <* ASSERT deltay <= 1 *>
        <* ASSERT deltal <= 1 *>
        <* ASSERT deltax + deltay + deltal = 1 *>
      END;

      IF from.x # to.x THEN
        RETURN costs[from.l].xCost
      ELSIF from.y # to.y THEN
        RETURN costs[from.l].yCost
      ELSIF to.l > from.l THEN
        RETURN costs[from.l].upCost
      ELSIF to.l < from.l THEN
        RETURN costs[to.l].upCost
      ELSE
        <* ASSERT FALSE *>
      END
    END SingleCost;

  PROCEDURE SingleCostInPlane(from : GridPoint.T; dir : Compass.Dir) : INTEGER =
    BEGIN
      WITH l = from.l DO
        CASE dir OF 
        | Compass.Dir.N, Compass.Dir.S => RETURN costs[l].yCost
        | Compass.Dir.E, Compass.Dir.W => RETURN costs[l].xCost
        END
      END
    END SingleCostInPlane;

  PROCEDURE MoveInPlane(READONLY from : GridPoint.T; 
                        dir: Compass.Dir; 
                        VAR stateArr : ARRAY OF T;
                        idx : CARDINAL) : CARDINAL =
    VAR
      stepCost := SingleCostInPlane(from,dir);
      to := from;
    BEGIN
      WITH res = stateArr[idx] DO
        res.p := to;
        INC(res.p.x, Compass.Step[dir].x);
        INC(res.p.y, Compass.Step[dir].y);
        res.prev := from;
        res.cost := stepCost;
        INC(idx)
      END;
      RETURN idx (* either idx' or idx' + 1 or idx' + 2 *)
    END MoveInPlane;

  VAR 
    costs := Cost.costs();
  VAR
    h := p.x; v := p.y; l := p.l;
    idx := 0;
    jdx : CARDINAL;
    res : ARRAY [0..MaxNeighbors-1] OF T;
  BEGIN
    IF iter = NIL THEN iter := NEW(Iterator) END;

    IF DoDebug THEN
      Debug.Out("Step.Neighbors( " & GridPoint.Format(p) & " )")
    END;

    (* find neighboring points and costs of moving *)
    (* these are the basic ones -- if we overstep boundaries or *)
    (* need to rip up, that comes later *)
    
    FOR i := FIRST(Compass.Dir) TO LAST(Compass.Dir) DO
      idx := MoveInPlane(p, i, res, idx)
    END;

    IF DoDebug THEN
      Debug.Out("Neighbors: idx = " & Fmt.Int(idx))
    END;

    (* vertical steps *)
    
    IF l # FIRST(Layer) THEN
      res[idx].p := 
          GridPoint.T { h, v, l - 1 }; (* down *)
      res[idx].cost := SingleCost(p,res[idx].p);
      res[idx].prev := p;
      INC(idx)
    END;

    IF l # LAST(Layer) THEN
      res[idx].p := 
          GridPoint.T { h, v, l + 1 };  (* up *)
      res[idx].cost := SingleCost(p,res[idx].p);
      res[idx].prev := p;
      INC(idx)
    END;

    (* the following garbage doesn't really work... *)

    IF OptPts > 0 THEN
      VAR
        o := 2;
        next, prev : GridPoint.T;
        xstep := Integer.Compare(dst.x, p.x); 
        ystep := Integer.Compare(dst.y, p.y);       
      BEGIN
        IF    costs[l].xCost = Cost.bestXcost AND xstep # 0 THEN
          WHILE o <= OptPts + 1 DO 
            next := GridPoint.T { h + o * xstep, v, l };
            prev := next; prev.x := prev.x - xstep;
            IF NOT Filled.Marked(prev) THEN
              res[idx].p := next;
              res[idx].cost := SingleCost(prev, res[idx].p);
              res[idx].prev := prev;
              INC(idx)
            ELSE
              IF o > 1 THEN DEC(idx) END;
              EXIT
            END;
            INC(o)
          END 
        ELSIF costs[l].yCost = Cost.bestYcost AND ystep # 0 THEN
          WHILE o <= OptPts + 1 DO 
            next := GridPoint.T { h, v + o * ystep, l };
            prev := next; prev.y := prev.y - ystep;
            IF NOT Filled.Marked(prev) THEN
              res[idx].p := next;
              res[idx].cost := SingleCost(prev, res[idx].p);
              res[idx].prev := prev;
              INC(idx)
            ELSE
              IF o > 1 THEN DEC(idx) END;
              EXIT
            END;
            INC(o)
          END 
        END
      END
    END;


    jdx := idx;

    (*************** DONE MODIFYING idx ***************)

    FOR i := 0 TO idx - 1 DO

      (* this needs to get a bit trickier if we are expanding the
         current search cell ... *)

      (* we also need to make sure that we don't under-cut the CURRENT
         search path.. *)
      BEGIN
        IF res[i].cost >= Cost.Infty THEN 
          DEC(jdx)

     (* ELSE SKIP --- all other cases *)
        END
      END
    END;

    (* XXX we no longer fiddle with internals of res[] below this *)

    (* re-alloc in proper sized array *)

    VAR
      j := 0;
    BEGIN
      FOR i := 0 TO idx - 1 DO
        IF res[i].cost # Cost.Infty THEN
          iter.array[j] := res[i];
          INC(j)
        END
      END;
      <* ASSERT j = jdx *>
      iter.n := j;
      iter.i := 0;
      IF DoDebug THEN
        Debug.Out("Step.Neighbors: " & GridPoint.Format(p) & ":");
        FOR i := 0 TO iter.n - 1 DO
          Debug.Out("Neighbor " & Fmt.Int(i) & ": " & 
            GridPoint.Format(iter.array[i].p) & 
            " cost = " & Fmt.Int(iter.array[i].cost))
        END
      END;
      RETURN
    END
  END Neighbors;

BEGIN END SimpleStep.

