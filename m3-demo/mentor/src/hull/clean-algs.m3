(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE HullAlgs;

TYPE Point = RECORD x, y: INTEGER END;

VAR p: REF ARRAY OF Point;
    N: INTEGER;
(* The input points are p[1] through p[N], where N >= 1.  
   We are allowed to use p[0] to hold a sentinel.

   Our task is to compute M, the number of vertices of the convex 
   hull, and to permute the array p[1..N] to make p[1..M] be the hull 
   vertices in counterclockwise order, starting somewhere. 

   The input may include pairs of coincident points and triples of 
   collinear points.  Of two points that coincide, at most one is 
   a hull vertex.  Of three distinct, collinear points, the middle 
   one is never a hull vertex. 
   
   If the input points all coincide, M is 1.  If the input points 
   are all collinear but do not all coincide, M is 2. *)   

TYPE AB = {Above, Below,  Left, Right,  On};
PROCEDURE TestAB(old, new: Point): AB =
(* Classify the point "new" with respect to a horizontal line
   through the point "old". *)
  BEGIN
    IF    new.y < old.y THEN RETURN AB.Below
    ELSIF new.y > old.y THEN RETURN AB.Above
    ELSIF new.x < old.x THEN RETURN AB.Left
    ELSIF new.x > old.x THEN RETURN AB.Right
    ELSE RETURN AB.On
    END;
  END TestAB;

                                 (* The three points:           *)
TYPE LR = {Left, Right,          (*  form a triangle.           *)
           Back, Shaft, Front,   (*  collinear, but distinct.   *) 
           Tail, Head, DegenOff, (*  2 coincide, but not all 3. *)
           DegenOn};             (*  all 3 points coincide.     *)
PROCEDURE TestLR(tail, head, new: Point): LR =
(* Compute the relative orientation of a triple of points, from among 
   the nine possibilities.  In the seven cases where "tail" and "head" 
   do not coincide, we classify "new" with respect to a vector from 
   "tail" to "head".  In the remaining two cases, we merely check 
   whether or not "new" coincides with the common value of "head" 
   and "tail". *) 
  VAR area: INTEGER :=   (head.x - tail.x) * (new.y - tail.y)
                       - (head.y - tail.y) * (new.x - tail.x);
      (* The signed area of the parallelogram spanned by the vectors 
         from "tail" to "head" and from "tail" to "new";  you can 
         think of it either as a 3-by-3 determinant or as the norm 
         of a vector cross product. *) 
  BEGIN
    IF    area > 0 THEN RETURN LR.Left
    ELSIF area < 0 THEN RETURN LR.Right
    ELSE
      VAR (* The following are Manhattan distances. *)
        distTailHead: INTEGER:=ABS(head.x - tail.x) + ABS(head.y - tail.y);
        distTailNew: INTEGER := ABS(new.x - tail.x) + ABS(new.y - tail.y);
        distHeadNew: INTEGER := ABS(new.x - head.x) + ABS(new.y - head.y);
        max: INTEGER := MAX(MAX(distTailHead, distTailNew), distHeadNew);
      BEGIN
        IF    max = 0            THEN RETURN LR.DegenOn
        ELSIF distTailHead = 0   THEN RETURN LR.DegenOff
        ELSIF distTailNew = 0    THEN RETURN LR.Tail
        ELSIF distHeadNew = 0    THEN RETURN LR.Head
        ELSIF max = distTailHead THEN RETURN LR.Shaft
        ELSIF max = distTailNew  THEN RETURN LR.Front
        ELSIF max = distHeadNew  THEN RETURN LR.Back
        END;
      END;
    END;
  END TestLR;

PROCEDURE Swap(VAR p, q: Point) =
  VAR t: Point; BEGIN t := p; p := q; q := t END Swap;
  
PROCEDURE PackageWrap(): INTEGER =
  VAR min: INTEGER;
  BEGIN
    min := 1;
    FOR i := 2 TO N DO
      CASE TestAB(p[min], p[i]) OF
      | AB.Below, AB.Right       => min := i
      | AB.Above, AB.Left, AB.On => 
      END;
    END;
    (* p[min] is one of the rightmost of the lowest points. *)
    FOR M := 1 TO N DO
      Swap(p[min], p[M]);  (* p[1..M] is a prefix of the hull. *)
      min := 1;
      FOR i := M+1 TO N DO
        CASE TestLR(p[M], p[min], p[i]) OF
        | LR.Right, LR.Front, LR.DegenOff                 => min := i
        | LR.Left, LR.Tail, LR.Shaft, LR.Head, LR.DegenOn => 
        END; (* LR.Back never happens;  LR.DegenOn and LR.DegenOff  *)
      END;   (* happen only at the start, when  M = min = 1.        *)
      IF min = 1 THEN RETURN M END;
    END;
    RETURN 0;
  END PackageWrap;

PROCEDURE LessEq(s, t: Point): BOOLEAN =
(* Using polar coordinates centered at p[0], the point "s" is less 
   than or equal to the point "t" when either 
       theta(s) < theta(t)  or 
       theta(s) = theta(t)  and  r(s) <= r(t).
   By fiat, the point p[0], which has r(p[0]) = 0 and theta(p[0]) 
   undefined, is less than any other point. 

   Every point  q  passed to this procedure with  theta(q)  defined,
   that is, with  r(q) > 0, will satisfy
       0 <= theta(q) < 180,
   and the following implementation depends on that.  *)
  BEGIN
    CASE TestLR(p[0], s, t) OF
    | LR.Left, LR.Front, LR.Head  => RETURN TRUE
    | LR.Right, LR.Shaft, LR.Tail => RETURN FALSE
    | LR.DegenOn, LR.DegenOff     => RETURN TRUE
    END; (* LR.Back can't happen. *)
  END LessEq;

PROCEDURE GrahamScan(): INTEGER =
  VAR min, M: INTEGER;
  BEGIN
    min := 1;
    FOR i := 2 TO N DO
      CASE TestAB(p[min], p[i]) OF
      | AB.Below, AB.Left         => min := i
      | AB.Above, AB.Right, AB.On => 
      END;
    END;
    p[0] := p[min]; (* p[0] is a copy of the leftmost of the lowest. *)
    Sort();
    (* The segment p[1..N] consists of one or more copies of p[0] 
       followed by all points distinct from p[0], sorted by theta 
       from p[0] and, for equal theta, sorted by r.  *) 
    M := 1;
    FOR i := 2 TO N DO
      LOOP 
        CASE TestLR(p[M-1], p[M], p[i]) OF
        | LR.Right                      => DEC(M);
        | LR.Left, LR.DegenOff          => EXIT;
        | LR.Front, LR.Head, LR.DegenOn => DEC(M);  EXIT;
        END; (* LR.Back, LR.Tail, and LR.Shaft can't happen; LR.DegenOn *)
      END;   (* and LR.DegenOff happen only at the start, when M = 1.   *)
      INC(M);  Swap(p[i], p[M]);
      (* p[1..M] is the convex hull of p[1..i]. *)
    END;
    RETURN M;
  END GrahamScan;

PROCEDURE Sort() =
(* Sort the array p[1..N] into "LessEq" order.  This implementation
   uses ShellSort. *)
  VAR h: INTEGER := 1;  j: INTEGER;  v: Point;
  BEGIN
    REPEAT h := 3 * h + 1 UNTIL h > N;
    REPEAT
      h := h DIV 3;
      FOR i := h + 1 TO N DO
        v := p[i];  j := i;
        WHILE j - h >= 1 AND NOT LessEq(p[j - h], v) DO
          p[j] := p[j - h];  DEC(j, h);
        END;
        p[j] := v;
      END;
    UNTIL h = 1;
  END Sort;

BEGIN
END HullAlgs.
