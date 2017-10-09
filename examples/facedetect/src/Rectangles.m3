MODULE Rectangles;

IMPORT RefSeq,IntSeq,Image,Word;

TYPE
  TreeNode = RECORD
    parent,rank : INTEGER;
  END;
  RefArrTree = REF ARRAY OF TreeNode;
  
  RefArrRect = REF ARRAY OF Rect;
  RefArrInt = REF ARRAY OF INTEGER;

PROCEDURE Predicate(eps : REAL; READONLY r1,r2 : Rect) : BOOLEAN =
  VAR
    delta : REAL;
    res : BOOLEAN;
  BEGIN
    delta := eps * FLOAT(MIN(r1.width, r2.width) + MIN(r1.height, r2.height)) * 0.5;
    
    res := FLOAT(ABS(r1.x - r2.x)) <= delta AND
           FLOAT(ABS(r1.y - r2.y)) <= delta AND
           FLOAT(ABS(r1.x + r1.width - r2.x - r2.width)) <= delta AND
           FLOAT(ABS(r1.y + r1.height - r2.y - r2.height)) <= delta;
    RETURN res;
  END Predicate;

PROCEDURE Partition(vec : RefSeq.T; VAR labels : IntSeq.T; eps : REAL) : INTEGER =
  VAR
    r1,r2 : RefRect;
    nodes : RefArrTree;
    k,N,root,root2,rank,rank2,parent,nClasses : INTEGER := 0;
    res : BOOLEAN;
  BEGIN
    N := vec.size();
    nodes := NEW(RefArrTree,N*2);

    (* The first O(N) pass: create N single-vertex trees *)
    FOR i := 0 TO N - 1 DO
      nodes[i].parent := -1;
      nodes[i].rank := 0;
    END;
  
    (* The main O(N^2) pass: merge connected components *)
    FOR i := 0 TO N - 1 DO
      root := i;

      (* find root *)
      WHILE nodes[root].parent >= 0 DO
        root := nodes[root].parent;
      END;
      
      FOR j := 0 TO N - 1 DO
        r1 := vec.get(i);
        r2 := vec.get(j);
        res := Predicate(eps, r1^, r2^);
        IF i # j AND res THEN
          root2 := j;
          WHILE nodes[root2].parent >= 0 DO
            root2 := nodes[root2].parent;  
          END;
	  IF root2 # root THEN
            (* unite both trees *)
            rank := nodes[root].rank;
            rank2 := nodes[root2].rank;
            
            IF rank > rank2 THEN
              nodes[root2].parent := root;
            ELSE
              nodes[root].parent := root2;
              IF rank = rank2 THEN
                INC(nodes[root2].rank);
              END;
              root := root2;
            END;
            k := j;

            (* compress the path from node2 to root *)
            WHILE nodes[k].parent >= 0 DO
              parent := nodes[k].parent;
              nodes[k].parent := root;
              k := parent;
            END;

            (* compress the path from node to root *)
            k := i;
            WHILE nodes[k].parent >= 0 DO
              parent := nodes[k].parent;
              nodes[k].parent := root;
              k := parent;
            END;
          END;
        END;
      END;
    END;
  
   (* Final O(N) pass: enumerate classes *)
    labels := NEW(IntSeq.T).init();

    FOR i := 0 TO N - 1 DO
      root := i;
      WHILE nodes[root].parent >= 0 DO
        root := nodes[root].parent;
      END;
      (* re-use the rank as the class label *)
      IF nodes[root].rank >= 0 THEN
        INC(nClasses);
        nodes[root].rank := Word.Not(nClasses);
      END;
      labels.addhi(Word.Not(nodes[root].rank));
    END;
    RETURN nClasses;
  END Partition;
  
PROCEDURE GroupRectangles(VAR rectList : RefSeq.T; groupThreshold : INTEGER; eps : REAL) =
  VAR
    labels : IntSeq.T;
    rr : RefRect;
    r,r1,r2 : Rect;
    rrects : RefArrRect;
    rweights : RefArrInt;
    nClasses,nLabels,cls,n1,n2,dx,dy : INTEGER;
    checkAll : BOOLEAN;
    s : REAL;
  BEGIN
  
    IF groupThreshold <= 0 OR rectList.size() = 0 THEN
      RETURN;
    END;

    nClasses := Partition(rectList, labels, eps);
    
    nLabels := labels.size();
    
    rrects := NEW(RefArrRect,nClasses);
    rweights := NEW(RefArrInt,nClasses);
    
    FOR i := 0 TO nLabels - 1 DO
      cls := labels.get(i)-1;
      rr := rectList.get(i);
      INC(rrects[cls].x, rr.x);
      INC(rrects[cls].y, rr.y);
      INC(rrects[cls].width, rr.width);
      INC(rrects[cls].height, rr.height);
      INC(rweights[cls]);
    END;
  
    FOR i := 0 TO nClasses - 1 DO
      r := rrects[i];
      s := 1.0E0 / FLOAT(rweights[i]);
      rrects[i].x := ROUND(FLOAT(r.x) * s);
      rrects[i].y := ROUND(FLOAT(r.y) * s);
      rrects[i].width := ROUND(FLOAT(r.width) * s);
      rrects[i].height := ROUND(FLOAT(r.height) * s);
    END;
  
    rectList := NEW(RefSeq.T).init();
    
    FOR i := 0 TO nClasses - 1 DO
      r1 := rrects[i];
      n1 := rweights[i];
      IF n1 > groupThreshold THEN
        (* filter out small face rectangles inside large rectangles *)
        checkAll := TRUE;
        FOR j := 0 TO nClasses - 1 DO
          n2 := rweights[j];      
          (*********************************
           * if it is the same rectangle, 
           * or the number of rectangles in class j is < group threshold, 
           * do nothing 
           ********************************)
          IF NOT (j = i OR n2 <= groupThreshold) THEN
            r2 := rrects[j];
             
            dx := ROUND( FLOAT(r2.width) * eps );
            dy := ROUND( FLOAT(r2.height) * eps );

            IF i # j AND
              r1.x >= r2.x - dx AND
              r1.y >= r2.y - dy AND
              r1.x + r1.width <= r2.x + r2.width + dx AND
              r1.y + r1.height <= r2.y + r2.height + dy AND
              (n2 > MAX(3, n1) OR n1 < 3) THEN
              checkAll := FALSE;
              EXIT;
            END;
          END;
        END;
        IF checkAll THEN
          rr := NEW(RefRect);
          rr^ := r1;
          rectList.addhi(rr);
        END;
      END;
    END;
  END GroupRectangles;

PROCEDURE DrawRectangle(VAR image : Image.Image; r : Rect) =
  BEGIN
    FOR i := 0 TO r.width - 1 DO
      image.data[r.y, r.x + i] := 255;
    END;
    FOR i := 0 TO r.height - 1 DO
      image.data[r.y + i, r.x + r.width] := 255;
    END;
    FOR i := 0 TO r.width - 1 DO
      image.data[r.y + r.height, r.x + r.width - i] := 255;
    END;
    FOR i := 0 TO r.height - 1 DO
      image.data[r.y + r.height - i, r.x] := 255;
    END;
  END DrawRectangle;

BEGIN
END Rectangles.
