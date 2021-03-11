(* $Id$ *)

MODULE SimpleGrid;
IMPORT MagPoint;
IMPORT PointTypeTbl;
IMPORT MagPointDist;
IMPORT MagPointPQ, MagPointList;
IMPORT MagRect;
IMPORT MagPointIntTbl;
IMPORT TextWr, Wr, Thread;
IMPORT MagRectList;
IMPORT Debug;
IMPORT RefList;

VAR DoDebug := Debug.DebugThis("SIMPLEGRID");

TYPE
  Offset = ARRAY Dir OF [-1..1];

CONST
  XOff = Offset {  0,  1, -1,  0 };
  YOff = Offset {  1,  0,  0, -1 };

TYPE
  Private = T OBJECT
    clip : MagRect.T;
    minx, miny := LAST(INTEGER);
    maxx, maxy := FIRST(INTEGER);
  METHODS
    get(p : MagPoint.T) : PointType;
    set(p : MagPoint.T; t : PointType) := Set;
    setNoClip(p : MagPoint.T; t : PointType);
  OVERRIDES    
    drawRect      :=    DrawRect;
    drawPointList :=    DrawPointList;
    bloat         :=    Bloat2;
    shrink        :=    Shrink;
    route         :=    Route;
    isCovered     :=    IsCovered;
    rectify       :=    Rectify;
    format        :=    Format;
    touches       :=    Touches;
  END;

(************************* array-based impl *************************)

TYPE
  Array = REF ARRAY OF ARRAY OF PointType;

REVEAL
  ArrImpl = Private BRANDED "ArrImpl of " & Brand OBJECT 
    array : Array; 
    defaultType : PointType;
  OVERRIDES
    copy          :=    ArrCopy;
    get           :=    ArrGet;
    setNoClip     :=    ArrSetNoClip;
    init          :=    ArrInit;
    zap           :=    ArrZap;
    copyPointsFrom:=    ArrCopyPointsFrom;
  END;

PROCEDURE ArrZap(self : ArrImpl; from, to : PointType) =
  BEGIN
    FOR i := FIRST(self.array^) TO LAST(self.array^) DO
      FOR j := FIRST(self.array[0]) TO LAST(self.array[0]) DO
        IF self.array[i,j] = from THEN self.array[i,j] := to END
      END
    END
  END ArrZap;

PROCEDURE ArrCopyPointsFrom(self : ArrImpl; grid : T; types : TypeSet) =
  VAR
    o := NARROW(grid, ArrImpl);
  BEGIN
    <* ASSERT self.clip = o.clip *>
    FOR i := FIRST(self.array^) TO LAST(self.array^) DO
      FOR j := FIRST(self.array[0]) TO LAST(self.array[0]) DO
        IF o.array[i,j] IN types THEN self.array[i,j] := o.array[i,j] END
      END
    END
  END ArrCopyPointsFrom;

PROCEDURE ArrInit(self : ArrImpl; clip : MagRect.T; initType : PointType) : T =
  VAR
    xsiz := clip.ur.x - clip.ll.x + 1;
    ysiz := clip.ur.y - clip.ll.y + 1;
  BEGIN 
    self.defaultType := initType;
    self.clip := clip;
    VAR
      fromCache : Array := NIL;
      p : RefList.T;
      q : RefList.T;
    BEGIN
      LOCK rMu DO
        p := recycleBin;
        WHILE p # NIL DO
          IF NUMBER(NARROW(p.head,Array)^) = xsiz AND 
             NUMBER(NARROW(p.head,Array)[0]) = ysiz THEN

            fromCache := p.head;
            recycleBin := RefList.AppendD(q,p.tail);
            DEC(rbCount);
            EXIT
          ELSE
            q := RefList.Cons(p.head,q)
          END;
          p := p.tail
        END
      END;
      IF fromCache # NIL  THEN
        self.array := fromCache
      ELSE
        self.array := NEW(Array, xsiz, ysiz)
      END
    END;
    FOR i := FIRST(self.array^) TO LAST(self.array^) DO
      FOR j := FIRST(self.array[0]) TO LAST(self.array[0]) DO
        self.array[i,j] := initType
      END
    END;
    RETURN self 
  END ArrInit;

PROCEDURE ArrGet(self : ArrImpl; p : MagPoint.T) : PointType =
  BEGIN 
    IF NOT Inside(p,self.clip) THEN RETURN self.defaultType END;

    RETURN self.array[p.x - self.clip.ll.x, p.y - self.clip.ll.y] 
  END ArrGet;

PROCEDURE ArrCopy(self : ArrImpl) : T =
  VAR 
    res : ArrImpl := NEW(ArrImpl).init(self.clip);
  BEGIN
    res.minx := self.minx;
    res.miny := self.miny;
    res.maxx := self.maxx;
    res.maxy := self.maxy;
    res.array^ := self.array^;
    RETURN res
  END ArrCopy;

PROCEDURE ArrSetNoClip(self : ArrImpl; p : MagPoint.T; t : PointType) =
  BEGIN 
    self.array[p.x - self.clip.ll.x, p.y - self.clip.ll.y] := t 
  END ArrSetNoClip;

(************************* table-based impl *************************)

REVEAL
  TblImpl = Private BRANDED "TblImpl of " & Brand OBJECT 
    tbl : PointTypeTbl.T;
  OVERRIDES
    copy          :=    TblCopy;
    get           :=    TblGet;
    setNoClip     :=    TblSetNoClip;
    init          :=    TblInit;
  END;

PROCEDURE TblInit(self : TblImpl; clip : MagRect.T; initType : PointType) : T =
  BEGIN 
    <* ASSERT initType = PointType.Space *> (* dumb, but what can you do? *)
    self.clip := clip;
    self.tbl := NEW(PointTypeTbl.Default).init(200); 
    RETURN self 
  END TblInit;

PROCEDURE TblGet(self : TblImpl; p : MagPoint.T) : PointType =
  VAR 
    res := PointType.Space;
  BEGIN
    EVAL self.tbl.get(p,res);
    RETURN res
  END TblGet;

PROCEDURE TblCopy(self : TblImpl) : T =
  VAR 
    res : TblImpl := NEW(TblImpl).init(self.clip);
    iter := self.tbl.iterate();
    p : MagPoint.T;
    t : PointType;
  BEGIN
    res.minx := self.minx;
    res.miny := self.miny;
    res.maxx := self.maxx;
    res.maxy := self.maxy;
    WHILE iter.next(p, t) DO EVAL res.tbl.put(p,t) END;
    RETURN res
  END TblCopy;

PROCEDURE TblSetNoClip(self : TblImpl; p : MagPoint.T; t : PointType) =
  BEGIN EVAL self.tbl.put(p,t) END TblSetNoClip;

(************************* clients below *************************)

PROCEDURE Inside(READONLY p : MagPoint.T; READONLY clip : MagRect.T) :BOOLEAN=
  BEGIN 
    RETURN p.x >= clip.ll.x AND p.x <= clip.ur.x AND 
           p.y >= clip.ll.y AND p.y <= clip.ur.y 
  END Inside;

PROCEDURE Set(self : Private; p : MagPoint.T; t : PointType) =
  BEGIN
    IF NOT Inside(p,self.clip) THEN RETURN END;

    self.minx := MIN(self.minx,p.x);
    self.maxx := MAX(self.maxx,p.x);

    self.miny := MIN(self.miny,p.y);
    self.maxy := MAX(self.maxy,p.y);

    self.setNoClip(p,t) 
  END Set;

PROCEDURE DrawRect(self : Private; type : PointType; rect : MagRect.T) =
  BEGIN
    IF MagRect.Clip(rect, self.clip, rect) THEN
      FOR i := rect.ll.x TO rect.ur.x - 1 DO
        FOR j := rect.ll.y TO rect.ur.y - 1 DO
          self.set(MagPoint.T { i, j } , type)
        END
      END
    END;
    
    IF DoDebug THEN 
      Debug.Out("SimpleGrid.DrawRect: grid:\n" & self.format()) 
    END

  END DrawRect;

PROCEDURE DrawPointList(s : Private; t : PointType; l : MagPointList.T) =
  BEGIN WHILE l # NIL DO s.set(l.head,t); l := l.tail END END DrawPointList;

PROCEDURE Bloat(self : Private; type : PointType; dir : Dir; by : CARDINAL) =

  PROCEDURE DoIt(i, j : INTEGER) =
    VAR
      thisp := MagPoint.T { i, j };
      neigh := MagPoint.T { i + xoff, j + yoff };
    BEGIN
      IF self.get(thisp) = type (*AND self.get(neigh) = PointType.Space*) THEN
        self.set(neigh,type)
      END
    END DoIt;

  VAR
    xoff := XOff[dir]; 
    yoff := YOff[dir];
  BEGIN
    IF by = 0 THEN 
      IF DoDebug THEN 
        Debug.Out("SimpleGrid.Bloat: grid:\n" & self.format()) 
      END;
      RETURN
    END;

    IF xoff + yoff < 0 THEN
      FOR i := self.minx TO self.maxx DO
        FOR j := self.miny TO self.maxy DO
          DoIt(i,j)
        END
      END
    ELSE
      FOR i := self.maxx TO self.minx BY -1 DO
        FOR j := self.maxy TO self.miny BY -1 DO
          DoIt(i,j)
        END
      END
    END;
    Bloat(self, type, dir, by - 1)
  END Bloat;

PROCEDURE Bloat2(self : Private; type : PointType; dir : Dir; by : CARDINAL) =

  PROCEDURE DoIt(i, j : INTEGER) =
    VAR
      thisp := MagPoint.T { i, j };
      neigh := thisp;
    BEGIN
      IF self.get(thisp) = type THEN
        FOR s := 1 TO by DO
          neigh := MagPoint.T { neigh.x + xoff, neigh.y + yoff };
          IF self.get(neigh) = type THEN 
            EXIT
          ELSE
            self.set(neigh,type)
          END
        END
      END
    END DoIt;

  VAR
    xoff := XOff[dir]; 
    yoff := YOff[dir];
  BEGIN
    IF by = 0 THEN 
      IF DoDebug THEN 
        Debug.Out("SimpleGrid.Bloat: grid:\n" & self.format()) 
      END;
      RETURN
    END;

    IF xoff + yoff < 0 THEN
      FOR i := self.minx TO self.maxx DO
        FOR j := self.miny TO self.maxy DO
          DoIt(i,j)
        END
      END
    ELSE
      FOR i := self.maxx TO self.minx BY -1 DO
        FOR j := self.maxy TO self.miny BY -1 DO
          DoIt(i,j)
        END
      END
    END
  END Bloat2;

PROCEDURE Shrink(self : Private; type : PointType; dir : Dir; by : CARDINAL) =

  PROCEDURE DoIt(i, j : INTEGER) =
    VAR
      thisp := MagPoint.T { i, j };
      neigh := MagPoint.T { i + xoff, j + yoff };
    BEGIN
      IF self.get(thisp) = type AND self.get(neigh) # type THEN
        self.set(thisp,PointType.Space)
      END
    END DoIt;

  VAR
    xoff := XOff[dir]; 
    yoff := YOff[dir];
  BEGIN
    IF by = 0 THEN       
      IF DoDebug THEN 
        Debug.Out("SimpleGrid.Shrink: grid:\n" & self.format()) 
      END;
      RETURN 
    END;
    IF xoff + yoff > 0 THEN
      FOR i := self.minx TO self.maxx DO
        FOR j := self.miny TO self.maxy DO
          DoIt(i,j)
        END
      END
    ELSE
      FOR i := self.maxx TO self.minx BY -1 DO
        FOR j := self.maxy TO self.miny BY -1 DO
          DoIt(i,j)
        END
      END
    END;
    Shrink(self, type, dir, by - 1)
  END Shrink;

PROCEDURE Neighbors(READONLY p : MagPoint.T) : ARRAY Dir OF MagPoint.T =
  VAR
    res : ARRAY Dir OF MagPoint.T;
  BEGIN 
    FOR i := FIRST(Dir) TO LAST(Dir) DO
      res[i] := MagPoint.T { p.x + XOff[i] , p.y + YOff[i] }
    END;
    RETURN res
  END Neighbors;
    
PROCEDURE Route(self : Private; 
                READONLY src, llBound, urBound : MagPoint.T; 
                tgt : PointType; 
                VAR path : MagPointList.T) : BOOLEAN =
  VAR
    dist := NEW(MagPointIntTbl.Default).init();
    pq := NEW(MagPointPQ.Default).init();
  BEGIN
    IF DoDebug THEN
      Debug.Out("SimpleGrid.Route: grid:\n" & self.format())
    END;

    (* clear source... why? *)
    self.set(src,PointType.Space);

    pq.insert(NEW(MagPointPQ.Elt, priority := MagPointDist.T { src, 0 }));

    TRY
      LOOP
        VAR 
          next := pq.deleteMin().priority;
          old : INTEGER;
        BEGIN
          IF self.get(next.p) = tgt THEN 
            (* done *)
            path := MagPointList.List1(next.p);
            EXIT
          END; 
          
          (* if we didn't visit before, 
             or if we got there a longer way before.. *)
          IF NOT dist.get(next.p, old) OR old > next.dist THEN
            EVAL dist.put(next.p,next.dist);
            VAR
              nc := next.dist + 1;
              oc : INTEGER;
              neighs := Neighbors(next.p);
            BEGIN
              (* not done, put on neighbors *)
              FOR i := FIRST(neighs) TO LAST(neighs) DO
                WITH n = neighs[i] DO
                  IF n.x <= urBound.x AND n.x >= llBound.x AND
                     n.y <= urBound.y AND n.y >= llBound.y AND
                     self.get(n) # PointType.Obstacle THEN
                    IF NOT dist.get(n, oc) OR nc < oc THEN
                      pq.insert(NEW(MagPointPQ.Elt, 
                                    priority := MagPointDist.T { n, nc }))
                    END
                  END
                END
              END
            END
          END
        END
      END
    EXCEPT
      MagPointPQ.Empty => RETURN FALSE
    END;

    (* got a path, figure it out *)
    WHILE path.head # src DO
      VAR
        neighs := Neighbors(path.head);
        minNeigh : MagPoint.T;
        cost : INTEGER;
        minCost := LAST(CARDINAL);
      BEGIN
        FOR i := FIRST(neighs) TO LAST(neighs) DO
          IF dist.get(neighs[i], cost) AND cost < minCost THEN
            minNeigh := neighs[i]; minCost := cost
          END
        END;
        path := MagPointList.Cons(minNeigh, path)
      END
    END;

    <* ASSERT path.head = src *>

    IF DoDebug THEN
      Debug.Out("SimpleGrid.Route: Route done: grid:\n" & FormatR(self,path))
    END;

    RETURN TRUE
  END Route;

PROCEDURE IsCovered(self : Private; type : PointType; rect : MagRect.T) : BOOLEAN =
  VAR
    clipped : MagRect.T;
  BEGIN
    IF NOT MagRect.Clip(rect, self.clip, clipped) OR clipped # rect THEN
      RETURN FALSE
    END;
    
    FOR i := rect.ll.x TO rect.ur.x - 1 DO
      FOR j := rect.ll.y TO rect.ur.y - 1 DO
        IF self.get(MagPoint.T { i, j }) # type THEN RETURN FALSE END
      END
    END;
    RETURN TRUE
  END IsCovered;

PROCEDURE Touches(self : Private; type : PointType; rect : MagRect.T) : BOOLEAN =
  BEGIN
    FOR i := rect.ll.x TO rect.ur.x - 1 DO
      FOR j := rect.ll.y TO rect.ur.y - 1 DO
        IF self.get(MagPoint.T { i, j }) = type THEN RETURN TRUE END
      END
    END;
    RETURN FALSE
  END Touches;

PROCEDURE Rectify(self : Private; type : PointType) : MagRectList.T =

  (* find the first included point of the type in the grid *)
  PROCEDURE FindFirst() : BOOLEAN =
    BEGIN
      FOR i := MAX(self.clip.ll.x, self.minx) TO 
               MIN(self.clip.ur.x - 1, self.maxx) DO
        FOR j := MAX(self.clip.ll.y, self.miny) TO 
                 MIN(self.clip.ur.y - 1, self.maxy) DO
          ll :=  MagPoint.T { i, j };
          IF self.get(ll) = type THEN RETURN TRUE END
        END
      END;
      RETURN FALSE
    END FindFirst;

  PROCEDURE Rect() : MagRect.T = BEGIN RETURN MagRect.T { ll, ur } END Rect;

  VAR
    ll, ur : MagPoint.T; (* ll, ur corners *)
    res : MagRectList.T := NIL;
  BEGIN
    WHILE FindFirst() DO
      ur := MagPoint.T { ll.x + 1, ll.y + 1 };
      <* ASSERT self.isCovered(type, Rect()) *>
      WHILE self.isCovered(type, Rect()) AND ur.x <= self.clip.ur.x DO 
        INC(ur.x) 
      END;
      DEC(ur.x);

      WHILE self.isCovered(type, Rect()) AND ur.y <= self.clip.ur.y DO 
        INC(ur.y) 
      END;
      DEC(ur.y);
      
      <* ASSERT self.isCovered(type,Rect()) *>
      res := MagRectList.Cons(Rect(), res);
      self.drawRect(PointType.Space,Rect())

    END;
    RETURN res
  END Rectify;
  
PROCEDURE Format(self : Private) : TEXT =
  <* FATAL Wr.Failure, Thread.Alerted *>
  CONST
    F =  ARRAY PointType OF CHAR { 'o', 't', 's' };
    O =  ARRAY PointType OF CHAR { 'O', 'T', 'S' };
  VAR
    wr := TextWr.New();
  BEGIN
    FOR j := MAX(0,self.maxy) TO MIN(0,self.miny) BY -1 DO
      FOR i := MIN(0,self.minx) TO MAX(0,self.maxx) DO
        WITH t = self.get(MagPoint.T { i,j }) DO
          IF i = 0 AND j = 0 THEN
            Wr.PutChar(wr, O[t])
          ELSE
            Wr.PutChar(wr, F[t])
          END
        END
      END;
      Wr.PutChar(wr, '\n')
    END;
    RETURN TextWr.ToText(wr)
  END Format;

  
PROCEDURE FormatR(self : Private; path : MagPointList.T) : TEXT =
  <* FATAL Wr.Failure, Thread.Alerted *>
  CONST
    F =  ARRAY PointType OF CHAR { 'o', 't', 's' };
    O =  ARRAY PointType OF CHAR { 'O', 'T', 'S' };
  VAR
    wr := TextWr.New();
  BEGIN
    FOR j := self.maxy TO self.miny BY -1 DO
      FOR i := self.minx TO self.maxx DO
        VAR
          p := MagPoint.T { i, j };
        BEGIN
          IF MagPointList.Member(path,p) THEN
            IF p = path.head THEN 
              Wr.PutChar(wr, 'R') 
            ELSE 
              Wr.PutChar(wr,'r') 
            END
          ELSE
            WITH t = self.get(p) DO
              IF i = 0 AND j = 0 THEN
                Wr.PutChar(wr, O[t])
              ELSE
                Wr.PutChar(wr, F[t])
              END
            END
          END
        END
      END;
      Wr.PutChar(wr, '\n')
    END;
    RETURN TextWr.ToText(wr)
  END FormatR;

PROCEDURE FormatDir(dir : Dir; longForm : BOOLEAN) : TEXT =
  CONST
    ShortNames = ARRAY Dir OF TEXT { "N", "E", "W", "S" };
    LongNames = ARRAY Dir OF TEXT { "NORTH", "EAST", "WEST", "SOUTH" };
  BEGIN
    IF longForm THEN 
      RETURN LongNames[dir]
    ELSE
      RETURN ShortNames[dir]
    END
  END FormatDir;

VAR 
  rMu := NEW(MUTEX);
  recycleBin : RefList.T := NIL;
  rbCount := 0;
CONST 
  MaxCount = 100;

PROCEDURE Recycle(stale : T) =
  BEGIN
    LOCK rMu DO
      IF rbCount < MaxCount AND ISTYPE(stale,ArrImpl) THEN
        recycleBin := RefList.Cons(NARROW(stale,ArrImpl).array, recycleBin);
        INC(rbCount)
      END
    END
  END Recycle;

BEGIN END SimpleGrid.
