(* $Id$ *)

MODULE FilledWithTables EXPORTS Filled;
IMPORT RouteID;
IMPORT GridPoint, GridPointSet;
IMPORT GridPointList;
FROM GridPointList IMPORT Cons;
IMPORT IntPair AS Pair;
IMPORT FilledArray;
IMPORT Debug;
IMPORT Fmt;
IMPORT Wr, Thread, Rd, Pickle;

VAR DoDebug := Debug.DebugThis("Filled");

REVEAL 
  T = BRANDED "FilledWithTables" OBJECT
    data : ARRAY GridPoint.Layer OF FilledArray.T;
  END;

PROCEDURE New() : T = 
  VAR
    res := NEW(T);
  BEGIN
    FOR l := FIRST(GridPoint.Layer) TO LAST(GridPoint.Layer) DO
      res.data[l] := NEW(FilledArray.T).init();
    END;
    RETURN res
  END New;

PROCEDURE MarkedRouteInternal(p : GridPoint.T; VAR rec : RouteID.T) : BOOLEAN =
  BEGIN RETURN space.data[p.l].get(Pair.T { p.x,p.y }, rec) END MarkedRouteInternal;

PROCEDURE Marked(p : GridPoint.T) : BOOLEAN =
  BEGIN RETURN space.data[p.l].haveDataAt(Pair.T { p.x,p.y }) END Marked;

PROCEDURE MarkedRoute(p : GridPoint.T) : RouteID.T =
  VAR
    x : BOOLEAN;
    rec : RouteID.T;
  BEGIN
    x := MarkedRouteInternal(p, rec);
    <* ASSERT x *>
    RETURN rec
  END MarkedRoute;

PROCEDURE Mark(p : GridPoint.T; path : RouteID.T) =
  VAR
    x : BOOLEAN;
    oldPath : RouteID.T;
  BEGIN
    <* ASSERT path # RouteID.Nil *>

    x := space.data[p.l].get(Pair.T { p.x,p.y }, oldPath);

    IF DoDebug THEN
      Debug.Out("FilledWithTables.Mark: marking " & GridPoint.Format(p) & 
        " with path=" & RouteID.Format(path) & " marked=" & Fmt.Bool(x) & 
        " oldPath=" & 
        ARRAY BOOLEAN OF TEXT { "NIL", RouteID.Format(oldPath) }[x])
    END;

    <* ASSERT NOT x OR oldPath = path *>

    EVAL space.data[p.l].put(Pair.T { p.x,p.y }, path)

  END Mark;

PROCEDURE MarkAsRigid(p : GridPoint.T) =
  CONST
    path = RouteID.Nil;
  VAR
    x : BOOLEAN;
  BEGIN
    IF DoDebug THEN
      Debug.Out("FilledWithTables.MarkAsRigid: marking " & GridPoint.Format(p) & 
        " with path=" & RouteID.Format(path))
    END;

    x := space.data[p.l].put(Pair.T { p.x,p.y }, path);
    <* ASSERT NOT x *>

  END MarkAsRigid;

PROCEDURE UnMark(p : GridPoint.T) =
  VAR 
    x : BOOLEAN;
    path : RouteID.T;
  BEGIN
    IF DoDebug THEN
      Debug.Out("FilledWithTables.Mark: unmarking " & GridPoint.Format(p))
    END;
    x := space.data[p.l].delete(Pair.T { p.x,p.y }, path);
    <* ASSERT x *>
  END UnMark;

PROCEDURE IntervalToGridPointList(from, to : GridPoint.T) : GridPointList.T =
  VAR
    res : GridPointList.T := NIL;
  BEGIN
    <* ASSERT from.x = to.x OR from.y = to.y *>
    IF from.l # to.l THEN
      <* ASSERT from.x = to.x AND from.y = to.y *>
      FOR k := MIN(from.l,to.l) TO MAX(from.l,to.l) DO
        res := Cons(GridPoint.T { from.x, from.y, k }, res) 
      END
    ELSIF from.x # to.x THEN        
      FOR i := MIN(from.x,to.x) TO MAX(from.x,to.x) DO
        res := Cons(GridPoint.T { i, from.y, from.l }, res)
      END
    ELSIF from.y # to.y THEN      
      FOR j := MIN(from.y,to.y) TO MAX(from.y,to.y) DO
        res := Cons(GridPoint.T { from.x, j, from.l }, res)
      END
    ELSE
      <* ASSERT from = to *>
      res := GridPointList.List1(from)
    END;
    RETURN res
  END IntervalToGridPointList;

PROCEDURE MarkSegment(from, to : GridPoint.T; path : RouteID.T;
                      nonObstacles : GridPointSet.T) =
  VAR
    list := IntervalToGridPointList(from,to);
  BEGIN
    WHILE list # NIL DO 
      IF NOT (nonObstacles.member(list.head) AND Marked(list.head)) THEN
        Mark(list.head,path)
      END;
      list := list.tail 
    END
  END MarkSegment;

PROCEDURE UnMarkSegment(from, to : GridPoint.T) =
  VAR
    list := IntervalToGridPointList(from,to);
  BEGIN
    WHILE list # NIL DO UnMark(list.head); list := list.tail END
  END UnMarkSegment;

PROCEDURE UnMarkSegmentWhereMarked(from, to : GridPoint.T) =
  VAR
    list := IntervalToGridPointList(from,to);
  BEGIN
    WHILE list # NIL DO 
      IF Marked(list.head) THEN
        UnMark(list.head)
      END; 
      list := list.tail 
    END
  END UnMarkSegmentWhereMarked;

PROCEDURE DumpToWriter(wr : Wr.T) RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    Wr.PutText(wr, "BEGINFILLED\n");
    FOR i := FIRST(GridPoint.Layer) TO LAST(GridPoint.Layer) DO
      Wr.PutText(wr, "BEGINLAYER " & Fmt.Int(i) & "\n");
      VAR
        iter := space.data[i].iterate();
        p : Pair.T;
        id : RouteID.T;
      BEGIN
        WHILE iter.next(p,id) DO
          Wr.PutText(wr, Fmt.Int(p.k1));
          Wr.PutChar(wr, ',');
          Wr.PutText(wr, Fmt.Int(p.k2));
          Wr.PutChar(wr, '\n');
        END
      END;
      Wr.PutText(wr, "ENDLAYER " & Fmt.Int(i) & "\n")
    END;
    Wr.PutText(wr, "ENDFILLED\n");
    
  END DumpToWriter;

PROCEDURE PickleToWriter(wr : Wr.T) RAISES { Wr.Failure, Thread.Alerted, Pickle.Error } =
  BEGIN
    Pickle.Write(wr, space)
  END PickleToWriter;

PROCEDURE UnPickleFromReader(rd : Rd.T) RAISES { Rd.Failure, Thread.Alerted,
                                                 Rd.EndOfFile, Pickle.Error } =
  BEGIN
    space := Pickle.Read(rd)
  END UnPickleFromReader;

VAR 
  space : T;
BEGIN 
  space := New()
END FilledWithTables.
