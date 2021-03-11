(* $Id$ *)

MODULE GridPointStatus;
IMPORT GridPoint, Word, GridPointSet, GridPointSetDef;
IMPORT RouteTag;

REVEAL
  T = Public BRANDED Brand OBJECT
    me : GridPoint.T;
    n : GridPointSetDef.T;
    isEnd : BOOLEAN;
    tag : RouteTag.T;
  OVERRIDES
    init := Init;
    routeNeighbors := GetRouteNeighbors;
    addANeighbor := AddANeighbor;
    delANeighbor := DelANeighbor;
    size := Size;
    getTag := GetTag;
  END;

PROCEDURE GetTag(a : T) : RouteTag.T = BEGIN RETURN a.tag END GetTag;

PROCEDURE Equal(a,b : T) : BOOLEAN = BEGIN RETURN a = b END Equal;

PROCEDURE Hash(a : T) : Word.T = BEGIN RETURN GridPoint.Hash(a.me) END Hash;

PROCEDURE Init(a : T; READONLY me : GridPoint.T; isEnd : BOOLEAN;
               tag : RouteTag.T) : T = 
  BEGIN 
    a.me := me; 
    a.n := NEW(GridPointSetDef.T).init();
    a.isEnd := isEnd;
    a.tag := tag;
    RETURN a 
  END Init;

PROCEDURE Size(self : T) : CARDINAL = BEGIN RETURN self.n.size() END Size;

PROCEDURE AddANeighbor(a : T; READONLY p : GridPoint.T) =
  BEGIN
    <* ASSERT GridPoint.AreNeighbors(a.me,p) *>
    <* ASSERT NOT a.routeNeighbors().member(p) *>
    EVAL a.routeNeighbors().insert(p)
  END AddANeighbor;

PROCEDURE DelANeighbor(a : T; READONLY p : GridPoint.T) =
  BEGIN
    <* ASSERT GridPoint.AreNeighbors(a.me,p) *>
    <* ASSERT a.routeNeighbors().member(p) *>
    EVAL a.routeNeighbors().delete(p)
  END DelANeighbor;

PROCEDURE GetRouteNeighbors(a : T) : GridPointSet.T =
  BEGIN RETURN a.n END GetRouteNeighbors;

BEGIN END GridPointStatus.
