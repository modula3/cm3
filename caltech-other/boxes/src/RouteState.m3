(* $Id$ *)

MODULE RouteState;

IMPORT RouteID, RouteIDSetDef;
IMPORT GridPointSet, GridPointSetDef, GridPoint;

(* we can modify the state by ripping up a route *)

TYPE
  Type = { Root, (* No-op type, a placeholder *)
           RipUp 
  };

REVEAL
  T = Public BRANDED "RouteState" OBJECT
    up : T := NIL; (* previous route state *)
    type := Type.Root;
    ripUp : RouteID.T := RouteID.Nil;
    firstRipUp := FALSE;
    at : GridPoint.T;
  OVERRIDES
    addRipUp := AddRipUp;
    ripUpIterate := MakeRipUpIterator;
    firstRipUpIterate := MakeFirstRipUpIterator;
    rippedUpHere := RippedUpHere;
    rippedUpAnywhere := RippedUpAnywhere;
    numRipUps := NumRipUps;
    previous := Previous;
    ripUpPoints := RipUpPoints;
  END;

PROCEDURE Previous(self : T) : T = BEGIN RETURN self.up END Previous;

PROCEDURE NumRipUps(self : T) : CARDINAL = 
  VAR
    s := self;
    res := 0;
    ridSet := NEW(RouteIDSetDef.T).init();
  BEGIN
    WHILE s.up # NIL DO
      IF NOT RouteID.Equal(s.ripUp,RouteID.Nil) AND 
         NOT ridSet.member(s.ripUp) THEN 
        EVAL ridSet.insert(s.ripUp);
        INC(res) 
      END;
      s := s.up
    END;
    RETURN res
  END NumRipUps;

PROCEDURE RippedUpAnywhere(self : T; path : RouteID.T) : BOOLEAN =
  VAR 
    x := self;
  BEGIN
    WHILE x # NIL DO
      IF x.type = Type.RipUp AND RouteID.Equal(x.ripUp,path) THEN RETURN TRUE END;
      x := x.up
    END;
    RETURN FALSE
  END RippedUpAnywhere;

PROCEDURE RippedUpHere(self : T; path : RouteID.T; p : GridPoint.T) : BOOLEAN =
  VAR 
    x := self;
  BEGIN
    WHILE x # NIL DO
      IF x.type = Type.RipUp AND 
         RouteID.Equal(x.ripUp,path) AND
         GridPoint.Equal(x.at,p) THEN RETURN TRUE END;
      x := x.up
    END;
    RETURN FALSE
  END RippedUpHere;

PROCEDURE AddRipUp(self : T; of : RouteID.T; at : GridPoint.T; first : BOOLEAN) : T =
  VAR 
    res := NEW(T, up := self, type := Type.RipUp, ripUp := of, at := at,
               firstRipUp := first);
  BEGIN
    <* ASSERT of # RouteID.Nil *>
    <* ASSERT NOT self.rippedUpHere(of,at) *>
    RETURN res
  END AddRipUp;

PROCEDURE MakeRipUpIterator(self : T) : RipUpIterator = 
  BEGIN
    RETURN NEW(RipUpIteratorPrivate, leaf := self)
  END MakeRipUpIterator;

PROCEDURE MakeFirstRipUpIterator(self : T) : RipUpIterator = 
  BEGIN
    RETURN NEW(RipUpIteratorPrivate, leaf := self, next := FirstRipUpNext)
  END MakeFirstRipUpIterator;

TYPE
  RipUpIteratorPrivate = RipUpIterator OBJECT
    leaf : T;
  OVERRIDES
    next := RipUpNext;
  END;

PROCEDURE RipUpNext(self : RipUpIteratorPrivate; 
                    VAR deadRoute : RouteID.T;
                    VAR at : GridPoint.T) : BOOLEAN =
  BEGIN
    WHILE self.leaf # NIL DO
      TRY
        IF self.leaf.type = Type.RipUp THEN
          deadRoute := self.leaf.ripUp;
          at := self.leaf.at;
          RETURN TRUE
        END
      FINALLY
        self.leaf := self.leaf.up
      END
    END;
    RETURN FALSE
  END RipUpNext;

PROCEDURE FirstRipUpNext(self : RipUpIteratorPrivate; 
                    VAR deadRoute : RouteID.T;
                    VAR at : GridPoint.T) : BOOLEAN =
  BEGIN
    WHILE self.leaf # NIL DO
      TRY
        IF self.leaf.type = Type.RipUp AND self.leaf.firstRipUp THEN
          deadRoute := self.leaf.ripUp;
          at := self.leaf.at;
          RETURN TRUE
        END
      FINALLY
        self.leaf := self.leaf.up
      END
    END;
    RETURN FALSE
  END FirstRipUpNext;

PROCEDURE RipUpPoints(t : T) : GridPointSet.T =
  VAR
    ripUpIter := t.ripUpIterate();
    deadRoute : RouteID.T;
    deadPoint : GridPoint.T;
    graveyard := NEW(GridPointSetDef.T).init();
  BEGIN
    WHILE ripUpIter.next(deadRoute,deadPoint) DO 
      EVAL graveyard.insert(deadPoint)
    END;
    RETURN graveyard
  END RipUpPoints;




BEGIN END RouteState.
