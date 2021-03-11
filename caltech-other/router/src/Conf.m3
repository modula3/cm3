(* $Id$ *)

MODULE Conf EXPORTS Conf, ConfPrivate;
IMPORT Rd, TextMagLayerTbl AS TextLayerTbl;
IMPORT Text, TextList, TextReader, Scan;
IMPORT MagRouteLayer AS RouteLayer;
IMPORT Debug;
IMPORT MagLayer AS Layer;
IMPORT LayerList, MagLayerList;
IMPORT Thread;
IMPORT FloatMode, Lex;
IMPORT GridPoint;
IMPORT RouteLayerIntTbl;
IMPORT Fmt;
IMPORT IntList;
IMPORT MagMergeDB;

VAR
  res := NEW(TextLayerTbl.Default).init();
  routeLayerTbl := NEW(RouteLayerIntTbl.Default).init();
  mergeDB := NEW(MagMergeDB.T).init();

PROCEDURE RouteLayerIterate() : RouteLayerIntTbl.Iterator =
  BEGIN RETURN routeLayerTbl.iterate() END RouteLayerIterate;

PROCEDURE ReadConf(rd : Rd.T) : TextLayerTbl.T RAISES { LayerNotFound, Rd.Failure, Thread.Alerted, ParseError  } =
  
  PROCEDURE AddAliases( l : RouteLayer.T; aliasList : TextList.T) =
    BEGIN
      WHILE aliasList # NIL DO
        EVAL res.put(aliasList.head, l);
        aliasList := aliasList.tail
      END;
    END AddAliases;

  PROCEDURE Lookup(name : TEXT) : RouteLayer.T RAISES { LayerNotFound } = 
    VAR
      l : Layer.T;
    BEGIN
      IF res.get(name, l) THEN RETURN l ELSE RAISE LayerNotFound(name) END
    END Lookup;

  PROCEDURE TextListToLayerList(tl : TextList.T) : LayerList.T RAISES { LayerNotFound } =
    VAR
      ll : LayerList.T := NIL;
    BEGIN
      WHILE tl # NIL DO
        ll := LayerList.Cons(Lookup(tl.head), ll);
        tl := tl.tail
      END;
      RETURN ll
    END TextListToLayerList;
    
  VAR
    lineNo := 0;
  BEGIN
    TRY
      LOOP
        INC(lineNo);
        VAR
          line := Rd.GetLine(rd);
          textReader := NEW(TextReader.T).init(line);
          keyWord : TEXT;
        BEGIN
          IF Text.Length(line) > 0 AND Text.GetChar(line,0) # '#' THEN
            keyWord := textReader.nextE(" ");
            IF Text.Equal(keyWord, "") THEN
              
            ELSIF Text.Equal(keyWord, "LAYER") THEN
              VAR
                aliasList := textReader.shatter(" ,", ":", skipNulls := TRUE);
                layer := NEW(RouteLayer.T).init(aliasList.head);
              BEGIN
                Debug.Out("Reading LAYER: " & line,100);
                AddAliases(layer, aliasList)
              END
            ELSIF Text.Equal(keyWord, "EQUIV") THEN
              VAR
                headLayer : RouteLayer.T :=
                    Lookup(textReader.nextE(" :", skipNulls := TRUE));
                equivList := textReader.shatter(" :,", "", skipNulls := TRUE);
                l := TextListToLayerList(equivList);
                ml : MagLayerList.T := NIL;
              BEGIN
                WHILE l # NIL DO 
                  ml := MagLayerList.Cons(l.head,ml); l := l.tail 
                END;
                mergeDB.addEquivalence(headLayer,ml)
              END
            ELSIF Text.Equal(keyWord, "CONNECT") THEN
              VAR
                headLayer : RouteLayer.T := Lookup(textReader.nextE(" :", skipNulls := TRUE));
                connectList := textReader.shatter(" :,", "", skipNulls := TRUE);
                l := TextListToLayerList(connectList);
              BEGIN
                WHILE l # NIL DO
                  headLayer.connects := 
                      headLayer.connects.union(l.head.connects);

                  VAR
                    iter := l.head.connects.iterate();
                    layer : Layer.T;
                  BEGIN
                    WHILE iter.next(layer) DO
                      EVAL NARROW(layer,RouteLayer.T).connects.insert(headLayer)
                    END
                  END;

                  l := l.tail
                END
              END
            ELSIF Text.Equal(keyWord, "ROUTE") THEN
              VAR
                rLevel := Scan.Int(textReader.nextE(" \t", skipNulls := TRUE));
                rLayer := Lookup(textReader.nextE(" \t", skipNulls := TRUE));
              BEGIN
                IF rLevel < FIRST(GridPoint.Layer) OR 
                   rLevel > LAST(GridPoint.Layer) THEN
                  Debug.Out("Routing level out of range---please recompile GridPoint.");
                  RAISE ParseError(lineNo)
                END;
                rLayer.setRouting(rLevel);

                <* ASSERT rLayer # NIL *>
                IF routeLayerTbl.put(rLayer,rLevel) THEN
                  Debug.Out("Duplicate routing layer at level " & 
                    Fmt.Int(rLevel));
                  RAISE ParseError(lineNo)
                END
              END
            ELSIF Text.Equal(keyWord, "VIA") THEN
              VAR
                srcL := Lookup(textReader.nextE(" \t", skipNulls := TRUE));
                tgtL := Lookup(textReader.nextE(": \t", skipNulls := TRUE));
                viaL := Lookup(textReader.nextE(": \t", skipNulls := TRUE));
              BEGIN 
                srcL.addVia(tgtL, viaL); 
                tgtL.addVia(srcL, viaL) 
              END
            END
          END
        END
      END
    EXCEPT 
      Rd.EndOfFile => Rd.Close(rd) 
    | TextReader.NoMore, FloatMode.Trap, Lex.Error => RAISE ParseError(lineNo)
    END;
    RETURN res
  END ReadConf;

PROCEDURE ViaLookup(from, to : Layer.T) : Layer.T =
  BEGIN RETURN NARROW(from, RouteLayer.T).getVia(to) END ViaLookup;

PROCEDURE RouteLevelLookup(layer : Layer.T) : INTEGER RAISES { LayerNotFound }=
  VAR
    res : INTEGER;
  BEGIN
    IF NOT routeLayerTbl.get(layer,res) THEN 
      RAISE LayerNotFound("hashVal = " & Fmt.Int(Layer.Hash(layer)))
    END;
    RETURN res
  END RouteLevelLookup;

PROCEDURE GetObstructedRoutingLayers(layer : Layer.T) : IntList.T =
  VAR
    res : IntList.T := NIL;
    iter := routeLayerTbl.iterate();
    rLayer : RouteLayer.T;
    rLevel : INTEGER;
  BEGIN
    WHILE iter.next(rLayer,rLevel) DO
      IF rLayer.connects.member(layer) THEN
        res := IntList.Cons(rLevel, res)
      END
    END;
    RETURN res
  END GetObstructedRoutingLayers;

PROCEDURE LayerLookup(level : INTEGER) : Layer.T RAISES { LayerNotFound } =
  VAR
    iter := routeLayerTbl.iterate();
    rLevel : INTEGER;
    rLayer : RouteLayer.T;
  BEGIN
    (* slow linear search *)
    WHILE iter.next(rLayer, rLevel) DO
      IF rLevel = level THEN RETURN rLayer END
    END;
    RAISE LayerNotFound("rLayer = " & Fmt.Int(level)) 
  END LayerLookup;

PROCEDURE MergeDB() : MagMergeDB.T = BEGIN RETURN mergeDB END MergeDB;

BEGIN END Conf.




