(* $Id$ *)
MODULE Main;
IMPORT IntSet, IntSetDef;
IMPORT OnGridRouter, Bbox;
IMPORT FileRd, FileWr;
IMPORT Rd, Wr, Text, Thread;
IMPORT Params;
IMPORT TextReader, Debug;
IMPORT Cost;
IMPORT Stdio;
IMPORT Fmt;
IMPORT OSError;
IMPORT Process;
IMPORT Pickle, Filled;
IMPORT GridPoint, GridPointSet, GridPointSetDef, GridPointSetSeq;
IMPORT Scan, RefList;
IMPORT Lex, FloatMode;
IMPORT CompRoute;
IMPORT RTRefStats;
IMPORT SimpleRoute;
FROM GridPoint IMPORT Layer;
IMPORT IntRefTbl;

<* FATAL Thread.Alerted *>

    
CONST
  Metal2MaxCosts = Cost.Costs {
    Cost.Def { 0, 999, 999,  Cost.Infty },
    Cost.Def { 1, 15, 50,  100 },
    Cost.Def { 2, 100, 10, Cost.Infty },
    Cost.Def { 3, 10, 100, Cost.Infty },
    Cost.Def { 4, 100, 20,  Cost.Infty },
    Cost.Def { 5, 5, 100,  Cost.Infty (* ignored *)}
  }; 
  Metal3MaxCosts = Cost.Costs {
    Cost.Def { 0, 999, 999,  Cost.Infty },
    Cost.Def { 1, 15, 50,  100 },
    Cost.Def { 2, 100, 10, 100 },
    Cost.Def { 3, 10, 100, Cost.Infty },
    Cost.Def { 4, 100, 20,  Cost.Infty },
    Cost.Def { 5, 5, 100,  Cost.Infty (* ignored *)}
  }; 
  Metal4MaxCosts = Cost.Costs {
    Cost.Def { 0, 999, 999,  Cost.Infty },
    Cost.Def { 1, 15, 50,  100 },
    Cost.Def { 2, 100, 10, 100 },
    Cost.Def { 3, 10, 100, 500 },
    Cost.Def { 4, 100, 20,  Cost.Infty },
    Cost.Def { 5, 5, 100,  Cost.Infty (* ignored *)}
  }; 
  Metal5MaxCosts = Cost.Costs {
    Cost.Def { 0, 999, 999,  Cost.Infty },
    Cost.Def { 1, 15, 50,  100 },
    Cost.Def { 2, 100, 10, 100 },
    Cost.Def { 3, 10, 100, 500 },
    Cost.Def { 4, 100, 20,  300 },
    Cost.Def { 5, 5, 100,  Cost.Infty (* ignored *)}
  }; 

  AvoidMetal4Costs = Cost.Costs {
    Cost.Def { 0, 999, 999,  Cost.Infty },
    Cost.Def { 1, 15, 50,  100 },
    Cost.Def { 2, 100, 10, 100 },
    Cost.Def { 3, 10, 100, 500 },
    Cost.Def { 4, 200, 200,  300 },
    Cost.Def { 5, 5, 100,  Cost.Infty (* ignored *)}
  }; 

  (* the following is used when you want wires to go ONLY in the right
     direction *)
  StrictMetal5Costs = Cost.Costs {
    Cost.Def { 0, 999, 999,  Cost.Infty },
    Cost.Def { 1, 15, 500,  100 },
    Cost.Def { 2, 1000, 10, 100 },
    Cost.Def { 3, 10, 1000, 500 },
    Cost.Def { 4, 1000, 20,  300 },
    Cost.Def { 5, 5, 1000,  Cost.Infty (* ignored *)}
  }; 

  CheapMetal5Costs = Cost.Costs {
    Cost.Def { 0, 999, 999,  Cost.Infty },
    Cost.Def { 1, 50, 50,  100 },
    Cost.Def { 2, 100, 20, 100 },
    Cost.Def { 3, 20, 100, 50 },
    Cost.Def { 4, 1000, 5,  50 },
    Cost.Def { 5, 5, 1000,  Cost.Infty (* ignored *)}
  }; 
  CheapMetal4Costs = Cost.Costs {
    Cost.Def { 0, 999, 999,  Cost.Infty },
    Cost.Def { 1, 50, 50,  100 },
    Cost.Def { 2, 100, 20, 100 },
    Cost.Def { 3, 20, 100, 50 },
    Cost.Def { 4, 1000, 5,  Cost.Infty },
    Cost.Def { 5, 5, 1000,  Cost.Infty (* ignored *)}
  }; 
  EmphMetal1Costs = Cost.Costs {
    Cost.Def { 0, 999, 999,  Cost.Infty },
    Cost.Def { 1, 9, 9,  100 },
    Cost.Def { 2, 100, 10, 100 },
    Cost.Def { 3, 10, 100, 500 },
    Cost.Def { 4, 100, 20,  300 },
    Cost.Def { 5, 5, 100,  Cost.Infty (* ignored *)}
  }; 

TYPE
  MyNet = GridPointSetSeq.T OBJECT
    label : TEXT; (* for debugging *)
  END;

TYPE
  DebugMemClosure = Thread.Closure OBJECT
    interval : LONGREAL;
  OVERRIDES
    apply := DebugMemApply
  END;

PROCEDURE DebugMemApply(c : DebugMemClosure) : REFANY =
  BEGIN
    <* ASSERT c.interval > 0.0d0 *>
    LOOP
      RTRefStats.ReportReachable();
      Thread.Pause(c.interval)
    END
  END DebugMemApply;

PROCEDURE PickNeighbors(fromSet : GridPointSet.T; 
                        neighboring : GridPointSet.T;
                        origComps : MyNet) : GridPointSet.T = 
  VAR
    res := NEW(GridPointSetDef.T).init();
    iter := fromSet.iterate();
    f : GridPoint.T;
    myComp : [-1..LAST(CARDINAL)];


  PROCEDURE Test(READONLY p : GridPoint.T) =
    BEGIN
      (* check if we have a non-endpoint neighbor *)
      IF neighboring.member(p) THEN EVAL res.insert(f) END;

      (* check if we have an endpoint neighbor *)
      IF fromSet.member(p) THEN
        (* we do---track down component of neighbor... *)
        FOR i := 0 TO origComps.size()-1 DO
          IF origComps.get(i).member(p) THEN
            IF i # myComp THEN
              EVAL res.insert(f)
            END;
            EXIT
          END
        END
      END
    END Test;

  BEGIN
    WHILE iter.next(f) DO
      myComp := -1;
      FOR i := 0 TO origComps.size()-1 DO
        IF origComps.get(i).member(f) THEN
          myComp := i;
          EXIT
        END
      END;

      Test(GridPoint.T { f.x-1, f.y,   f.l });
      Test(GridPoint.T { f.x,   f.y-1, f.l });
      Test(GridPoint.T { f.x+1, f.y,   f.l });
      Test(GridPoint.T { f.x,   f.y+1, f.l });
      IF f.l # FIRST(Layer) THEN Test(GridPoint.T { f.x, f.y, f.l-1 }) END;
      IF f.l # LAST(Layer)  THEN Test(GridPoint.T { f.x, f.y, f.l+1 }) END
    END;
    RETURN res
  END PickNeighbors;

VAR
  router : OnGridRouter.T;
  deferrals, failed : IntSet.T;
  errout := Stdio.stderr;
  costsToUse : Cost.Costs := Metal3MaxCosts;
  p1 : TEXT;
  delWr, tgtWr : Wr.T;
  pickRd, srcRd : Rd.T;

  src : TEXT;
  delName, srcName, pickName, tgtName, quitName : TEXT;
  nextP := 1;
  hierRouting := FALSE;
  keepInternalDeferredWiring := FALSE;
  respectBbox := FALSE;
  rotateCosts := FALSE;
  bbox : Bbox.T;
  routingProblem : TEXT := NIL;
  maxNet := -1;
  debugMem := 0.0d0;
  quitThreshold := 0.0d0;
  quitFailures := LAST(CARDINAL);
  netCompsTbl := NEW(IntRefTbl.Default).init();
BEGIN
  IF Params.Count < 2 THEN
    Process.Crash("No cmd-line parameters!  Give me something to do!")
  END;
  p1 := Params.Get(nextP);
  WHILE Text.GetChar(p1,0) = '-' DO
    INC(nextP);
    IF    Text.Equal(p1,"-metal3") THEN
      costsToUse := Metal3MaxCosts
    ELSIF Text.Equal(p1,"-metal2") THEN
      costsToUse := Metal2MaxCosts
    ELSIF Text.Equal(p1,"-metal4") THEN
      costsToUse := Metal4MaxCosts
    ELSIF Text.Equal(p1,"-metal5") THEN
      costsToUse := Metal5MaxCosts
    ELSIF Text.Equal(p1,"-strictMetal5") THEN
      costsToUse := StrictMetal5Costs
    ELSIF Text.Equal(p1,"-cheapMetal5") THEN
      costsToUse := CheapMetal5Costs
    ELSIF Text.Equal(p1,"-avoidMetal4") THEN
      costsToUse := AvoidMetal4Costs
    ELSIF Text.Equal(p1,"-cheapMetal4") THEN
      costsToUse := CheapMetal4Costs
    ELSIF Text.Equal(p1, "-debugMem") THEN
<*NOWARN*>      debugMem := Scan.LongReal(Params.Get(nextP)); INC(nextP)
    ELSIF Text.Equal(p1,"-emphasizeMetal1") THEN
      costsToUse := EmphMetal1Costs
    ELSIF Text.Equal(p1, "-quitfailures") THEN
      quitFailures := Scan.Int(Params.Get(nextP)); INC(nextP)
    ELSIF Text.Equal(p1, "-quitthreshold") THEN
      quitThreshold := Scan.LongReal(Params.Get(nextP)); INC(nextP)
    ELSIF Text.Equal(p1,"-hierarchical") THEN
      hierRouting := TRUE
    ELSIF Text.Equal(p1,"-keepinternaldeferredwiring") THEN
      keepInternalDeferredWiring := TRUE
    ELSIF Text.Equal(p1,"-respectbbox") THEN
      respectBbox := TRUE
    ELSIF Text.Equal(p1,"-rotatecosts") THEN
      rotateCosts := TRUE
    ELSE
      Process.Crash("What do you mean by \""&p1&"\"!?")
    END;
    p1 := Params.Get(nextP)
  END;

  IF debugMem # 0.0d0 THEN
    EVAL Thread.Fork(NEW(DebugMemClosure, interval := debugMem))
  END;

  Cost.MaxCostOverGreedy := 5.0;
  Cost.MaxMaxCost := 100 * 1000 * 1000;
  Cost.RipUpIncrement := 0.2;
  Cost.OutOfBoundsIncreaseRate := 0.05;
  Cost.BaseCost := 1000000;

  (* the following rotates the costs if we want that... *)
  IF rotateCosts THEN
    FOR i := FIRST(costsToUse) TO LAST(costsToUse) DO
      VAR
        t := costsToUse[i].xCost;
      BEGIN
        costsToUse[i].xCost := costsToUse[i].yCost;
        costsToUse[i].yCost := t
      END
    END
  END;

  Cost.Set_costs(costsToUse);

  IF Params.Count = nextP + 1 THEN 
    src := Params.Get(nextP) 
  ELSE
    Process.Crash("Wrong # of cmd-line params!")
  END;

  delName := src & ".deleted";
  pickName := src & ".filledpickle";
  srcName := src & ".rp";
  tgtName := src & ".rr";
  quitName := src & ".quit";
  
  SimpleRoute.SetRipUpFilename(src & ".ripups");

  TRY
    delWr := FileWr.Open(delName)
  EXCEPT
    OSError.E => 
      Process.Crash("Couldn't open \"" & delName & "\" for writing!")
  END;

  TRY
    tgtWr := FileWr.Open(tgtName)
  EXCEPT
    OSError.E => 
      Process.Crash("Couldn't open \"" & tgtName & "\" for writing!")
  END;

  TRY
    pickRd := FileRd.Open(pickName);
    Filled.UnPickleFromReader(pickRd)
  EXCEPT
    Rd.Failure, OSError.E, Rd.EndOfFile =>
      Process.Crash("Trouble reading pickle file \"" & pickName & "\" for reading!")
  |
    Pickle.Error => 
      Process.Crash("Can't read the filledpickle---try re-running \"y\"?  (Data types may have changed.)")
  END;

  TRY
    srcRd := FileRd.Open(srcName)
  EXCEPT
    OSError.E => 
      Process.Crash("Couldn't open \"" & srcName & "\" for writing!")
  END;
  
  IF hierRouting THEN
    deferrals := NEW(IntSetDef.T).init()
  END;

  (* read routing problem, add the nets, etc. *)
  PROCEDURE PushBlock(b : Block) =
    VAR
      bb := NEW(REF Block);
    BEGIN
      bb^ := b;
      nesting := RefList.Cons(bb,nesting)
    END PushBlock;

  PROCEDURE PopBlock(n : TEXT) : BlockType =
    BEGIN
      IF nesting = NIL THEN
        Process.Crash("END without matching block start on line " &
            Fmt.Int(lineNo) & " of file \"" & srcName & "\"!")
      ELSIF NOT Text.Equal(n, NARROW(nesting.head,REF Block).name) THEN
        Process.Crash("Mismatched END on line " &
            Fmt.Int(lineNo) & " of file \"" & srcName & "\"!")
      END;
      TRY
        RETURN NARROW(nesting.head,REF Block).type
      FINALLY
        nesting := nesting.tail
      END
    END PopBlock;

  TYPE
    Block = RECORD
      name : TEXT;
      type : BlockType;
    END;

    BlockType = { RP, Comp, Net };
  CONST
    RP = BlockType.RP; Comp = BlockType.Comp; Net = BlockType.Net;
  VAR
    nesting : RefList.T := NIL;
    line : TEXT;
    reader : TextReader.T;
    cmd : TEXT;
    lineNo := 1;
    netName, compName : TEXT;
    comp : GridPointSet.T := NIL;
    nonObs : GridPointSet.T := NIL;
    net : MyNet := NIL;
    netNum : INTEGER := -1;
  BEGIN
    TRY
      LOOP
        line := Rd.GetLine(srcRd);
        reader := NEW(TextReader.T).init(line);
        cmd := reader.nextE(" ", skipNulls := TRUE);

        IF Text.Equal(cmd,"ROUTINGPROBLEM") THEN
          routingProblem := reader.nextE(" ", skipNulls := TRUE);
          PushBlock(Block { name := routingProblem, type := RP });
        ELSIF Text.Equal(cmd,"BOUNDINGBOX") THEN
          VAR
            llx, lly, urx, ury := Scan.Int(reader.nextE(" ", skipNulls := TRUE));
          BEGIN
            bbox.llx := llx; bbox.lly := lly; bbox.urx := urx; bbox.ury := ury;
            (* initialize the router *)
            router := NEW(OnGridRouter.T).init(bbox, delWr);
          END
        ELSIF Text.Equal(cmd,"NET") THEN
          netName := reader.nextE(" ", skipNulls := TRUE);
          netNum := Scan.Int(netName);
          PushBlock(Block { name := netName, type := Net });
          net := NEW(MyNet).init();
          nonObs := NEW(GridPointSetDef.T).init();
          maxNet := MAX(maxNet, netNum)
        ELSIF Text.Equal(cmd,"LABEL") THEN
          net.label := reader.nextE("", skipNulls := FALSE)
        ELSIF Text.Equal(cmd,"DEFER") THEN
          IF hierRouting THEN
            EVAL deferrals.insert(netNum)
          END
        ELSIF Text.Equal(cmd,"COMP") THEN
          compName := reader.nextE(" ", skipNulls := TRUE);
          PushBlock(Block { name := compName, type := Comp });
          comp := NEW(GridPointSetDef.T).init()
        ELSIF Text.Equal(cmd,"P") THEN
          VAR
            gp := GridPoint.Parse(reader.nextE(" ", skipNulls := TRUE));
          BEGIN
            EVAL comp.insert(gp)
          END
        ELSIF Text.Equal(cmd,"NONOBS") THEN
          VAR
            gp := GridPoint.Parse(reader.nextE(" ", skipNulls := TRUE));
          BEGIN
            EVAL nonObs.insert(gp)
          END
        ELSIF Text.Equal(cmd,"END") THEN
          VAR
            nam := reader.nextE(" ", skipNulls := TRUE);
            bt := PopBlock(nam);
          BEGIN
            CASE bt OF
              RP => EXIT
            |
              Comp => 
                IF comp = NIL THEN
                  Process.Crash("END for comp on line "&Fmt.Int(lineNo)&
                    " without matching COMP statement")
                END;

                IF comp.size() > 0 THEN
                  net.addhi(comp)
                ELSE
                  Debug.Warning("Component of size zero (no targets): " & nam)
                END;
                comp := NIL
            |
              Net =>
                IF netNum < 0 THEN
                  Process.Crash("END for net on line "&Fmt.Int(lineNo)&
                    " without matching NET statement")
                END;
                router.addNet(netNum, net, nonObs);
                EVAL netCompsTbl.put(netNum, net);
                net := NIL;
                netNum := -1;
                nonObs := NIL
            END
          END
        ELSE
          Process.Crash("Unknown command \"" & cmd & "\" on line " &
            Fmt.Int(lineNo) & " of file \"" & srcName & "\"!")
        END;
        INC(lineNo);
      END
    EXCEPT
      TextReader.NoMore, Lex.Error, FloatMode.Trap, GridPoint.ParseError =>
        Process.Crash("Syntax error reading routing problem file \"" & srcName &"\", line "&Fmt.Int(lineNo) &"!")
    |
      Rd.Failure => 
        Process.Crash("I/O error reading routing problem file \"" & srcName &"\"!")
    |
      Rd.EndOfFile => 
        Process.Crash("Routing problem file \"" & srcName &"\" ends prematurely!")
    END
  END;

  IF hierRouting THEN
    (* set deferrals *)
    router.setDeferrals(deferrals)
  END;

  (* close the router for further updates *)

  router.close();

  router.setQuittingRules(quitThreshold,quitFailures, quitName);

  (* and run it! *)
  router.run(respectBbox, failed);

  (* here we should check for failed routes *)
  <* FATAL Wr.Failure *>
  BEGIN
    IF failed.size() > 0 THEN
      Wr.PutText(errout, "**\n**  WARNING: "&Fmt.Int(failed.size())&" FAILED ROUTES!\n**\n");
      VAR
        iter := failed.iterate();
        n : INTEGER;
      BEGIN
        WHILE iter.next(n) DO
          Wr.PutText(errout, "FAILED NET #" & Fmt.Int(n) &"\n")
        END
      END
    END
  END;

  TRY
    VAR
      compRoute : CompRoute.T;
      tbl := router.getRoutes();
      p : GridPoint.T;
    BEGIN
      Wr.PutText(tgtWr, "ROUTINGRESULT " & routingProblem & "\n");
      FOR idx := 0 TO maxNet DO
        IF tbl.get(idx,compRoute) AND 
          (NOT hierRouting OR NOT deferrals.member(idx) OR keepInternalDeferredWiring) THEN

          Wr.PutText(tgtWr, "NET " & Fmt.Int(idx) & "\n");

          (* the below code accomplishes the following task:

             draw layout between the appropriate points in the result.
             
             layout is drawn between points as follows:

             a non-endpoint to all its included neighbors.

             an endpoint to its non-endpoint neighbors and its
             endpoint neighbors in other components.
          *)
          
          VAR
            endPoints := compRoute.activeEndPoints;
            allPoints := compRoute.allPoints;
            
            (* this code doesn't work for routes that go direct from one
               ep to another, but those are banned... or no? *)
            nonEndPoints := allPoints.diff(endPoints);

            r : REFANY;
            x := netCompsTbl.get(idx,r);

            pickedEndPoints := PickNeighbors(endPoints,nonEndPoints,r);
          BEGIN
            <* ASSERT x *>
            VAR
              epIter := pickedEndPoints.iterate();      
            BEGIN
              WHILE epIter.next(p) DO
                Wr.PutText(tgtWr, "ENDPOINT " & GridPoint.Format(p) & "\n")
              END
            END;

            VAR
              pIter := pickedEndPoints.union(nonEndPoints).iterate();
            BEGIN
              WHILE pIter.next(p) DO
                Wr.PutText(tgtWr, "P " & GridPoint.Format(p) & "\n")
              END
            END
          END;

          Wr.PutText(tgtWr, "END " & Fmt.Int(idx) & "\n");
        END
      END;
      Wr.PutText(tgtWr, "END " & routingProblem & "\n")
    END
  EXCEPT
    Wr.Failure =>
      Process.Crash("I/O error writing to output file \"" & tgtName &"\"!")
  END;

  TRY Wr.Close(tgtWr) EXCEPT ELSE END;
  TRY Wr.Close(delWr) EXCEPT ELSE END;

  IF failed.size() > 0 THEN Process.Exit(1) END
    
END Main.
