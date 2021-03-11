(* $Id$ *)
MODULE Main;
IMPORT MagCell;
IMPORT GridRouter2;
IMPORT FileRd, FileWr, Conf;
IMPORT Rd, Wr, Text, Thread, LabelListSetList, LabelListSet, LabelFinder, TextSetDef;
IMPORT MagRouter, MagLabelList;
IMPORT Params;
IMPORT TextReader, Debug;
IMPORT Cost, MagicStuff;
IMPORT Stdio, MagRect;
IMPORT Fmt;
IMPORT OSError;
IMPORT TextMagLayerTbl AS TextLayerTbl;
IMPORT Process;
IMPORT Env, MagPath;
IMPORT RouterDeferral, TextRouterDeferralTbl, RouterDeferralList;

IMPORT EndPointStatus;
FROM EndPointStatus IMPORT Dir;

<* FATAL Thread.Alerted *>

PROCEDURE CopyFile(from, to : TEXT) RAISES { OSError.E, Thread.Alerted, Rd.Failure, Wr.Failure } =
  CONST
    Buffer = 8 * 1024;
  VAR
    rd := FileRd.Open(from);
    wr := FileWr.Open(to);
    t : TEXT;
  BEGIN
    REPEAT
      t := Rd.GetText(rd,Buffer);
      Wr.PutText(wr,t)
    UNTIL Text.Equal(t,""); (* better than Text.Length *)
    Wr.Close(wr); Rd.Close(rd)
  END CopyFile;

PROCEDURE SlashesToDots(line : TEXT; doIt : BOOLEAN) : TEXT =
  BEGIN
    IF NOT doIt THEN 
      RETURN line
    ELSE
      VAR
        resArr := NEW(REF ARRAY OF CHAR, Text.Length(line));
      BEGIN
        FOR i := 0 TO LAST(resArr^) DO
          WITH c = Text.GetChar(line,i) DO
            IF c = '/' THEN resArr[i] := '.' ELSE resArr[i] := c END
          END
        END;
        RETURN Text.FromChars(resArr^)
      END
    END
  END SlashesToDots;

EXCEPTION DeferralSyntaxError(CARDINAL);

PROCEDURE ParseDeferrals(name : TEXT; slashesAreDots : BOOLEAN) : TextRouterDeferralTbl.T RAISES { DeferralSyntaxError, OSError.E, Rd.Failure } =
  VAR
    res := NEW(TextRouterDeferralTbl.Default).init();
    rd := FileRd.Open(name);
    line : TEXT;
    reader : TextReader.T;
    node, dirs : TEXT;
    ds : SET OF Dir;
    lNo := 0;
    found : BOOLEAN;
  BEGIN
    TRY
      LOOP
        INC(lNo);
        line := Rd.GetLine(rd);
        reader := NEW(TextReader.T).init(line);
        node := SlashesToDots(reader.nextE(" "),slashesAreDots);
        
        dirs := "";

        EVAL reader.next(" ", dirs);
        
        (* one way of signalling a syntax error ... *)
        IF NOT reader.isEmpty() THEN RAISE DeferralSyntaxError(lNo) END;

        (* parse it *)
        ds := SET OF Dir {};
        FOR i := 0 TO Text.Length(dirs) - 1 DO
          found := FALSE;
          FOR j := FIRST(Dir) TO LAST(Dir) DO
            IF Text.GetChar(dirs,i) = Text.GetChar(EndPointStatus.DirName[j],0) THEN
              found := TRUE;
              
              (* no listing twice *)
              IF j IN ds THEN RAISE DeferralSyntaxError(lNo) END;

              ds := ds + SET OF Dir { j }
            END
          END;

          (* no gibberish, please *)
          IF NOT found THEN RAISE DeferralSyntaxError(lNo) END
        END;
        
        EVAL res.put(node,NEW(RouterDeferral.T, exitDirs := ds))
        
      END
    EXCEPT
      Rd.EndOfFile => (* skip *)
    |
      TextReader.NoMore => RAISE DeferralSyntaxError(lNo)
    END;
    RETURN res
  END ParseDeferrals;

(* parsealiasfile back-patches deferrals with the label lists *)
PROCEDURE ParseAliasFile(cell : MagCell.T; name : TEXT;
                         slashesAreDots : BOOLEAN;
                         deferrals : TextRouterDeferralTbl.T) : LabelListSet.T RAISES { OSError.E, Rd.Failure } =
  VAR
    toAttempt : LabelListSet.T := NEW(LabelListSetList.T).init();
    rd := FileRd.Open(name);
  BEGIN
    TRY
      LOOP
        VAR
          line := SlashesToDots(Rd.GetLine(rd),slashesAreDots);
          reader := NEW(TextReader.T).init(line);
          aliasList := reader.shatter(" ", "", TRUE);
          lNams := NEW(TextSetDef.T).init();
          deferral : RouterDeferral.T := NIL;
        BEGIN
          Debug.Out("Reading alias line \"" & line & "\"");
          WHILE aliasList # NIL DO
            Debug.Out("Adding alias \"" & aliasList.head & "\"");
            EVAL lNams.insert(aliasList.head);

            (* back-patch deferrals *)
            (* this is an awful hack because deferrals are being duplicated
               by m3-3.  if we find a duplicated deferral, we delete it
               and print a warning and revert to the old deferral... *)
            IF deferrals # NIL THEN
              IF deferral = NIL THEN
                EVAL deferrals.get(aliasList.head,deferral)
              ELSE
                VAR
                  o := deferral;
                BEGIN
                  IF deferrals.delete(aliasList.head,deferral) THEN
                    Debug.Warning("Duplicate deferral! node \"" & aliasList.head &
                      "\"!"); 
                    deferral := o
                  END
                END
              END
            END;

            aliasList := aliasList.tail

          END;
          VAR
            labs := LabelFinder.FindTextSetLabels(cell,lNams);
          BEGIN
            IF deferral # NIL THEN
              (* this "fixes" the entry in the deferral table bc. of
                 "rep exposure"  *)
              IF labs # NIL THEN
                deferral.labels := labs 
              END
            END;

            IF MagLabelList.Length(labs) = 0 THEN
              IF warnOnNoLabels THEN
                Debug.Warning("No labels found for alias list \"" & line & "\"!")
              END
            END;

            VAR
              p := labs;
            BEGIN
              Debug.Out("Main.ParseAliasFile: alias line:\n" & line & "\n:");
              WHILE p # NIL DO
                Debug.Out("LABEL \"" & p.head.name & "\" at " &
                  MagRect.Format(p.head.rect));
                p := p.tail
              END
            END;

            IF labs # NIL THEN 
              EVAL toAttempt.insert(labs) 
            END

          END
        END
      END
    EXCEPT
      Rd.EndOfFile => (* skip *)
    END;

    IF deferrals # NIL THEN
      (* check that we have targets for every deferral *)
      VAR
        iter := deferrals.iterate();
        n : TEXT;
        d : RouterDeferral.T;
        noTargets := NEW(TextSetDef.T).init();
      BEGIN
        WHILE iter.next(n,d) DO
          IF d.labels = NIL THEN
            Debug.Warning("Couldn't find a target for deferred node \"" &
              n & "\"!");
            EVAL noTargets.insert(n)
          END
        END;

        VAR
          noTargIter := noTargets.iterate();
        BEGIN
          WHILE noTargIter.next(n) DO
            EVAL deferrals.delete(n,d)
          END
        END
      END
    END;

    RETURN toAttempt
  END ParseAliasFile;
  
VAR
  rd : Rd.T; 
  layerDB : TextLayerTbl.T;
  cell : MagCell.T;
  router : MagRouter.T;
  toAttempt : LabelListSet.T;
  failed : LabelListSet.T := NEW(LabelListSetList.T).init();
  src : TEXT;
  srcName, conName : TEXT;
  outName : TEXT;
  out : TEXT := NIL;
  seven := FALSE;
  errout := Stdio.stderr;
    


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
  EmphMetal1Costs = Cost.Costs {
    Cost.Def { 0, 999, 999,  Cost.Infty },
    Cost.Def { 1, 9, 9,  100 },
    Cost.Def { 2, 100, 10, 100 },
    Cost.Def { 3, 10, 100, 500 },
    Cost.Def { 4, 100, 20,  300 },
    Cost.Def { 5, 5, 100,  Cost.Infty (* ignored *)}
  }; 
VAR
  costsToUse : Cost.Costs := Metal3MaxCosts;
  p1 : TEXT;
  delWr : Wr.T;
  delName : TEXT;
  deferrals : TextRouterDeferralTbl.T;
  nextP := 1;
  slashesAreDots := FALSE;
  hierRouting := FALSE;
  keepInternalDeferredWiring := FALSE;
  respectBbox := FALSE;
  rotateCosts := FALSE;
  deferName : TEXT;
  routerMagPath := Env.Get("ROUTERMAGPATH");
  warnOnNoLabels := TRUE;
BEGIN
  IF routerMagPath # NIL THEN
    MagPath.Set(routerMagPath)
  END;
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
    ELSIF Text.Equal(p1,"-emphasizeMetal1") THEN
      costsToUse := EmphMetal1Costs
    ELSIF Text.Equal(p1,"-slashesaredots") THEN
      slashesAreDots := TRUE
    ELSIF Text.Equal(p1,"-hierarchical") THEN
      hierRouting := TRUE
    ELSIF Text.Equal(p1,"-keepinternaldeferredwiring") THEN
      keepInternalDeferredWiring := TRUE
    ELSIF Text.Equal(p1,"-respectbbox") THEN
      respectBbox := TRUE
    ELSIF Text.Equal(p1,"-rotatecosts") THEN
      rotateCosts := TRUE
    ELSIF Text.Equal(p1,"-nomissinglabelwarnings") THEN
      warnOnNoLabels := FALSE
    ELSIF Text.Equal(p1,"-o") THEN
      p1 := Params.Get(nextP);
      INC(nextP);
      out := p1
    ELSE
      Process.Crash("What do you mean by \""&p1&"\"!?")
    END;
    p1 := Params.Get(nextP)
  END;

  TRY
    rd := FileRd.Open("router.conf");
    layerDB := Conf.ReadConf(rd)
  EXCEPT
    OSError.E => 
      Process.Crash("Couldn't open config file!")
  |
    Rd.Failure => 
      Process.Crash("Problems reading config file!")
  |
    Conf.ParseError(lno) =>
      Process.Crash("Parse error reading config file on line "&Fmt.Int(lno)&
        "!")
  |
    Conf.LayerNotFound(lname) =>
      Process.Crash("Layer \"" & lname & "\" in conf file not found!")
  END;

  (* the following doesn't really seem to help... *)
  (* Cost.MaxCostOverGreedy := 10.0 * Cost.MaxCostOverGreedy; *)

  Cost.MaxCostOverGreedy := 2000.0;
  Cost.MaxMaxCost := 100 * 1000 * 1000;
  Cost.RipUpIncrement := 0.2;
  Cost.OutOfBoundsIncreaseRate := 0.05;

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

  IF seven THEN
    MagicStuff.SetViaNEOverlap(0);
    Cost.SetMoveMoreThanOneVertically(FALSE)
  END;

  
  IF Params.Count = nextP + 1 THEN 
    src := Params.Get(nextP) 
  ELSE
    Process.Crash("Wrong # of cmd-line params!")
  END;
  srcName := src & ".mag";

  IF out = NIL THEN
    out := src & "_out";
  END;

  outName := out & ".mag";
  conName := src & ".con";
  delName := src & ".deleted";
  
  IF hierRouting THEN
    deferName := src & ".defer";
    TRY
      deferrals := ParseDeferrals(deferName,slashesAreDots)
    EXCEPT
      OSError.E, Rd.Failure => Process.Crash("Trouble reading \"" & deferName &
        "\"!")
    |
      DeferralSyntaxError(line) => Process.Crash("Syntax error reading \"" & deferName &
        "\", line "&Fmt.Int(line)&"!")
    END
  END;

  TRY
    delWr := FileWr.Open(delName)
  EXCEPT
    OSError.E => 
      Process.Crash("Couldn't open \"" & delName & "\" for writing!")
  END;

  (* copy source file so that we don't overwrite it *)
  TRY
    CopyFile(srcName,outName)
  EXCEPT
    OSError.E, Rd.Failure, Wr.Failure => 
      Process.Crash("Problems copying \""&srcName&"\" to \""&outName&"\"!")
  END;
    

  (* read in start cell *)
  TRY
    cell := NEW(MagCell.Labelled).lookup(out,layerDB)
  EXCEPT
    MagCell.NotFound(c) =>
      Process.Crash("Couldn't find mag cell \""&c&"\" while attempting to open \""&out&"!")
  |
    MagCell.SyntaxError =>      
      Process.Crash("Syntax error while reading mag cell \""&out&"!")
  |
    Rd.Failure =>
      Process.Crash("I/O error reading mag cell \""&out&"!")
  END;

  TRY
    toAttempt := ParseAliasFile(cell,conName,slashesAreDots,deferrals)
  EXCEPT ELSE 
    Process.Crash("Problems reading alias file \""&conName&"\"!")
  END;

  (* initialize the router *)
  router := NEW(GridRouter2.T).init(cell,layerDB);

  EVAL NARROW(router,GridRouter2.T).initDelWr(delWr);

  IF hierRouting THEN
    VAR
      l : RouterDeferralList.T := NIL;
      iter := deferrals.iterate();
      n : TEXT; (* dummy *)
      d : RouterDeferral.T;
    BEGIN
      WHILE iter.next(n,d) DO l := RouterDeferralList.Cons(d,l) END;

      NARROW(router,GridRouter2.T).setDeferrals(l,keepInternalDeferredWiring)
    END
  END;

  (* and run it! *)
  router.run(toAttempt, failed, respectBbox);

  (* here we should check for failed routes *)
  <* FATAL Wr.Failure *>
  BEGIN
    IF failed.size() > 0 THEN
      Wr.PutText(errout, "**\n**  WARNING: "&Fmt.Int(failed.size())&" FAILED ROUTES!\n**\n");
      VAR
        iter := failed.iterate();
        lp : MagLabelList.T;
      BEGIN
        WHILE iter.next(lp) DO
          Wr.PutText(errout, "FAILED ROUTE LABELS:\n");
          WHILE lp # NIL DO
            Wr.PutText(errout, " " & lp.head.name & "@" & MagRect.Format(lp.head.rect ) & "\n");
            lp := lp.tail
          END;
          Wr.PutChar(errout, '\n')
        END
      END
    END
  END;

  (* dump the layout *)
  TRY
    cell.tightenBBox();
    cell.write()
  EXCEPT
    OSError.E, Wr.Failure =>
      Process.Crash("Problems writing output layout!")
  END;

  TRY Wr.Close(delWr) EXCEPT ELSE END
END Main.
