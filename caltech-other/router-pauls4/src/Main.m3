(* $Id$ *)
MODULE Main;
IMPORT MagCell, MagCellExtendable, MagSubCell, MagTransform;
IMPORT FileRd, FileWr, Conf;
IMPORT Rd, Wr, Text, Thread;
IMPORT Params;
IMPORT TextReader;
<*NOWARN*> IMPORT Debug;
IMPORT MagicStuff;
IMPORT Fmt;
IMPORT OSError;
IMPORT TextMagLayerTbl AS TextLayerTbl;
IMPORT Process;
IMPORT Env, MagPath;
IMPORT GridPointSet, GridPointSetDef, RefList, GridPoint, FloatMode, Lex;
IMPORT Scan;      
FROM TextUtils IMPORT ReplaceChar;
IMPORT AtomList, AL;

IMPORT Compass;

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

PROCEDURE DumpNet(c : MagCell.T;
                  endPoints, netPoints : GridPointSet.T) =
  BEGIN
    <* ASSERT endPoints.subset(netPoints) *>

    (* paint endpoints *)

    VAR
      iter := endPoints.iterate();
      p : GridPoint.T;
    BEGIN
      WHILE iter.next(p) DO
        VAR
          set := MagicStuff.EndPadRects(p);
        BEGIN
          c.addLayerRectSet(set)
        END
      END
    END;

    PROCEDURE ProcCand(READONLY q : GridPoint.T) =
      BEGIN
        IF netPoints.member(q) THEN
          VAR
            set := MagicStuff.ConnRects(p,q);
          BEGIN
            c.addLayerRectSet(set)
          END
        END
      END ProcCand;
    VAR
      iter := netPoints.iterate();
      p : GridPoint.T;
    BEGIN
      WHILE iter.next(p) DO
        (* find neighbors *)
        FOR i := FIRST(Compass.Dir) TO LAST(Compass.Dir) DO
          ProcCand(GridPoint.T { p.x + Compass.Step[i].x,
                                 p.y + Compass.Step[i].y,
                                 p.l })
        END;
        IF p.l > FIRST(GridPoint.Layer) THEN 
          ProcCand( GridPoint.T { p.x, p.y, p.l - 1 })
        END;
        IF p.l < LAST(GridPoint.Layer) THEN
          ProcCand( GridPoint.T { p.x, p.y, p.l + 1 })
        END;
      END
    END
  END DumpNet;

  
CONST WiringCellId = "gen_wiring";

PROCEDURE CrashMe(why : TEXT; xName : TEXT := NIL; al : AtomList.T := NIL) =
  VAR
    msg := why;
  BEGIN
    IF xName # NIL THEN
      msg := msg & " " & xName & ": "
    END;
    IF al # NIL THEN
      msg := msg & " : " & AL.Format(al)
    END;
    Process.Crash(msg)
  END CrashMe;

VAR
  rd, inRd : Rd.T; 
  layerDB : TextLayerTbl.T;
  wiringCell, cell : MagCell.T;
  src : TEXT;
  srcName, rrName : TEXT;
  outName : TEXT;
  out : TEXT := NIL;
  p1 : TEXT;
  nextP := 1;
  routerMagPath := Env.Get("ROUTERMAGPATH");
  rrfn : TEXT;
BEGIN
  IF routerMagPath # NIL THEN
    MagPath.Set(routerMagPath)
  END;

  IF Params.Count < 2 THEN
    CrashMe("No cmd-line parameters!  Give me something to do!")
  END;

  p1 := Params.Get(nextP);

  WHILE Text.GetChar(p1,0) = '-' DO
    INC(nextP);
    CrashMe("What do you mean by \""&p1&"\"!?");
    p1 := Params.Get(nextP)
  END;

  VAR
  BEGIN
    rrfn := p1 & ".rr";
    TRY 
      inRd := FileRd.Open(rrfn)
    EXCEPT
      OSError.E(x) => 
        CrashMe("Couldn't open routing-result file \"" & rrfn &  "\"!",
                "OSError.E", x)
    END
  END;

  TRY
    rd := FileRd.Open("router.conf");
    layerDB := Conf.ReadConf(rd)
  EXCEPT
    OSError.E(x) => 
      CrashMe("Couldn't open config file!","OSError.E",x)
  |
    Rd.Failure(x) => 
      CrashMe("Problems reading config file!","Rd.Failure",x)
  |
    Conf.ParseError(lno) =>
      CrashMe("Parse error reading config file on line "&Fmt.Int(lno)&
        "!")
  |
    Conf.LayerNotFound(lname) =>
      CrashMe("Layer \"" & lname & "\" in conf file not found!")
  END;

  IF Params.Count = nextP + 1 THEN 
    src := Params.Get(nextP) 
  ELSE
    CrashMe("Wrong # of cmd-line params!")
  END;
  srcName := src & ".mag";
  rrName := src & ".rr";

  IF out = NIL THEN
    out := src & "_out";
  END;

  outName := out & ".mag";
  
  (* copy source file so that we don't overwrite it *)
  TRY
    CopyFile(srcName,outName)
  EXCEPT
    Rd.Failure(x) =>
      CrashMe("Problems copying \""&srcName&"\" to \""&outName&"\"!",
              "Rd.Failure",x)
  |
    Wr.Failure(x) =>
    CrashMe("Problems copying \""&srcName&"\" to \""&outName&"\"!",
            "Wr.Failure",x)
  |
    OSError.E(x) =>
    CrashMe("Problems copying \""&srcName&"\" to \""&outName&"\"!",
            "OSError.E",x)
  END;

  (* read in start cell *)
  TRY
    cell := NEW(MagCell.Labelled).lookup(out,layerDB)
  EXCEPT
    MagCell.NotFound(c) =>
      CrashMe("Couldn't find mag cell \""&c&"\" while attempting to open \""&out&"!")
  |
    MagCell.SyntaxError =>      
      CrashMe("Syntax error while reading mag cell \""&out&"!")
  |
    Rd.Failure(x) =>
      CrashMe("I/O error reading mag cell \""&out&"!",
              "Rd.Failure",x)
  END;

  VAR
    sub : MagSubCell.T;
    try := 0;
    base := ReplaceChar(ReplaceChar(cell.getName() & "_" & WiringCellId,'.','_'),'/','_');
  BEGIN
    WHILE cell.getSubCell(base & Fmt.Int(try), sub) DO
      INC(try)
    END;

    sub := MagSubCell.T {
      NEW(MagCell.Labelled).init(cell.getName() & "_wiring"),
      MagTransform.Unitary,
      base & Fmt.Int(try),
      box := cell.getBBox() };
    
    cell.addSub(sub);
    wiringCell := sub.c;
  END;

  (* parse .rr file and add rects *)

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
        CrashMe("END without matching block start on line " &
            Fmt.Int(lineNo) & " of file \"" & srcName & "\"!")
      ELSIF NOT Text.Equal(n, NARROW(nesting.head,REF Block).name) THEN
        CrashMe("Mismatched END on line " &
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

    BlockType = { RR, Net };
  CONST
    RR = BlockType.RR; Net = BlockType.Net;
  VAR
    nesting : RefList.T := NIL;
    line : TEXT;
    reader : TextReader.T;
    cmd : TEXT;
    lineNo := 1;
    routingResult : TEXT;
    netNum : INTEGER := -1;
    netPoints, endPoints : GridPointSet.T := NIL;
  BEGIN
    TRY
      LOOP
        line := Rd.GetLine(inRd);
        reader := NEW(TextReader.T).init(line);
        cmd := reader.nextE(" ", skipNulls := TRUE);

        IF Text.Equal(cmd,"ROUTINGRESULT") THEN
          routingResult := reader.nextE(" ", skipNulls := TRUE);
          PushBlock(Block { name := routingResult, type := RR })
        ELSIF Text.Equal(cmd,"NET") THEN
          netNum := Scan.Int(reader.nextE(" ", skipNulls := TRUE));
          PushBlock(Block { name := Fmt.Int(netNum), type := Net });
          endPoints := NEW(GridPointSetDef.T).init();
          netPoints := NEW(GridPointSetDef.T).init()
        ELSIF Text.Equal(cmd,"ENDPOINT") THEN
          EVAL endPoints.insert(GridPoint.Parse(reader.nextE(" ", skipNulls := TRUE)))
        ELSIF Text.Equal(cmd,"P") THEN
          EVAL netPoints.insert(GridPoint.Parse(reader.nextE(" ", skipNulls := TRUE)))
        ELSIF Text.Equal(cmd,"END") THEN
          VAR
            bt := PopBlock(reader.nextE(" ", skipNulls := TRUE));
          BEGIN
            CASE bt OF
              RR => EXIT
            |
              Net =>
                IF netNum < 0 THEN
                  CrashMe("END for net on line "&Fmt.Int(lineNo)&
                    " without matching NET statement")
                END;
                DumpNet(wiringCell, endPoints, netPoints)
            END
          END
        ELSE 
          CrashMe("Unknown command \"" & cmd & "\" on line " &
            Fmt.Int(lineNo) & " of file \"" & srcName & "\"!")
        END;
        INC(lineNo);
      END
    EXCEPT
      TextReader.NoMore, Lex.Error, FloatMode.Trap, GridPoint.ParseError =>
        CrashMe("Syntax error reading routing problem file \"" & rrfn &
"\", line "&Fmt.Int(lineNo) &"!")
    |
      Rd.Failure(x) =>
        CrashMe("I/O error reading routing problem file \"" & rrfn &"\"!",
                "Rd.Failure", x) 
    |
      Rd.EndOfFile =>  
        CrashMe("Routing problem file \"" & rrfn &"\" ends prematurely!",
                "Rd.EndofFile")
    END
  END;


  (* dump the layout *)
  TRY
    wiringCell.tightenBBox();
    wiringCell.write()
  EXCEPT
    OSError.E, Wr.Failure =>
      CrashMe("Problems writing output layout!")
  END;
  TRY
    cell.tightenBBox();
    cell.write()
  EXCEPT
    OSError.E(x) =>
      CrashMe("Problems writing output layout!","OSError.E",x)
  |
    Wr.Failure(x) =>
      CrashMe("Problems writing output layout!","Wr.Failure",x)
  END;

END Main.
