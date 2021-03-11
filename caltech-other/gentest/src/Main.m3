(* $Id$ *)

MODULE Main;
IMPORT Scan, Params, CITRandom, IO;
IMPORT GridPoint, GridPointSetDef, MagCell;
IMPORT Wr, FileWr, Thread;
IMPORT Lex, FloatMode;
IMPORT OSError;
IMPORT Fmt;
IMPORT MagicStuff;
IMPORT Conf, FileRd, Rd;

<* FATAL Wr.Failure, OSError.E, Lex.Error, FloatMode.Trap, Thread.Alerted,
Conf.ParseError, Rd.Failure, Conf.LayerNotFound *>

BEGIN
  IO.Put(Params.Get(0) & 
    " <nets> <size of layout> <terms> <random seed> <cell name>\n");
  VAR
    nets := Scan.Int(Params.Get(1));
    size := Scan.Int(Params.Get(2));
    trms := Scan.Int(Params.Get(3));
    seed := Scan.Int(Params.Get(4));
    cellName := Params.Get(5);
    rand := NEW(CITRandom.T).init(fixed := TRUE, seed := seed);
    hadIt := NEW(GridPointSetDef.T).init();
    c := NEW(MagCell.Labelled).init(cellName);
    conWr := FileWr.Open(cellName & ".con");
    confRd := FileRd.Open("router.conf");
  BEGIN
    EVAL Conf.ReadConf(confRd);

    FOR i := 0 TO nets - 1 DO
      VAR
        netNam := "NET" & Fmt.Int(i);
        p : GridPoint.T;
      BEGIN
        Wr.PutText(conWr, netNam);
        Wr.PutChar(conWr, '\n');
        FOR j := 0 TO trms - 1 DO
          REPEAT
            p := GridPoint.T { rand.integer(0,size-1),
                               rand.integer(0,size-1),
                               1 };
          UNTIL NOT hadIt.member(p);
          
          EVAL hadIt.insert(p);
          
          EVAL MagicStuff.DrawATarget(c, p, netNam)
          
        END
      END
    END;
    
    c.write();
    Wr.Close(conWr)
  END
END Main.
