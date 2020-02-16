(* $Id$ *)
MODULE Main;
IMPORT MagCell;
IMPORT GridRouter;
IMPORT FileRd, FileWr, Conf;
IMPORT Rd, Wr, Text, Thread, LabelListSetList, LabelFinder, TextSetDef;
IMPORT MagRouter, MagLabelList;
IMPORT Params;

PROCEDURE CopyFile(from, to : TEXT) =
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

CONST
  DefSrc = "test";
VAR
  rd := FileRd.Open("router.conf");
  layerDB := Conf.ReadConf(rd);
  cell : MagCell.T;
  lNams0, lNams1, lNams2 := NEW(TextSetDef.T).init();
  labelList : MagLabelList.T;
  router : MagRouter.T;
  toAttempt, failed := NEW(LabelListSetList.T).init();
  src := DefSrc;
BEGIN
  IF Params.Count > 1 THEN src := Params.Get(1) END;
  
  (* copy source file so that we don't overwrite it *)
  CopyFile(src & ".mag","output.mag");

  (* read in start cell *)
  cell := NEW(MagCell.Labelled).lookup("output",layerDB);

  (* create the set of label names that we're interested in.  Hierarchical
     names are OK if the cell is hierarchical too *)
  EVAL lNams0.insert("x");
  EVAL lNams1.insert("y");
  EVAL lNams2.insert("z");

  (* run the Label Finder!  The Label Finder finds all the labels with names
     that appear in conLabelNames. 
     The return value is a list of MagLabel.Ts *)
  (* insert the label list in the set of lists to attempt to route *)
  EVAL toAttempt.insert(LabelFinder.FindTextSetLabels(cell,lNams1));
  EVAL toAttempt.insert(LabelFinder.FindTextSetLabels(cell,lNams0));
  EVAL toAttempt.insert(LabelFinder.FindTextSetLabels(cell,lNams2));

  (* initialize the router *)
  router := NEW(GridRouter.T).init(cell,layerDB);

  (* and run it! *)
  router.run(toAttempt, failed);

  (* here we should check for failed routes *)

  (* dump the layout *)
  cell.write()

END Main.
