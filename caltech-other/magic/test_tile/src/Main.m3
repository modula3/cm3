(* $Id$ *)
MODULE Main;
IMPORT MagCell;
IMPORT MagTile;
IMPORT TextMagLayerTbl;
IMPORT IO;

VAR
  layerDB := NEW(TextMagLayerTbl.Default).init();
  aCell := NEW(MagCell.Labelled).lookup("a", layerDB, fillInLayers := TRUE);


  a := NEW(MagTile.T).makeFromMagCell(aCell);
  xCell := a.dump();
  b, c, d, e, f : MagTile.T;
BEGIN
  IO.Put("\n");
  xCell.write("x.mag");
  b := NEW(MagTile.T).makeHorizontally(ARRAY OF MagTile.T { a, a });
  b.dump().write("y.mag");  
  c := NEW(MagTile.T).makeByRotateCCW(b);
  c.dump().write("z.mag");

  d := NEW(MagTile.T).makeByOverlay(a, NEW(MagTile.T).makeSideways(a));

  e := NEW(MagTile.T).makeEmpty(100,d.getHeight());
  f := NEW(MagTile.T).makeHorizontally(ARRAY OF MagTile.T {d, e, d});
  f.dump().write("t.mag");
END Main.
