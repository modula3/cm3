(* $Id$ *)
MODULE MagTile;
IMPORT Word, MagCell, MagCellExtendable, Debug;
IMPORT MagTransform;
IMPORT MagLabel, MagLabelList;
IMPORT Fmt, MagSubCell;
IMPORT Text;
IMPORT MagLayerRect;
IMPORT TextTextTbl;

REVEAL 
  T = Public BRANDED Brand OBJECT 
    width, height : CARDINAL;
    cell : MagCell.Labelled;
  OVERRIDES
    dump := DumpT;
    makeFromMagCell  := MakeFromMagCell;
    makeHorizontally := MakeHorizontally;
    makeVertically   := MakeVertically;
    makeByRotateCCW  := MakeByRotateCCW;
    makeSideways     := MakeSideways;
    makeUpsideDown   := MakeUpsideDown;
    makeByOverlay    := MakeByOverlay;
    makeEmpty        := MakeEmpty;
    getHeight        := GetHeight;
    getWidth         := GetWidth;
    changeLabels     := ChangeLabels;
    changeLabelSet   := ChangeLabelSet;
  END;

CONST LabelName = "tile"; (* special label we search for *)

VAR mu := NEW(MUTEX);
VAR id := 0;
  
PROCEDURE NextId() : TEXT =
  BEGIN
    LOCK mu DO
      TRY
        RETURN Brand & "__" & Fmt.Int(id)
      FINALLY
        INC(id)
      END
    END
  END NextId;

PROCEDURE MakeEmpty(res : T; w, h : CARDINAL) : T = 
  BEGIN
    res.width := w; res.height := h; 
    res.cell := NEW(MagCell.Labelled).init(NextId());
    RETURN res
  END MakeEmpty;

PROCEDURE MakeFromMagCell(res : T; src : MagCell.Labelled) : T =
  VAR 
    labs := src.getLabels(LabelName);
    lab : MagLabel.T;
    trans : MagTransform.T;
  BEGIN
    (* search for special label *)
    IF MagLabelList.Length(labs) = 0 THEN
      Debug.Error("No label \"" & LabelName & 
        "\" found in cell \"" & src.getName() & "\"!")
    ELSIF MagLabelList.Length(labs) > 1 THEN
      Debug.Error("Multiple occurrences of label \"" & LabelName & 
        "\" found in cell \"" & src.getName() & "\"!")
    END;

    lab := labs.head;
    res.width := lab.rect.ur.x - lab.rect.ll.x;
    res.height := lab.rect.ur.y - lab.rect.ll.y;
    
    (* give proper transform that will transform cell so 
       that ll of label is at (0,0) *)

    trans := MagTransform.Unitary;
    trans.c := -lab.rect.ll.x;
    trans.f := -lab.rect.ll.y;
    
    res.cell := NEW(MagCell.Labelled).init(NextId());
    res.cell.addSub(MagSubCell.T { src, trans, "0", box := src.getBBox() });
    RETURN res
  END MakeFromMagCell;

PROCEDURE MakeHorizontally(self : T; READONLY a : ARRAY OF T) : T =
  VAR
    trans := MagTransform.Unitary;
  BEGIN
    <* ASSERT NUMBER(a) > 0 *>

    self.height := a[0].height;
    self.width := 0;
    
    self.cell := NEW(MagCell.Labelled).init(NextId());
    FOR i := FIRST(a) TO LAST(a) DO
      IF a[i].height # self.height THEN 
        Debug.Error("Height mismatch in tiles!")
      END;
      INC(self.width, a[i].width);

      self.cell.addSub(MagSubCell.T { a[i].cell, trans, Fmt.Int(i), 
                                      box := a[i].cell.getBBox() });
      INC(trans.c, a[i].width)
    END;
    RETURN self
  END MakeHorizontally;

PROCEDURE MakeVertically(self : T; READONLY a : ARRAY OF T) : T =
  VAR
    trans := MagTransform.Unitary;
  BEGIN
    <* ASSERT NUMBER(a) > 0 *>

    self.width := a[0].width;
    self.height := 0;
    
    self.cell := NEW(MagCell.Labelled).init(NextId());
    FOR i := FIRST(a) TO LAST(a) DO
      IF a[i].width # self.width THEN 
        VAR
          implicatedNames := a[i].cell.getName() & "("&a[i].cell.getDebugPath()&") " & self.cell.getName()&"("&self.cell.getDebugPath()&") ";
        BEGIN
          Debug.Error("Width mismatch in tiles!  Implicated names: " & implicatedNames)
        END
      END;
      INC(self.height, a[i].height);

      self.cell.addSub(MagSubCell.T { a[i].cell, trans, Fmt.Int(i), 
                                      box := a[i].cell.getBBox() });
      INC(trans.f, a[i].height)
    END;
    RETURN self
  END MakeVertically;

PROCEDURE MakeByRotateCCW(self : T; old : T) : T = 
  VAR 
    trans := MagTransform.CCW;
  BEGIN
    self.height := old.width;
    self.width := old.height;
    self.cell := NEW(MagCell.Labelled).init(NextId());
    trans.c := self.width;
    self.cell.addSub(MagSubCell.T { old.cell, trans, "0", 
                                    box := old.cell.getBBox()} );
    RETURN self
  END MakeByRotateCCW;

PROCEDURE MakeSideways(self : T; old : T) : T = 
  VAR 
    trans := MagTransform.Unitary;
  BEGIN
    self.height := old.height;
    self.width := old.width;
    self.cell := NEW(MagCell.Labelled).init(NextId());
    trans.a := -1;
    trans.c := self.width;
    self.cell.addSub(MagSubCell.T { old.cell, trans, "0", 
                                    box := old.cell.getBBox()} );
    RETURN self
  END MakeSideways;

PROCEDURE MakeUpsideDown(self : T; old : T) : T = 
  VAR 
    trans := MagTransform.Unitary;
  BEGIN
    self.height := old.height;
    self.width := old.width;
    self.cell := NEW(MagCell.Labelled).init(NextId());
    trans.e := -1;
    trans.f := self.height;
    self.cell.addSub(MagSubCell.T { old.cell, trans, "0", 
                                    box := old.cell.getBBox()} );
    RETURN self
  END MakeUpsideDown;

PROCEDURE MakeByOverlay(self : T; old1, old2 : T) : T = 
  BEGIN
    IF old1.height # old2.height OR old1.width # old2.width THEN
      Debug.Error("Size mismatch in MagTile.MakeByOverlay!")
    END;
    self.height := old1.height;
    self.width := old1.width;
    self.cell := NEW(MagCell.Labelled).init(NextId());
    self.cell.addSub(MagSubCell.T { old1.cell, MagTransform.Unitary, "0", 
                                    box := old1.cell.getBBox()} );
    self.cell.addSub(MagSubCell.T { old2.cell, MagTransform.Unitary, "1", 
                                    box := old2.cell.getBBox()} );
    RETURN self
  END MakeByOverlay;

PROCEDURE DumpT(self : T; name : TEXT) : MagCell.T = 
  BEGIN 
    IF name = NIL THEN
      name := NextId() 
    END;
    RETURN NEW(MagCell.Labelled).initByFlattening(name,self.cell) 
  END DumpT;
  
PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN a = b END Equal;

PROCEDURE Hash(a : T) : Word.T = 
  BEGIN RETURN Text.Hash(a.cell.getName()) END Hash;

PROCEDURE GetHeight(self : T) : CARDINAL =
  BEGIN RETURN self.height END GetHeight;

PROCEDURE GetWidth(self : T) : CARDINAL = 
  BEGIN RETURN self.width END GetWidth;


VAR 
  nu := NEW(MUTEX);
  curCell : MagCell.Labelled;
  curS : TextTextTbl.T;
  
PROCEDURE LabelProc(label : MagLabel.T) =
  VAR
    newName : TEXT;
  BEGIN
    IF curS.get(label.name,newName) THEN
      label.name := newName
    END;
    curCell.addLabel(label)
  END LabelProc;

PROCEDURE DummyRectProc(rect : MagLayerRect.T) =
  BEGIN 
    curCell.addLayerRect(rect)
  END DummyRectProc;

PROCEDURE ChangeLabels(self : T; a, b : TEXT) : T =
  VAR
    tbl := NEW(TextTextTbl.Default).init();
  BEGIN
    EVAL tbl.put(a,b);
    RETURN self.changeLabelSet(tbl)
  END ChangeLabels;

PROCEDURE ChangeLabelSet(self : T; labels : TextTextTbl.T) : T =
  BEGIN
    LOCK nu DO
      curCell := NEW(MagCell.Labelled);
      curS := labels;
      self.cell.flatMapAllNoArgs(DummyRectProc,LabelProc)
    END;
    RETURN NEW(T).makeFromMagCell(curCell)
  END ChangeLabelSet;


BEGIN END MagTile.
