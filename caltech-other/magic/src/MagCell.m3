(* $Id$ *)

MODULE MagCell;
IMPORT Word;
IMPORT Process;
IMPORT MagCellExtendable;
FROM MagCellExtendable IMPORT NoSuchSubcell;
IMPORT MagCellReader;
IMPORT MagPath, TextList;
IMPORT TextCellTbl;
IMPORT Rd, FileRd;
IMPORT OSError, Thread;
IMPORT Text, TextUtils, TextSet, TextSetDef;
IMPORT MagSubCell AS SubCell, TextSubCellTbl;
IMPORT Fmt, Debug;
IMPORT Wr, Stdio;
IMPORT MagTimeStamp, Time, MagRect;
IMPORT MagLayer AS Layer, TextMagLayerTbl AS TextLayerTbl;
IMPORT MagArrayData, MagArrayDataClass, MagTransform;
IMPORT MagLabel, MagLabelList AS LabelList;
IMPORT RTAllocator; (* for object "cloning" *)
IMPORT MagLayerRect;
IMPORT MagLayerRectSet, MagLayerRectSetDef;
IMPORT MagRouteLayer AS RouteLayer;
IMPORT FileWr;
IMPORT MagMergeDB, MagLayerSet AS LayerSet, MagLayerSetDef AS LayerSetDef;
IMPORT Rect_LayerSetTbl;
IMPORT RegularFile, FS, Atom;
IMPORT MagLayerRectList AS LayerRectList;
IMPORT TextCellInstanceTbl, MagCellFlatten;
IMPORT RefList;
IMPORT MagCellList;
IMPORT MagRectSet, MagRectSetDef;
IMPORT MagCellSet, MagCellSetDef;

VAR DoDebug := Debug.DebugThis("MagCell");

REVEAL
  T = MagCellExtendable.Public BRANDED Brand OBJECT
    touched := FALSE;
    name : TEXT := "--unknown--";
    subCells : TextSubCellTbl.T := NIL;
    timeStamp : MagTimeStamp.T := MagTimeStamp.Epoch;
    rectList : LayerRectList.T := NIL;
    sessions : RefList.T := NIL;
    haveBBox := FALSE;
    bbox : MagRect.T;
    flatInstances : TextCellInstanceTbl.T := NIL;
    path := ""; (* path for DEBUGGING ONLY *)
    parents : MagCellList.T := NIL;
  METHODS 
    (* map all rects at THIS level (does not recurse) *)
    mapMyRects(p : PROCEDURE(r : MagLayerRect.T) RAISES { Wr.Failure, Thread.Alerted}) RAISES {Wr.Failure, Thread.Alerted} := MapMyRects;
    mapMyLabels(p : PROCEDURE(l : MagLabel.T) RAISES { Wr.Failure, Thread.Alerted}) RAISES {Wr.Failure, Thread.Alerted} := DummyMapMyLabels;
    mapMySubcells(p : PROCEDURE(READONLY s : SubCell.T) RAISES { Wr.Failure, Thread.Alerted}) RAISES {Wr.Failure, Thread.Alerted} := MapMySubcells;

    mapMyRectsWithArg(p : PROCEDURE(r : MagLayerRect.T; q : REFANY) RAISES { Wr.Failure, Thread.Alerted}; q : REFANY) RAISES {Wr.Failure, Thread.Alerted} := MapMyRectsWithArg;
    mapMyLabelsWithArg(p : PROCEDURE(l : MagLabel.T; q : REFANY) RAISES { Wr.Failure, Thread.Alerted}; q : REFANY) RAISES {Wr.Failure, Thread.Alerted} := DummyMapMyLabelsWithArg;
  OVERRIDES
    empty := Empty;
    havePaint := HavePaint;
    haveSubCells := HaveSubCells;
    haveLabels := HaveLabels;
    debugDumpData := DebugDumpData;
    getDebugPath := GetPath;
    lookup := Lookup;
    init := Init;
    initByFlattening := InitByFlattening;
    getBBox := GetBBox;
    addRect := AddRect;
    addLayerRect := AddLayerRect;
    addLayerRectSet := AddLayerRectSet;
    addSub := AddSub;
    delSub := DelSub;
    addLabel := DontAddLabel;
    setTimeStamp := SetTimeStamp;
    flatClipMap := FlatClipMap;
    flatClipMap2 := FlatClipMap2;
    flatMapAll := FlatMapAll;
    flatMapAllNoArgs := FlatMapAllNoArgs;
    flatMapLabels := FlatMapLabels;
    subCellMap := SubCellMap;
    getSubCell := GetSubCell;
    getName := GetName;
    writeToWriter := WriteToWriter;
    write := Write;
    writeRecursivelyToDirectory := WriteRecursively;
    flatten := Flatten;

    flattenSubOneLevel := FlattenSubOneLevel;

    findLabels := FindLabels;

    (* sessions *)
    newSession := NewSession;
    sessAddRect := SessAddRect;
    sessAddLayerRect := SessAddLayerRect;
    sessAddLayerRectSet := SessAddLayerRectSet;
    rollbackSession := RollbackSession;

    (* rects *)
    layerRectsBbox := LayerRectsBbox;
    layerSetRectsBbox := LayerSetRectsBbox;
    
    tightenBBox := TightenBBox;
  END;

  Labelled = 
    MagCellExtendable.PublicLabelled BRANDED "Labelled " & Brand OBJECT
    lblList : LabelList.T := NIL;
  OVERRIDES
    addLabel := AddLabel;
    getLabels := GetLabels;
    mapMyLabels := MapMyLabels;
    mapMyLabelsWithArg := MapMyLabelsWithArg;
  END;

  Writer = MagCellExtendable.PublicCellWriter BRANDED Brand & " Writer" OBJECT 
    valid := TRUE; (* changes to FALSE if session is deleted *)
    for : T;
    rects : LayerRectList.T := NIL;
    myID : Word.T;
  OVERRIDES
    id := WriterID;
    iterate := WrIterate;
    rectSet := WrRectSet;
  END;

TYPE Writer = MagCellExtendable.CellWriter;

REVEAL
  MagCellExtendable.LayerRectIterator = MagCellExtendable.PublicLayerRectIterator BRANDED Brand & " LayerRectIterator" 
                           OBJECT
    p : LayerRectList.T;
  OVERRIDES
    next := LRINext
  END;

PROCEDURE Empty(t : T) : BOOLEAN =
  BEGIN
    RETURN NOT HaveSubCells(t) AND NOT HaveLabels(t) AND NOT HavePaint(t)
  END Empty;

PROCEDURE HaveSubCells(t : T) : BOOLEAN =
  BEGIN
    (* subcells *)
    IF t.subCells # NIL AND t.subCells.size() # 0 THEN RETURN TRUE END;
    
    RETURN FALSE
  END HaveSubCells;

PROCEDURE HaveLabels(t : T) : BOOLEAN =
  BEGIN
    IF ISTYPE(t,Labelled) AND NARROW(t,Labelled).lblList # NIL THEN
      RETURN TRUE
    END;
    RETURN FALSE
  END HaveLabels;

PROCEDURE HavePaint(t : T) : BOOLEAN =
  BEGIN
    (* rects *)
    IF t.rectList # NIL THEN RETURN TRUE END;

    (* sessions *)
    VAR
      p := t.sessions;
    BEGIN
      WHILE p # NIL DO
        WITH s = NARROW(p.head,Writer) DO
          <* ASSERT s.valid *>
          IF s.rects # NIL THEN RETURN TRUE END
        END;
        p := p.tail
      END
    END;

    (* ahhh.. it's empty! *)
    RETURN FALSE
  END HavePaint;

PROCEDURE WrIterate(self : Writer) : MagCellExtendable.LayerRectIterator =
  BEGIN RETURN NEW(MagCellExtendable.LayerRectIterator, p := self.rects) END WrIterate;

PROCEDURE WrRectSet(self : Writer) : MagLayerRectSet.T =
  VAR
    res := NEW(MagLayerRectSetDef.T).init();
    r : MagLayerRect.T;
    iter := self.iterate();
  BEGIN
    WHILE iter.next(r) DO EVAL res.insert(r) END;
    RETURN res
  END WrRectSet;

PROCEDURE LRINext(self : MagCellExtendable.LayerRectIterator; 
                  VAR rect : MagLayerRect.T) : BOOLEAN =
  BEGIN
    IF self.p = NIL THEN RETURN FALSE END;
    TRY
      rect := self.p.head;
      RETURN TRUE
    FINALLY
      self.p := self.p.tail
    END
  END LRINext;

PROCEDURE GetPath(self : T) : TEXT = BEGIN RETURN self.path END GetPath;

VAR
  writerIDmu := NEW(MUTEX);
  nextWriterID : Word.T := 1;

PROCEDURE WriterID(w : Writer) : Word.T = BEGIN RETURN w.myID END WriterID;

PROCEDURE NewSession(self : T) : Writer =
  VAR
    wr := NEW(Writer, for := self);
  BEGIN 
    LOCK writerIDmu DO
      wr.myID := nextWriterID;
      INC(nextWriterID)
    END;

    self.sessions := RefList.Cons(wr,self.sessions);
    RETURN wr
  END NewSession;

PROCEDURE MapMyRects(self : T; proc : PROCEDURE(r : MagLayerRect.T)RAISES {Wr.Failure, Thread.Alerted}) RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    (* first do the non-optional rects *)
    VAR 
      p := self.rectList;
    BEGIN
      WHILE p # NIL DO proc(p.head); p := p.tail END
    END;

    (* now the sessions *)
    VAR
      s := self.sessions;
    BEGIN
      WHILE s # NIL DO 
        VAR
          p : LayerRectList.T := NARROW(s.head,Writer).rects;
        BEGIN
          WHILE p # NIL DO 
            proc(p.head); 
            p := p.tail 
          END
        END;
        s := s.tail
      END
    END
  END MapMyRects;

PROCEDURE MapMyLabels(self : Labelled; proc : PROCEDURE(l : MagLabel.T) RAISES { Wr.Failure, Thread.Alerted}) RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    p := self.lblList;
  BEGIN
    WHILE p # NIL DO proc(p.head); p := p.tail END
  END MapMyLabels;

PROCEDURE MapMySubcells(t : T; proc : PROCEDURE(READONLY s : SubCell.T) RAISES { Wr.Failure, Thread.Alerted}) RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    iter := t.subCells.iterate();
    s : SubCell.T;
    id : TEXT;
  BEGIN
    WHILE iter.next(id,s) DO proc(s) END
  END MapMySubcells;


PROCEDURE MapMyRectsWithArg(self : T; proc : PROCEDURE(r : MagLayerRect.T; q : REFANY)RAISES {Wr.Failure, Thread.Alerted}; arg : REFANY) RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    (* first do the non-optional rects *)
    VAR 
      p := self.rectList;
    BEGIN
      WHILE p # NIL DO 
        IF DoDebug THEN
          Debug.Out("MagCell.MapMyRectsWithArg: processing non-optional " &
            MagLayerRect.Format(p.head))
        END;
        proc(p.head,arg); p := p.tail 
      END
    END;

    (* now the sessions *)
    VAR
      s := self.sessions;
    BEGIN
      WHILE s # NIL DO 
        VAR
          p : LayerRectList.T := NARROW(s.head,Writer).rects;
        BEGIN
          WHILE p # NIL DO 
            IF DoDebug THEN
              Debug.Out("MagCell.MapMyRectsWithArg: processing session rect " &
                MagLayerRect.Format(p.head))
            END;
            proc(p.head,arg); 
            p := p.tail 
          END
        END;
        s := s.tail
      END
    END
  END MapMyRectsWithArg;

PROCEDURE MapMyLabelsWithArg(self : Labelled; proc : PROCEDURE(l : MagLabel.T; q : REFANY) RAISES { Wr.Failure, Thread.Alerted}; arg : REFANY) RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    p := self.lblList;
  BEGIN
    WHILE p # NIL DO proc(p.head,arg); p := p.tail END
  END MapMyLabelsWithArg;

<*NOWARN*>PROCEDURE DummyMapMyLabels(self : T; proc : PROCEDURE(l : MagLabel.T) RAISES { Wr.Failure, Thread.Alerted}) RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN 
    (* skip *)
  END DummyMapMyLabels;

<*NOWARN*>PROCEDURE DummyMapMyLabelsWithArg(self : T; proc : PROCEDURE(l : MagLabel.T; q : REFANY) RAISES { Wr.Failure, Thread.Alerted}; q : REFANY) RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN 
    (* skip *)
  END DummyMapMyLabelsWithArg;

(* delete wr from sessions *)
PROCEDURE RollbackSession(self : T; wr : Writer) = 
  VAR
    p := self.sessions;
    q : RefList.T := NIL;
    foundIt := FALSE;
  BEGIN
    <* ASSERT wr.for = self AND RefList.Member(p, wr) *>
    Debug.Out("MagCell.RollbackSession: rolling back session on cell id \"" &
      self.name & "\".");
    WHILE p # NIL DO
      IF p.head # wr THEN
        q := RefList.Cons(p.head,q)
      ELSE
        foundIt := TRUE
      END;
      p := p.tail
    END;
    <* ASSERT foundIt *>
    self.sessions := q;
    wr.valid := FALSE;

    (* what about bbox? *)

  END RollbackSession;

PROCEDURE DebugDumpData(t : T) =
  BEGIN
    Debug.Out("MagCell.DebugDumpData: \"" & t.name & "\":");
    Debug.Out("timestamp: " & Fmt.Int(t.timeStamp));
    Debug.Out("subcells:");
    VAR
      iter := t.subCells.iterate();
      n : TEXT;
      sub : SubCell.T;
    BEGIN
      WHILE iter.next(n,sub) DO
        Debug.Out("name: \"" & n & "\" id: \"" & sub.useId & "\" name: \"" & sub.c.name & "\" arrayed: " & Fmt.Bool(sub.array # NIL) )
      END
    END;

    VAR
      p := t.rectList;
    BEGIN
      Debug.Out("FIXED RECTS:");
      WHILE p # NIL DO
        Debug.Out(MagRect.Format(p.head.rect) & " " & NARROW(p.head.layer,RouteLayer.T).name);
        p := p.tail
      END
    END;

    VAR
      q := t.sessions;
    BEGIN
      WHILE q # NIL DO
        Debug.Out("SESSION RECTS");
        <* ASSERT NARROW(q.head,Writer).valid *>
        VAR
          sp := NARROW(q.head,Writer).rects;
        BEGIN
          WHILE sp # NIL DO
            Debug.Out(MagRect.Format(sp.head.rect) & " " & NARROW(sp.head.layer,RouteLayer.T).name);
            sp := sp.tail
          END
        END;
        q := q.tail
      END
    END
  END DebugDumpData;

(* this is global.  In a later release, we might make all the parsing *)
(* handled by an object.  (It might be a descendant of a yacc-generated *)
(* "parser object")  But for now, the parser already uses globals, so *)
(* the use of globals here won't hurt that much. *)

VAR tbl := NEW(TextCellTbl.Default).init();

PROCEDURE Canonicalize(line : TEXT; doIt : BOOLEAN := TRUE) : TEXT =
  BEGIN
    IF NOT doIt THEN
      RETURN line
    ELSE
      VAR
        resArr := NEW(REF ARRAY OF CHAR, Text.Length(line));
      BEGIN
        FOR i := 0 TO LAST(resArr^) DO
          WITH c = Text.GetChar(line,i) DO
            IF    c = '/' THEN resArr[i] := '.' 
            ELSIF c = '[' THEN resArr[i] := '('
            ELSIF c = ']' THEN resArr[i] := ')'
            ELSE resArr[i] := c 
            END
          END 
        END;
        RETURN Text.FromChars(resArr^)
      END
    END
  END Canonicalize;

PROCEDURE GetLabels(self : Labelled; name : TEXT) : LabelList.T = 
  VAR
    res : LabelList.T := NIL;
    l := self.lblList;
  BEGIN
    WHILE l # NIL DO
      IF Text.Equal(Canonicalize(name), Canonicalize(l.head.name)) THEN
        res := LabelList.Cons(l.head, res)
      END;
      l := l.tail
    END;
    RETURN res
  END GetLabels;

PROCEDURE GetName(self : T) : TEXT = BEGIN RETURN self.name END GetName;

PROCEDURE GetSubCell(self : T; useId : TEXT; VAR sub : SubCell.T) : BOOLEAN =
  BEGIN RETURN self.subCells.get(useId, sub) END GetSubCell;

PROCEDURE GetBBox(self : T) : MagRect.T = 
  BEGIN 
    <* ASSERT MagRect.IsProper(self.bbox) *>
    RETURN self.bbox 
  END GetBBox;

PROCEDURE Init(self : T; name : TEXT) : T = 
  BEGIN 
    self.name := name;
    self.subCells := NEW(TextSubCellTbl.Default).init();
    RETURN self 
  END Init;

PROCEDURE SetTimeStamp(self : T; timeStamp : MagTimeStamp.T)=
  BEGIN
(*
    IF self.timeStamp # MagTimeStamp.Epoch THEN
      RAISE MagCellExtendable.ReadOnlyTimeStamp
    END;
*)
    self.timeStamp := timeStamp
  END SetTimeStamp;
  
PROCEDURE AddSub(self : T; READONLY sub : SubCell.T) = 
  VAR 
    box : MagRect.T;
    x : BOOLEAN;
  BEGIN 
    (* flush instances cache *)
    self.flatInstances := NIL;

    IF sub.array # NIL THEN
      box := MagArrayData.ComputeArrayBBox(sub.box,sub.transform,sub.array^)
    ELSE
      box := MagTransform.Rect(sub.box, sub.transform)
    END;
    ExtendBBox(self, box);
    x := self.subCells.put(sub.useId, sub);

    (* record parent relationship *)
    sub.c.parents := MagCellList.Cons(self,sub.c.parents);

    IF x THEN
      Process.Crash("Adding duplicate use ID \"" & sub.useId & "\"!")
    END
  END AddSub;

PROCEDURE DelSub(t : T; useId : TEXT) RAISES { NoSuchSubcell } =
  VAR
    iter := t.subCells.iterate();
    thisId : TEXT;
    thisSub : SubCell.T;
    done := FALSE;
  BEGIN
    t.touched := TRUE;
    WHILE iter.next(thisId, thisSub) DO
      IF Text.Equal(thisId,useId) THEN
        done := TRUE; EXIT
      END
    END;

    IF NOT done THEN RAISE NoSuchSubcell END;

    (* delete it *)
    EVAL t.subCells.delete(thisId,thisSub)
  END DelSub;

PROCEDURE FlattenSubOneLevel(t : T; useId : TEXT;
                             okToRename : TextSet.T) RAISES { NoSuchSubcell } =

  PROCEDURE RP(r : MagLayerRect.T) =
    VAR
      newRect := r;
    BEGIN
      newRect.rect := MagTransform.Rect(r.rect, sub.transform);
      t.addLayerRect(newRect)
    END RP;

  PROCEDURE LP(l : MagLabel.T) =
    VAR
      newLabel := l;
    BEGIN
      newLabel.rect := MagTransform.Rect(l.rect, sub.transform);
      t.addLabel(newLabel)
    END LP;

  <* FATAL Wr.Failure, Thread.Alerted *>
  VAR
    sub : SubCell.T;
    haveIt := t.getSubCell(useId, sub);
  BEGIN
    IF NOT haveIt THEN RAISE NoSuchSubcell END;

    t.delSub(useId);
    
    (* labels *)
    sub.c.mapMyLabels(LP);
    
    (* rects *)
    sub.c.mapMyRects(RP);

    (* subcells *)
    LOCK fs1Mu DO
      fs1okToRename := okToRename;
      fs1subtran := sub.transform;
      fs1t := t;
      sub.c.mapMySubcells(SP)
    END;

  END FlattenSubOneLevel;

PROCEDURE SP(READONLY s : SubCell.T) = 
  VAR
    tran : MagTransform.T;
    sub2 := s;
    dummy : SubCell.T;
  BEGIN
    Debug.Out("SP Mapping subcell of type \"" & s.c.name & "\", useId \"" &
      s.useId & "\" subtran="& MagTransform.Format(fs1subtran) & 
      " s.transform=" & MagTransform.Format(s.transform));

    IF fs1t.getSubCell(s.useId, dummy) THEN
      IF fs1okToRename.member(s.useId) THEN
        VAR
          attempt := 1;
          attemptName := s.useId & "_" & Fmt.Int(attempt);
        BEGIN
          WHILE fs1t.getSubCell(attemptName,dummy) DO
            INC(attempt);
            attemptName := s.useId & "_" & Fmt.Int(attempt)
          END;

          sub2.useId := attemptName;

          Debug.Out("SP found already existing subcell with same useId \"" & 
            s.useId & "\", renaming to \"" & sub2.useId & "\"")
        END
      ELSE
        Debug.Error("MagCell.SP: Duplicate use ID \"" & 
          s.useId & "\" found, not OK to rename.")
      END
    END;
    
    tran := MagTransform.Compose(fs1subtran, s.transform);
    sub2.transform := tran;
    fs1t.addSub(sub2)
  END SP;

VAR
  fs1Mu := NEW(MUTEX);
  fs1t : T;
  fs1okToRename : TextSet.T;
  fs1subtran : MagTransform.T;


PROCEDURE DontAddLabel(<*UNUSED*>self: T;<*UNUSED*>READONLY lab : MagLabel.T) =
  BEGIN END DontAddLabel;
    
PROCEDURE AddLabel(self : Labelled; READONLY lab : MagLabel.T) =
  BEGIN
    self.touched := TRUE;
    ExtendBBox(self, lab.rect);
    <* ASSERT lab.name # NIL *>
    self.lblList := LabelList.Cons(lab, self.lblList) 
  END AddLabel;
    
PROCEDURE AddRect(res : T; READONLY rect : MagRect.T; l : Layer.T) =
  BEGIN
    res.touched := TRUE;
    ExtendBBox(res, rect);
    IF l # NIL THEN
      res.rectList := LayerRectList.Cons( MagLayerRect.T { rect, l }, 
                                          res.rectList )
    END
  END AddRect;

PROCEDURE AddLayerRect(self : T; READONLY lRect : MagLayerRect.T) =
  BEGIN self.addRect(lRect.rect, lRect.layer) END AddLayerRect;

PROCEDURE AddLayerRectSet(self : T; layerRectSet : MagLayerRectSet.T) =
  VAR
    iter := layerRectSet.iterate();
    l : MagLayerRect.T;
  BEGIN 
    WHILE iter.next(l) DO self.addLayerRect(l) END 
  END AddLayerRectSet;

(* session versions of all the addrect routines *)
PROCEDURE SessAddRect(res : T; wr : Writer; READONLY rect : MagRect.T; l : Layer.T) =
  BEGIN
    res.touched := TRUE;
    <* ASSERT wr.for = res AND RefList.Member(res.sessions,wr) AND wr.valid *>
    ExtendBBox(res, rect);
    IF l # NIL THEN
      wr.rects := LayerRectList.Cons( MagLayerRect.T { rect, l }, 
                                          wr.rects )
    END
  END SessAddRect;

PROCEDURE SessAddLayerRect(self : T; wr : Writer; READONLY lRect : MagLayerRect.T) =
  BEGIN self.sessAddRect(wr, lRect.rect, lRect.layer) END SessAddLayerRect;

PROCEDURE SessAddLayerRectSet(self : T; wr : Writer; layerRectSet : MagLayerRectSet.T) =
  VAR
    iter := layerRectSet.iterate();
    l : MagLayerRect.T;
  BEGIN 
    WHILE iter.next(l) DO self.sessAddLayerRect(wr,l) END 
  END SessAddLayerRectSet;

PROCEDURE ExtendBBox(res : T; rect : MagRect.T) =
  VAR
    oldBbox := res.bbox;
    oldHaveBBox := res.haveBBox;
  BEGIN
    res.touched := TRUE;
    IF res.haveBBox THEN
      res.bbox := MagRect.Union(rect, res.bbox)
    ELSE
      res.bbox := rect;
      res.haveBBox := TRUE
    END;
    IF DoDebug THEN
      Debug.Out("MagCell.ExtendBBox: Extending bbox of \"" & res.name & "\" by " & 
        MagRect.Format(rect) & " to " & MagRect.Format(res.bbox), 200)
    END;
    <* ASSERT MagRect.IsProper(res.bbox) *>

    (* now check parents IF we extended current cell's bbox *)
    IF NOT oldHaveBBox OR oldBbox # res.bbox THEN
      IF DoDebug THEN
        Debug.Out("MagCell.ExtendBBox: Extended bbox of \"" & res.name & "\": checking potential parents.")
      END;
      VAR
        pp := res.parents;
      BEGIN
        WHILE pp # NIL DO
          IF DoDebug THEN
            Debug.Out("MagCell.ExtendBBox: Extended bbox of \"" & res.name & "\": checking parent \""&pp.head.name&"\"." )
          END;
          VAR
            ppc := pp.head.subCells.iterate();
            s : SubCell.T;
            dummy : TEXT;
          BEGIN
            (* transform bbox of subcell into parents coordinate system *)
            WHILE ppc.next(dummy,s) DO
              ExtendBBox(pp.head, 
                         MagTransform.Rect(s.c.getBBox(),s.transform))
            END
          END;
          pp := pp.tail
        END
      END
    END
  END ExtendBBox;

PROCEDURE Lookup(self : T; name : TEXT; layerDB : TextLayerTbl.T; fillInLayers := FALSE; quiet := FALSE) : T RAISES { NotFound, Thread.Alerted, Rd.Failure, SyntaxError } =

  PROCEDURE DiskLookup() : T RAISES { NotFound, Thread.Alerted, Rd.Failure, SyntaxError } =
    CONST
      PrintWidth = 79;
    VAR
      magPath := MagPath.Get();
      tryPath : TEXT;
      rd : Rd.T;
      res : T;
    BEGIN
      
      (* names that begin with slash are/can be absolute names *)
      IF Text.GetChar(name,0) = '/' THEN
        magPath := TextList.Cons("/",magPath)
      END;

      WHILE magPath # NIL DO
        tryPath := magPath.head & "/" & name & ".mag";
        TRY 
          
          (* keep the user busy with some output *)
          IF NOT quiet THEN
            TRY
              Wr.PutText(Stdio.stderr, "\r" & 
                Fmt.Pad(Text.Sub(tryPath,0,PrintWidth),PrintWidth,
                  align := Fmt.Align.Left) & "");
              Wr.Flush(Stdio.stderr)
            EXCEPT Wr.Failure => (* skip *) 
            END;
          END;          
          rd := FileRd.Open(tryPath);
          res := MagCellReader.ReadFromRd(NARROW(RTAllocator.NewTraced(typecode),T).init(name), rd,
                                          tryPath, layerDB, fillInLayers, quiet);
          Rd.Close(rd);
          res.path := tryPath;
          res.touched := FALSE;
          RETURN res
        EXCEPT
          OSError.E => (* skip *)
        END;
        magPath := magPath.tail
      END;
      RAISE NotFound(name)
    END DiskLookup;

  VAR
    typecode := TYPECODE(self);
    res : T;
  BEGIN
    IF NOT tbl.get(name,res) THEN 
      res := DiskLookup();
      EVAL tbl.put(name,res)
    END;
    RETURN res 
  END Lookup;

(* we use parentheses instead of square brackets when we have CAST arrays
   that are not Magic arrays.. 

   Also semicolons become commas (for multi-D arrays) *)

PROCEDURE Reformat(str : TEXT) : TEXT =
  BEGIN
    RETURN TextUtils.ReplaceChar(TextUtils.ReplaceChar(TextUtils.ReplaceChar(str,'(','['),
                                 ')',']'),';',',')
  END Reformat;

PROCEDURE SubCellMap(self : T; cellProc : CellProc; args : REFANY := NIL) RAISES ANY =

  PROCEDURE Recurse(cell : T; path : TEXT; transform : MagTransform.T;
                    parentPath : TEXT) RAISES ANY =
    BEGIN

      (* call the processor *)
      cellProc(cell, path, transform, parentPath, args);

      (* do subcells *)

      VAR
        cellp := cell.subCells.iterate();
        c : SubCell.T;
        id : TEXT;
        pathExtended : TEXT;
      BEGIN
        (* avoid having a leading period... *)
        IF parentPath = NIL THEN
          pathExtended := path
        ELSE
          pathExtended := path & "."
        END;

        WHILE cellp.next(id, c) DO
          IF c.array = NIL THEN
            Recurse(c.c, pathExtended & Reformat(id), 
                    MagTransform.Compose(transform, c.transform), path)
          ELSE
            VAR
              tl := MagArrayDataClass.ToTransformList(c.transform,
                                                      c.array^);
            BEGIN
              WHILE tl # NIL DO
                VAR
                  subXform := MagTransform.Compose(transform, tl.head.transform);
                BEGIN
                  Recurse(c.c, pathExtended & id & 
                    MagArrayDataClass.FormatIndex(tl.head), subXform, path)
                END;
                tl := tl.tail
              END
            END
          END
        END
      END
    END Recurse;

  BEGIN Recurse(self, "", MagTransform.Unitary, NIL) END SubCellMap;

TYPE Arg2 = REF RECORD args : REFANY; proc : LayerRectProc END;

PROCEDURE RectProcBetween(rect : MagRect.T; layer : Layer.T; args : REFANY) =
  VAR
    arg2 := NARROW(args, Arg2);
  BEGIN
    arg2.proc(MagLayerRect.T { rect, layer } , arg2.args)
  END RectProcBetween;

PROCEDURE FlatClipMap2(self : T; proc : LayerRectProc;
                 args     : REFANY       := NIL;
                 clip     : REF MagRect.T := NIL ) =
  VAR arg2 := NEW(Arg2, args := args, proc := proc);
  BEGIN
    FlatClipMap(self, RectProcBetween, arg2, clip)
  END FlatClipMap2;

TYPE 
  MapType = REF RECORD 
    transform : MagTransform.T; 
    args : REFANY;
    rectProc : RectProc
  END;

PROCEDURE FlatClipMapRecurseMapper(r : MagLayerRect.T; tp : REFANY) =
  VAR
    mapTransform := NARROW(tp,MapType).transform;
    mapArgs := NARROW(tp,MapType).args;
  BEGIN
    NARROW(tp,MapType).rectProc(MagTransform.Rect(r.rect,mapTransform), 
             r.layer, mapArgs)
  END FlatClipMapRecurseMapper;

PROCEDURE FlatClipMap(self : T; 
                      rectProc : RectProc;
                      args : REFANY;
                      clip : REF MagRect.T) =

  PROCEDURE Recurse(cell : T; transform : MagTransform.T) =
    BEGIN
      IF DoDebug AND clip # NIL THEN
        Debug.Out("MagCell.FlatClipMap.Recurse(\""&cell.name&"\",,,"&MagRect.Format(clip^)&") transform="&MagTransform.Format(transform))
      ELSIF DoDebug AND clip = NIL THEN
        Debug.Out("MagCell.FlatClipMap.Recurse(\""&cell.name&"\",,,NIL) transform="&MagTransform.Format(transform))
      END;

      (* do "local" rects *)

      IF clip # NIL AND NOT MagRect.Overlap(MagTransform.Rect(cell.bbox, 
                                                              transform), 
                                            clip^) THEN 
        IF DoDebug THEN
          Debug.Out("FlatClipMap.Recurse returning because transformed cell.bbox = "& 
            MagRect.Format(MagTransform.Rect(cell.bbox,transform)) &
            " and clip = " & MagRect.Format(clip^))
        END;
        RETURN 
      END;

      <* FATAL Wr.Failure, Thread.Alerted *>
      VAR
        arg := NEW(MapType, transform := transform, 
                            args := args, 
                            rectProc := rectProc);
      BEGIN 
        cell.mapMyRectsWithArg(FlatClipMapRecurseMapper,arg) 
      END;

      (* do subcells *)

      VAR
        cellp := cell.subCells.iterate();
        c : SubCell.T;
        tDummy : TEXT;
      BEGIN
        WHILE cellp.next(tDummy, c) DO
          IF c.array = NIL THEN
            Recurse(c.c, MagTransform.Compose(transform, c.transform))
          ELSE
            (* new version, iterator : *)
            VAR
              iter := MagArrayDataClass.ToTransformIterator(c.transform,
                                                            c.array^);
              t : MagTransform.T;
            BEGIN
              WHILE iter.next(t) DO
                VAR
                  subXform := MagTransform.Compose(transform, 
                                                   t);
                BEGIN
                  Recurse(c.c, subXform)
                END
              END
            END
          END 
        END
      END
    END Recurse;

  BEGIN 
    IF DoDebug AND clip # NIL THEN
      Debug.Out("MagCell.FlatClipMap(\""&self.name&"\",,,"&MagRect.Format(clip^)&")")
    ELSIF DoDebug AND clip = NIL THEN
      Debug.Out("MagCell.FlatClipMap(\""&self.name&"\",,,NIL)")
    END;
    Recurse(self, MagTransform.Unitary) 
  END FlatClipMap;

PROCEDURE FlatMapAllNoArgs(self : T; 
                     rectProc : LayerRectProc2;
                     labelProc : LabelProc2) =

  PROCEDURE Recurse(cell : T; transform : MagTransform.T) =
    BEGIN

      (* do "local" rects *)

      PROCEDURE RectMapper(r : MagLayerRect.T) =
        BEGIN
          rectProc(MagLayerRect.T { MagTransform.Rect(r.rect,transform), 
                                    r.layer } )
        END RectMapper;

      PROCEDURE LabelMapper(l : MagLabel.T) = 
        BEGIN
          l.rect := MagTransform.Rect(l.rect,transform); (* ok, it's ugly *)
          labelProc(l)
        END LabelMapper;

      <* FATAL Wr.Failure, Thread.Alerted *>
      BEGIN 
        IF rectProc # NIL THEN cell.mapMyRects(RectMapper) END;
        IF labelProc # NIL THEN cell.mapMyLabels(LabelMapper) END
      END;

      (* do subcells *)

      IF cell.subCells # NIL THEN
        VAR
          cellp := cell.subCells.iterate();
          c : SubCell.T;
          tDummy : TEXT;
        BEGIN
          WHILE cellp.next(tDummy, c) DO
            IF c.array = NIL THEN
              Recurse(c.c, MagTransform.Compose(transform, c.transform))
            ELSE
              (* new version, iterator : *)
              VAR
                iter := MagArrayDataClass.ToTransformIterator(c.transform,
                                                              c.array^);
                t : MagTransform.T;
              BEGIN
                WHILE iter.next(t) DO
                  VAR
                    subXform := MagTransform.Compose(transform, 
                                                     t);
                  BEGIN
                    Recurse(c.c, subXform)
                  END
                END
              END
            END 
          END
        END
      END

    END Recurse;

  BEGIN Recurse(self, MagTransform.Unitary) END FlatMapAllNoArgs;

PROCEDURE FlatMapAll(self : T; 
                     rectProc : LayerRectProc;
                     labelProc : LabelProc;
                     args : REFANY) =

  PROCEDURE Recurse(cell : T; transform : MagTransform.T) =
    BEGIN

      (* do "local" rects *)

      PROCEDURE RectMapper(r : MagLayerRect.T) =
        BEGIN
          rectProc(MagLayerRect.T { MagTransform.Rect(r.rect,transform), 
                                    r.layer }, args )
        END RectMapper;

      PROCEDURE LabelMapper(l : MagLabel.T) = 
        BEGIN
          l.rect := MagTransform.Rect(l.rect,transform); (* ok, it's ugly *)
          labelProc(l, args)
        END LabelMapper;

      <* FATAL Wr.Failure, Thread.Alerted *>
      BEGIN 
        IF rectProc # NIL THEN cell.mapMyRects(RectMapper) END;
        IF labelProc # NIL THEN cell.mapMyLabels(LabelMapper) END
      END;

      (* do subcells *)

      IF cell.subCells # NIL THEN
        VAR
          cellp := cell.subCells.iterate();
          c : SubCell.T;
          tDummy : TEXT;
        BEGIN
          WHILE cellp.next(tDummy, c) DO
            IF c.array = NIL THEN
              Recurse(c.c, MagTransform.Compose(transform, c.transform))
            ELSE
              (* new version, iterator : *)
              VAR
                iter := MagArrayDataClass.ToTransformIterator(c.transform,
                                                              c.array^);
                t : MagTransform.T;
              BEGIN
                WHILE iter.next(t) DO
                  VAR
                    subXform := MagTransform.Compose(transform, 
                                                     t);
                  BEGIN
                    Recurse(c.c, subXform)
                  END
                END
              END
            END 
          END
        END
      END

    END Recurse;

  BEGIN Recurse(self, MagTransform.Unitary) END FlatMapAll;

PROCEDURE FlatMapLabels(self : T; 
                     labelProc : LabelProc;
                     args : REFANY) =

  PROCEDURE Recurse(cell : T; transform : MagTransform.T) =
    BEGIN

      (* do "local" rects *)

      PROCEDURE LabelMapper(l : MagLabel.T) = 
        BEGIN
          l.rect := MagTransform.Rect(l.rect,transform); (* ok, it's ugly *)
          labelProc(l, args)
        END LabelMapper;

      <* FATAL Wr.Failure, Thread.Alerted *>
      BEGIN 
        IF labelProc # NIL THEN cell.mapMyLabels(LabelMapper) END
      END;

      (* do subcells *)

      IF cell.subCells # NIL THEN
        VAR
          cellp := cell.subCells.iterate();
          c : SubCell.T;
          tDummy : TEXT;
        BEGIN
          WHILE cellp.next(tDummy, c) DO
            IF c.array = NIL THEN
              Recurse(c.c, MagTransform.Compose(transform, c.transform))
            ELSE
              (* new version, iterator : *)
              VAR
                iter := MagArrayDataClass.ToTransformIterator(c.transform,
                                                              c.array^);
                t : MagTransform.T;
              BEGIN
                WHILE iter.next(t) DO
                  VAR
                    subXform := MagTransform.Compose(transform, 
                                                     t);
                  BEGIN
                    Recurse(c.c, subXform)
                  END
                END
              END
            END 
          END
        END
      END

    END Recurse;

  BEGIN Recurse(self, MagTransform.Unitary) END FlatMapLabels;

(* this set of data structures are all protected by mdbMu;  
   they are used by the Mapper.
   I think the only reason this stuff is necessary is that the 
   compiler is buggy. *)
VAR 
  mdbMu := NEW(MUTEX);
  lockedMergeDB : MagMergeDB.T;
  lockedWr : Wr.T;
  lockedMergeData : Rect_LayerSetTbl.T;
  oldlayer : RouteLayer.T;

  (* the rects *)
PROCEDURE Mapper( r : MagLayerRect.T ) RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    IF lockedMergeDB # NIL AND lockedMergeDB.isMergeable(r.layer) THEN
      VAR 
        ls : LayerSet.T; 
      BEGIN
        IF NOT lockedMergeData.get(r.rect,ls) THEN
          ls := NEW(LayerSetDef.T).init(); 
          EVAL lockedMergeData.put(r.rect,ls)
        END;
        EVAL ls.insert(r.layer);
      END
    ELSE
      (* the layer name *)
      IF r.layer # oldlayer THEN
        <* ASSERT ISTYPE(r.layer,RouteLayer.T) *>

        Wr.PutText(lockedWr, "<< "); 
        Wr.PutText(lockedWr, NARROW(r.layer, RouteLayer.T).name);
        Wr.PutText(lockedWr, " >>\n");
        oldlayer := r.layer
      END;
      
      Wr.PutText(lockedWr, "rect "); 
      MagRect.WriteToWrForMagic(lockedWr, r.rect); 
      Wr.PutText(lockedWr,"\n")
    END;
  END Mapper;

(* can only call this if RouteLayers have been used *)
PROCEDURE WriteToWriter(self : T; 
                        wr : Wr.T; mergeDB : MagMergeDB.T) RAISES { Thread.Alerted, Wr.Failure } =

  BEGIN
    (* set timestamp *)
    IF self.touched OR self.timeStamp = MagTimeStamp.Epoch THEN
      SetTimeStamp(self, TRUNC(Time.Now()))
    END;

    (* write header *)
    Wr.PutText(wr,"magic\n");
    Wr.PutText(wr,"tech scmos\n");
    Wr.PutText(wr,"timestamp "& Fmt.Int(self.timeStamp) & "\n");
    
    VAR
      subIter := self.subCells.iterate();
      subCell : SubCell.T;
      useId : TEXT;
    BEGIN
      WHILE subIter.next(useId, subCell) DO
        Wr.PutText(wr,"use " & subCell.c.name & " " & subCell.useId & "\n"); 
        IF subCell.array # NIL THEN
          Wr.PutText(wr, MagArrayData.FormatForMag(subCell.array^) & "\n")
        END;
        Wr.PutText(wr,"timestamp " & Fmt.Int(subCell.c.timeStamp) & "\n");
        Wr.PutText(wr,"transform " & 
                      MagTransform.Format(subCell.transform) & "\n");
        (* the bbox should be output here, but it needs to be recomputed *)
        (* if we added anything.  We can do that by grabbing the bbox from *)
        (* the MagCell, i.e., subCell.c *)

        IF NOT subCell.c.haveBBox THEN
          subCell.c.bbox := MagRect.OriginSquare
        END;

        Wr.PutText(wr, "box " & MagRect.FormatForMagic(

(*
                     MagTransform.Rect(subCell.c.bbox,
                            MagTransform.Inverse(subCell.transform) )
*)
        (* the subcell's bbox should be given in the subcell's coordinate
           system, not in the parent's... *)
        subCell.c.bbox
        ) &
                            "\n");
      END
    END;
    

    VAR
      mergeData := NEW(Rect_LayerSetTbl.Default).init();
      mergeData2 : Rect_LayerSetTbl.T;
    BEGIN 
      LOCK mdbMu DO
        oldlayer := NIL;
        lockedWr := wr;
        lockedMergeDB := mergeDB;
        lockedMergeData := mergeData;
        self.mapMyRects(Mapper) 
      END;

      (* if mergeDB is non-NIL we convert mergeData and output those rects *)
      IF mergeDB # NIL THEN

        (* we do this in two passes to make it easier to break up later,
           if that is found to be desirable.. *)

        (* convert table *)
        VAR
          mdi := mergeData.iterate();
          r : MagRect.T;
          s : LayerSet.T;
        BEGIN
          mergeData2 := NEW(Rect_LayerSetTbl.Default).init();
          WHILE mdi.next(r,s) DO EVAL mergeData2.put(r,mergeDB.merge(s)) END;
          mergeData := NIL;
        END;

        (* dump table *)
        VAR
          mdi := mergeData2.iterate();
          r : MagRect.T;
          s : LayerSet.T;
        BEGIN
          WHILE mdi.next(r,s) DO 
            VAR
              lIter := s.iterate();
              l : Layer.T;
            BEGIN
              WHILE lIter.next(l) DO
                Wr.PutText(wr, "<< " & NARROW(l, RouteLayer.T).name & " >>\n");
                Wr.PutText(wr, "rect " & MagRect.FormatForMagic(r) & "\n")
              END  
            END
          END
        END
      END
    END;

    (* labels, if applicable *)
    IF ISTYPE(self, Labelled) THEN
      VAR
        labs := NARROW(self, Labelled).lblList;
      BEGIN
        Wr.PutText(wr,"<< labels >>\n");
        WHILE labs # NIL DO
          WITH l = labs.head DO
            Wr.PutText(wr,"rlabel " & l.layer & " " &
              MagRect.FormatForMagic(l.rect) & " " & Fmt.Int(l.direction) &
              " " & l.name & "\n")
          END;
          labs := labs.tail
        END
      END
    END;
    Wr.PutText(wr, "<< end >>\n");
    Wr.Close(wr)
  END WriteToWriter;

PROCEDURE Write(self : T; 
                fileName : TEXT; mergeDB : MagMergeDB.T) RAISES { Thread.Alerted, Wr.Failure,
                                          OSError.E } = 
  VAR
    wr : Wr.T;
  BEGIN
    IF fileName = NIL THEN fileName := self.getName() & ".mag" END;
    wr := FileWr.Open(fileName);
    self.writeToWriter(wr, mergeDB := mergeDB)
  END Write;

(************************************************************************)

TYPE RecArg = REF RECORD dirName : TEXT; wrote : TextSet.T END;

PROCEDURE RecursiveCallback(subCell : T; 
                            <*UNUSED*>id : TEXT; 
                            <*UNUSED*>transform : MagTransform.T; 
                            <*UNUSED*>parentId : TEXT; 
                            args : REFANY) RAISES { Thread.Alerted, Wr.Failure, OSError.E } =
  BEGIN DoRecursiveWrite(subCell,args) END RecursiveCallback;

PROCEDURE DoRecursiveWrite(cell : T; arg : RecArg) RAISES { Thread.Alerted, Wr.Failure, OSError.E } =
  VAR
    path := arg.dirName & "/" & cell.getName() & ".mag";
  BEGIN 
    IF NOT arg.wrote.insert(path) THEN
      cell.write(path)  
    END
  END DoRecursiveWrite;

PROCEDURE WriteRecursively(self : T; dirName : TEXT) RAISES { Thread.Alerted, Wr.Failure, OSError.E } =
  BEGIN

    (* create dir, if non-existent *)
    TRY
      IF NOT Atom.Equal(FS.Status(dirName).type, FS.DirectoryFileType) THEN
        <* ASSERT FALSE *>
      END;
    EXCEPT
      (* assume "file not found" *)
      OSError.E => FS.CreateDirectory(dirName)
    END;

    VAR
      iter := FS.Iterate(dirName);
      path, fpath : TEXT;
    BEGIN
      WHILE iter.next(path) DO 
        fpath := dirName & "/" & path;
        IF Atom.Equal(FS.Status(fpath).type, RegularFile.FileType) THEN
          FS.DeleteFile(fpath) 
        END
      END
    END;

    VAR
      arg := NEW(RecArg, dirName := dirName, 
                         wrote   := NEW(TextSetDef.T).init());
    <* FATAL ANY *>
    BEGIN
      DoRecursiveWrite(self, arg);
      self.subCellMap(RecursiveCallback, arg)
    END
  END WriteRecursively;

PROCEDURE Flatten(self : T) : TextCellInstanceTbl.T =
  BEGIN
    IF self.flatInstances = NIL THEN
      self.flatInstances := MagCellFlatten.Flatten(self)
    END;
    RETURN self.flatInstances
  END Flatten;

(* this is REALLY ugly... GRRRRRRR *)
VAR sMu := NEW(MUTEX);
VAR reallySelf : T;

PROCEDURE InitByFlattening(self : T; newName : TEXT; toFlatten : T) : T = 

  PROCEDURE RectMapper(rect : MagLayerRect.T) =
    BEGIN reallySelf.addLayerRect(rect) END RectMapper;

  PROCEDURE LabelMapper(label : MagLabel.T) =
    BEGIN reallySelf.addLabel(label) END LabelMapper;

  BEGIN    
    LOCK sMu DO
      reallySelf := self;  
      self.name := newName;
      self.subCells := NEW(TextSubCellTbl.Default).init();
      toFlatten.flatMapAllNoArgs(RectMapper,LabelMapper);
      self.timeStamp := toFlatten.timeStamp;
      self.touched := FALSE;
      RETURN self 
    END
  END InitByFlattening;


PROCEDURE Equal(a, b : T) : BOOLEAN = 
  BEGIN RETURN a = b END Equal;

PROCEDURE Hash(a : T) : Word.T =
  BEGIN RETURN Text.Hash(a.name) END Hash;

PROCEDURE LayerRectsBbox(t : T; 
                         layer : Layer.T; VAR bbox : MagRect.T) : BOOLEAN =
  VAR
    set := NEW(LayerSetDef.T).init();
  BEGIN
    EVAL set.insert(layer);
    RETURN t.layerSetRectsBbox(set, bbox)
  END LayerRectsBbox;

TYPE
  SetRectsT = OBJECT
    set : LayerSet.T;
    bbox : MagRect.T;
    foundOne := FALSE
  END;

PROCEDURE LayerSetRectsBbox(t : T;
                            set : LayerSet.T;VAR bbox : MagRect.T) : BOOLEAN = 
  VAR
    stuff := NEW(SetRectsT, set := set);
  BEGIN
    t.flatClipMap(SetRectsFinder,stuff);
    IF stuff.foundOne THEN bbox := stuff.bbox END;
    RETURN stuff.foundOne
  END LayerSetRectsBbox;

PROCEDURE SetRectsFinder(r : MagRect.T; l : Layer.T; args : REFANY) =
  VAR
    stuff := NARROW(args, SetRectsT);
  BEGIN
    IF stuff.set.member(l) THEN
      IF stuff.foundOne THEN
        stuff.bbox := MagRect.Union(r, stuff.bbox) 
      ELSE
        (* first rect *)
        stuff.foundOne := TRUE;
        stuff.bbox := r
      END
    END
  END SetRectsFinder;

(***********************************************************************)

TYPE
  LabelInserterT = OBJECT
    s : MagRectSet.T;
    search : TEXT;
  END;

PROCEDURE RectLabelInserter(label : MagLabel.T; args : REFANY) =
  VAR
    t : LabelInserterT := args;
  BEGIN
    IF Text.Equal(label.name, t.search) THEN
      EVAL t.s.insert(label.rect)
    END
  END RectLabelInserter;

PROCEDURE FindLabels(t : T; 
                     named : TEXT; set : MagRectSet.T) : MagRectSet.T =
  BEGIN
    IF set = NIL THEN set := NEW(MagRectSetDef.T).init() END;
    t.flatMapLabels(RectLabelInserter, 
                    NEW(LabelInserterT, s := set, search := named));
    RETURN set
  END FindLabels;

PROCEDURE TightenBBox(t : T) =

  PROCEDURE Recurse(r : T) = 
    VAR
      haveBBox := FALSE;
      n : TEXT;
      s : SubCell.T;
      subIter : TextSubCellTbl.Iterator;
      bbox : MagRect.T;
    BEGIN
      IF r.subCells # NIL THEN
        subIter := r.subCells.iterate();
        (* tighten subcells if not already done *)
        WHILE subIter.next(n,s) DO VAR box : MagRect.T; BEGIN
          
          IF NOT tightened.member(s.c) THEN
            Recurse(s.c);
            EVAL tightened.insert(s.c)
          END;

          s.box := s.c.bbox;

          IF s.array # NIL THEN
            box := MagArrayData.ComputeArrayBBox(s.box,
                                                 s.transform,s.array^)
          ELSE
            box := MagTransform.Rect(s.box, s.transform)
          END;

          IF s.c.haveBBox THEN
            (* grow our bbox *)
            IF NOT haveBBox THEN
              (* set initial bbox *)
              haveBBox := TRUE;
              bbox := box
            ELSE
              (* grow bbox *)
              bbox := MagRect.Union(bbox,box)
            END
          END
        END END
      END;
      
      VAR
        arg := NEW(BMapArg, haveBBox := haveBBox,
                   bbox := bbox);
        <* FATAL Thread.Alerted, Wr.Failure *>  (* can't happen here *)
      BEGIN
        r.mapMyRectsWithArg(BBoxMapper, arg);
        r.mapMyLabelsWithArg(BBoxMapper2, arg);
        
        bbox := arg.bbox;
        haveBBox := arg.haveBBox;
      END;
      IF haveBBox THEN
        r.bbox := bbox;
        r.haveBBox := haveBBox
      END
    END Recurse;

  VAR
    tightened : MagCellSet.T := NEW(MagCellSetDef.T).init();
  BEGIN
    
    (* tighten up the bbox

       must do it recursively.  check whether each type has been
       tightened before doing it.  (must cache w/in procedure) 
    *)
    Recurse(t)

  END TightenBBox;

TYPE
  BMapArg = REF RECORD
    haveBBox : BOOLEAN;
    bbox : MagRect.T;
  END;

PROCEDURE BBoxMapper(r : MagLayerRect.T; q : REFANY) =
  VAR
    qq := NARROW(q, BMapArg);
  BEGIN
    IF NOT qq.haveBBox THEN
      (* set initial bbox *)
      qq.haveBBox := TRUE;
      qq.bbox := r.rect
    ELSE
      (* grow bbox *)
      qq.bbox := MagRect.Union(qq.bbox,r.rect)
    END
  END BBoxMapper;

PROCEDURE BBoxMapper2(r : MagLabel.T; q : REFANY) =
  VAR
    qq := NARROW(q, BMapArg);
  BEGIN
    IF NOT qq.haveBBox THEN
      (* set initial bbox *)
      qq.haveBBox := TRUE;
      qq.bbox := r.rect
    ELSE
      (* grow bbox *)
      qq.bbox := MagRect.Union(qq.bbox,r.rect)
    END
  END BBoxMapper2;


BEGIN END MagCell.
