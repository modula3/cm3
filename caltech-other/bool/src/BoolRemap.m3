(* $Id$ *)
MODULE BoolRemap;
IMPORT BoolRemapImpl;
IMPORT Bool;
IMPORT BoolFormatter,TextWr,Wr,Thread;
IMPORT BoolBoolTbl;

REVEAL
  T = Public BRANDED Brand OBJECT 
  OVERRIDES
    format := Format;
    remap := Remap;
    mergeD := MergeD;
    init := Init;
  END;

PROCEDURE Init(s : T) : T = BEGIN RETURN BoolBoolTbl.Default.init(s) END Init;

PROCEDURE Format(s : T; bf : BoolFormatter.T) : TEXT =
  <* FATAL Thread.Alerted, Wr.Failure *>
  VAR
    i := s.iterate();
    b1, b2 : Bool.T;
    wr := NEW(TextWr.T).init();
  BEGIN
    WHILE i.next(b1,b2) DO
      Wr.PutText(wr,bf.fmt(b1)); Wr.PutText(wr," -> "); Wr.PutText(wr,bf.fmt(b2));
      Wr.PutChar(wr,'\n')
    END;
    RETURN TextWr.ToText(wr)
  END Format;

PROCEDURE Merge(m1, m2 : Map) : Map =
  VAR
    res := Empty();
  BEGIN
    RETURN MergeD(MergeD(res,m1),m2)
  END Merge;

PROCEDURE MergeD(m1, m2 : Map) : Map =
  VAR
    iter := m2.iterate();
    b0,b1 : Bool.T;
  BEGIN
    WHILE iter.next(b0,b1) DO EVAL m1.put(b0,b1) END;
    RETURN m1
  END MergeD;

PROCEDURE Empty() : Map = 
  BEGIN RETURN NEW(T).init() END Empty;

PROCEDURE Remap(map : T; e : Bool.T; check : BOOLEAN) : Bool.T =
  BEGIN RETURN BoolRemapImpl.Remap(map,e,check) END Remap;

BEGIN END BoolRemap.
