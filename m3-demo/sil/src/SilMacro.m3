(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Mon Nov  7 11:42:22 PST 1994 by kalsow    *)

MODULE SilMacro;

IMPORT Rect, Point;
IMPORT SilObject, SilWindow, SilFont, SilWr, SilRd;
  
(*----------------------------------------------------------- definitions ---*)
  
REVEAL
  Defn = BRANDED "MacroDefn" REF RECORD
    objs    : SilObject.T := NIL;
    file_id : INTEGER     := 0;
    age     : INTEGER     := 0;
  END;

PROCEDURE NewDefn (): Defn =
  BEGIN
    RETURN NEW (Defn);
  END NewDefn;

PROCEDURE AddObj (d: Defn;  obj: SilObject.T) =
  BEGIN
    <*ASSERT obj.next = NIL*>
    obj.next := d.objs;
    d.objs := obj;
  END AddObj;

PROCEDURE WriteDefn (d: Defn;  wr: SilWr.T): INTEGER =
  VAR o: SilObject.T;
  BEGIN
    IF (d.age < wr.generation) THEN
      d.age := wr.generation;
      d.file_id := wr.next_macro_id;  INC (wr.next_macro_id);

      (* make sure any nested marcros are already emitted *)
      o := d.objs;
      WHILE (o # NIL) DO
        IF (o.state >= SilObject.Visible) THEN
          TYPECASE o OF
          | T(t) => EVAL WriteDefn (t.body, wr);
          ELSE (* skip *)
          END;
        END;
        o := o.next;
      END;

      (* finally, emit self *)
      wr.putText ("m ");
      wr.endLine ();
      o := d.objs;
      WHILE (o # NIL) DO
        IF (o.state >= SilObject.Visible) THEN  o.write (wr);  END;
        o := o.next;
      END;
      wr.putText ("e");
      wr.endLine ();
    END;
    RETURN d.file_id;
  END WriteDefn;

TYPE
  RdInfo = REF RECORD
    cnt   : INTEGER;
    defns : REF ARRAY OF Defn;
  END;

PROCEDURE ReadDefn (rd: SilRd.T): REFANY =
  VAR d := NewDefn ();  info: RdInfo := rd.macros;
  BEGIN
    <*ASSERT rd.n_strs = 0 AND rd.n_ints = 0 *>
    IF (info = NIL) OR (info.cnt >= NUMBER (info.defns^)) THEN
      info := ExpandInfo (info);
      rd.macros := info;
    END;
    info.defns [info.cnt] := d;
    INC (info.cnt);
    rd.in_macro := TRUE;
    RETURN d;
  END ReadDefn;

PROCEDURE EndDefn (rd: SilRd.T): REFANY =
  BEGIN
    <*ASSERT rd.n_strs = 0 AND rd.n_ints = 0 *>
    rd.in_macro := FALSE;
    RETURN NIL;
  END EndDefn;

PROCEDURE ExpandInfo (info: RdInfo): RdInfo =
  VAR n: INTEGER;  new: REF ARRAY OF Defn;
  BEGIN
    IF (info = NIL) THEN
      info := NEW (RdInfo);
      info.defns := NEW (REF ARRAY OF Defn, 100);
    ELSE
      n := NUMBER (info.defns^);
      new := NEW (REF ARRAY OF Defn, n+n);
      SUBARRAY (new^, 0, n) := info.defns^;
      info.defns := new;
    END;
    RETURN info;
  END ExpandInfo;

(*------------------------------------------------------------------ uses ---*)
  
REVEAL
  T = Tx BRANDED "Macro" OBJECT
    body : Defn;
  OVERRIDES
    init     := Init;
    draw     := Draw;
    clone    := Clone;
    write    := Write;
    setWidth := SetWidth;
    setFont  := SetFont;
  END;

PROCEDURE Init (t: T;  READONLY bbox: Rect.T;  body: Defn): T =
  BEGIN
    t.state  := SilObject.Selected;
    t.next   := NIL;
    t.bbox   := bbox;
    t.body   := body;
    RETURN t;
  END Init;
  
PROCEDURE Write (t: T;  wr: SilWr.T) =
  VAR x: INTEGER;
  BEGIN
    IF t.state >= SilObject.Visible THEN
      x := WriteDefn (t.body, wr);
      wr.putText ("i ");
      SilObject.WriteBBox (t, wr);
      wr.putInt (x);
      wr.endLine ();
    END;
  END Write;

PROCEDURE Read (rd: SilRd.T): REFANY =
  VAR t := NEW (T);  info: RdInfo := rd.macros;
  BEGIN
    <*ASSERT rd.n_strs = 0 AND rd.n_ints = 5 *>
    SilObject.ReadBBox (t, rd);
    t.body := info.defns [rd.ints[4]];
    RETURN t;
  END Read;
  
PROCEDURE Clone (t: T): SilObject.T =
  BEGIN
    RETURN NEW (T, body := t.body);
  END Clone;
  
PROCEDURE Draw (t: T;  READONLY p: Point.T;  sel: BOOLEAN;  w: SilWindow.T) =
  VAR o: SilObject.T;  p0: Point.T;
  BEGIN
    IF t.state >= SilObject.Visible THEN
      sel := sel OR (t.state = SilObject.Selected);
      p0.h := t.bbox.west + p.h;
      p0.v := t.bbox.north + p.v;
      o := t.body.objs;
      WHILE o # NIL DO
        o.draw (p0, sel, w);
        o := o.next
      END;
    END;
  END Draw;
  
PROCEDURE SetWidth (t: T;  width: INTEGER;  sel: BOOLEAN): BOOLEAN =
  VAR found := FALSE;  o := t.body.objs;
  BEGIN
    IF (t.state = SilObject.Selected) OR sel THEN
      WHILE o # NIL DO
        IF o.setWidth (width, TRUE) THEN found := TRUE END;
        o := o.next
      END;
      RETURN found;
    ELSE
      RETURN FALSE;
    END;
  END SetWidth;
  
PROCEDURE SetFont (t: T; f: SilFont.T; sel: BOOLEAN; w: SilWindow.T): BOOLEAN =
  VAR found := FALSE;  o := t.body.objs;
  BEGIN
    IF (t.state = SilObject.Selected) OR sel THEN
      WHILE o # NIL DO
        IF o.setFont (f, TRUE, w) THEN found := TRUE END;
        o := o.next;
      END;
      RETURN found;
    ELSE
      RETURN FALSE;
    END;
  END SetFont;

PROCEDURE Contents (t: T): SilObject.T =
  BEGIN
    RETURN t.body.objs;
  END Contents;

BEGIN
  SilWindow.RegisterReader ('m', 'm', ReadDefn);
  SilWindow.RegisterReader ('e', 'e', EndDefn);
  SilWindow.RegisterReader ('i', 'i', Read);
END SilMacro.
