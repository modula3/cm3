(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Tue Jan 31 10:14:08 PST 1995 by kalsow   *)
(*      modified on Thu Mar  4 18:15:16 PST 1993 by msm      *)
(*      modified on Tue Mar 10 19:08:58 1992 by steveg   *)
(*      modified on Mon Feb 24 13:55:22 PST 1992 by muller   *)
(*      modified on Tue Nov 19 20:45:51 PST 1991 by gnelson  *)
<*PRAGMA LL*>

MODULE TypeInVBT;

IMPORT TextVBT, Text, VBT, Font, PaintOp, Latin1Key, KeyboardKey, 
  Rect, VBTClass, TextVBTClass, ComposeKey, Cursor;

TYPE Selection = {KBFocus, Target, Source};

VAR 
  map := ARRAY Selection OF VBT.Selection
    {VBT.KBFocus, VBT.Target, VBT.Source};

PROCEDURE Unmap(sel: VBT.Selection): Selection =
  BEGIN
    FOR i := FIRST(map) TO LAST(map) DO
      IF map[i] = sel THEN RETURN i END
    END;
    Crash(); <*ASSERT FALSE*>
  END Unmap;

REVEAL T = Public BRANDED OBJECT
    <* LL >= {VBT.mu} *>
    composer: ComposeKey.T;
    action: Proc;
    has := ARRAY Selection OF BOOLEAN{FALSE, ..};
  OVERRIDES
    init := Init;
    mouse := Mouse;
    key := KeyCode;
    misc := MiscCode;
    read := Read;
    write := Write
  END;

VAR
  mu := NEW(MUTEX);
  inited := FALSE;
  composingCursor: Cursor.T := Cursor.TextPointer;

REVEAL
  Composer = CPublic BRANDED OBJECT
               v: VBT.T := NIL
             OVERRIDES
               init := CompInit;
               feedback := Feedback
             END;

PROCEDURE Feedback (c: Composer; composing: BOOLEAN) =
  BEGIN
    IF c.v # NIL THEN
      IF composing THEN
        VBT.SetCursor(c.v, composingCursor)
      ELSE
        VBT.SetCursor(c.v, Cursor.TextPointer)
      END
    END
  END Feedback;

PROCEDURE CompInit (c: Composer; v: VBT.T) =
  BEGIN
    LOCK mu DO
      IF NOT inited THEN
        inited := TRUE;
        composingCursor := Cursor.FromName(ARRAY OF TEXT{"XC_exchange"})
      END
    END;
    c.v := v
  END CompInit;

PROCEDURE Init (v             : T;
                txt           : TEXT              := "";
                halign, valign: REAL              := 0.5;
                hmargin       : REAL              := 0.5;
                vmargin       : REAL              := 0.5;
                fnt           : Font.T            := Font.BuiltIn;
                op            : PaintOp.ColorQuad := NIL;
                action        : Proc              := NIL;
                ref           : REFANY            := NIL;
                composer      : ComposeKey.T                       ): T =
  VAR mycomp: Composer;
  BEGIN
    EVAL TextVBT.T.init(v, txt, halign, valign, hmargin, vmargin, fnt, op);
    IF composer = NIL THEN
      mycomp := NEW(Composer);
      mycomp.init(v);
      composer := mycomp
    END;
    v.composer := composer;
    v.action := action;
    IF ref # NIL THEN VBT.PutProp(v, ref) END;
    VBT.SetCursor(v, Cursor.TextPointer);
    RETURN v
  END Init;

PROCEDURE New(
  txt: TEXT := "";
  halign, valign: REAL := 0.5;
  hmargin: REAL := 0.5;
  vmargin: REAL := 0.5;
  fnt: Font.T := Font.BuiltIn;
  op: PaintOp.ColorQuad := NIL;
  action: Proc := NIL;
  ref: REFANY := NIL;
  composer: ComposeKey.T): T =
  BEGIN 
    RETURN NEW(T).init(txt, halign, valign, hmargin, vmargin, fnt, op, 
      action, ref, composer) 
  END New;

PROCEDURE HasFocus(v: T): BOOLEAN = 
  BEGIN RETURN v.has[Selection.KBFocus] END HasFocus;

PROCEDURE TakeSelection(v: T; t: VBT.TimeStamp; s: Selection): BOOLEAN =
  BEGIN
    IF NOT v.has[s] THEN
      TRY 
        VBT.Acquire(v, map[s], t);
        v.has[s] := TRUE
      EXCEPT
        VBT.Error => RETURN FALSE
      END
    END;
    RETURN TRUE
  END TakeSelection;

PROCEDURE TakeFocus(v: T; t: VBT.TimeStamp): BOOLEAN =
  BEGIN
    RETURN TakeSelection(v, t, Selection.KBFocus) 
       AND TakeSelection(v, t, Selection.Target) 
  END TakeFocus;

PROCEDURE MiscCode(v: T; READONLY cd: VBT.MiscRec) =
  BEGIN
    IF cd.type = VBT.TakeSelection THEN
      IF cd.selection = VBT.KBFocus THEN
        EVAL TakeFocus(v, cd.time)
      ELSIF cd.selection = VBT.Target OR cd.selection = VBT.Source THEN
        EVAL TakeSelection(v, cd.time, Unmap(cd.selection))
      END
    ELSIF cd.type = VBT.Lost THEN
      FOR i := FIRST(map) TO LAST(map) DO
        IF map[i] = cd.selection THEN
          v.has[i] := FALSE
        END
      END
    END
  END MiscCode;

PROCEDURE Mouse(v: T; READONLY cd: VBT.MouseRec) =
  BEGIN
    IF cd.clickType = VBT.ClickType.FirstDown THEN
      IF VBT.Modifier.Control IN cd.modifiers THEN
        EVAL TakeSelection(v, cd.time, Selection.Source)
      ELSE
        EVAL TakeFocus(v, cd.time)
      END
    END
  END Mouse;

PROCEDURE KeyCode (v: T; READONLY inputCd: VBT.KeyRec) =
  VAR cd := inputCd;
  BEGIN
    IF inputCd.wentDown AND cd.whatChanged # VBT.NoKey THEN
      LOOP
        IF v.composer # NIL THEN cd := v.composer.filter(cd) END;
        IF cd.whatChanged = VBT.NoKey THEN EXIT END;
        DoKeyCode(v, cd);
        cd.whatChanged := VBT.NoKey
      END
    END
  END KeyCode;

PROCEDURE DoKeyCode (v: T; READONLY cd: VBT.KeyRec) =
  CONST
    NoCtrlOpt = VBT.Modifiers{FIRST(VBT.Modifier).. LAST(VBT.Modifier)}
                  - VBT.Modifiers{
                      VBT.Modifier.Control, VBT.Modifier.Option};
  VAR
    handled := FALSE;
    wc      := cd.whatChanged;
  BEGIN
    IF NOT cd.wentDown OR Rect.IsEmpty(VBT.Domain(v)) THEN RETURN END;
    IF wc > 0 AND wc <= Latin1Key.ydiaeresis THEN
      IF cd.modifiers <= NoCtrlOpt THEN
        LOCK v DO
          v.text := v.text & Text.FromChar(VAL(cd.whatChanged, CHAR));
        END;
        VBT.Mark(v);
        handled := TRUE
      ELSIF wc = Latin1Key.Q OR wc = Latin1Key.q THEN
        TextVBT.Put(v, "");
        handled := TRUE
      ELSIF wc = Latin1Key.W OR wc = Latin1Key.w THEN
        TRY
          TYPECASE VBT.Read(v, VBT.Source, cd.time).toRef() OF
            TEXT (t) => LOCK v DO v.text := v.text & t; END; VBT.Mark(v)
          ELSE                   (* skip *)
          END
        EXCEPT
          VBT.Error =>           (* skip *)
        END;
        handled := TRUE
      ELSIF wc = Latin1Key.E OR wc = Latin1Key.e THEN
        TRY
          TYPECASE VBT.Read(v, VBT.Source, cd.time).toRef() OF
            TEXT (t) =>
              LOCK v DO v.text := v.text & t END;
              VBT.Mark(v);
              IF NOT v.has[Selection.Source] THEN
                VBT.Write(v, VBT.Source, cd.time, VBT.FromRef(""))
              END
          ELSE                   (* skip *)
          END
        EXCEPT
          VBT.Error =>           (* skip *)
        END;
        handled := TRUE
      ELSIF wc = Latin1Key.R OR wc = Latin1Key.r THEN
        VAR o := TextVBT.Get(v);
        BEGIN
          TRY
            TYPECASE VBT.Read(v, VBT.Source, cd.time).toRef() OF
              TEXT (t) =>
                TextVBT.Put(v, t);
                VBT.Write(v, VBT.Source, cd.time, VBT.FromRef(o))
            ELSE                 (* skip *)
            END
          EXCEPT
            VBT.Error =>         (* skip *)
          END
        END;
        handled := TRUE
      END
    ELSIF wc = KeyboardKey.BackSpace OR wc = KeyboardKey.Delete THEN
      LOCK v DO
        v.text := Text.Sub(v.text, 0, MAX(0, Text.Length(v.text) - 1));
      END;
      VBT.Mark(v);
      handled := TRUE
    END;
    IF v.action # NIL AND NOT handled THEN v.action(v, cd) END
  END DoKeyCode;

PROCEDURE SetAction(v: T; action: Proc) =
  BEGIN
    v.action := action
  END SetAction;

PROCEDURE Read(v: T; <*UNUSED*>s: VBT.Selection; tc: CARDINAL): VBT.Value 
  RAISES {VBT.Error} =
  BEGIN
    IF tc = TYPECODE(TEXT) THEN
      LOCK v DO RETURN VBT.FromRef(v.text) END;
    ELSE
      RAISE VBT.Error(VBT.ErrorCode.WrongType)
    END
  END Read;

PROCEDURE Write(v: T; s: VBT.Selection; val: VBT.Value; tc: CARDINAL)  
  RAISES {VBT.Error} =
  BEGIN
    IF tc = TYPECODE(TEXT) THEN
      TYPECASE val.toRef() OF
        TEXT (t) => 
          IF s = VBT.Target THEN
            LOCK v DO v.text := v.text & t END
          ELSE
            LOCK v DO v.text := t END
          END; 
          VBT.Mark(v);
          RETURN
      ELSE (* skip *)
      END
    END;
    RAISE VBT.Error(VBT.ErrorCode.WrongType)
  END Write;

EXCEPTION FatalError;

PROCEDURE Crash() =
  <*FATAL FatalError*>
  BEGIN
    RAISE FatalError
  END Crash;

BEGIN END TypeInVBT.
