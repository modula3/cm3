(* Copyright 1997, Critical Mass, Inc.  All rights reserved. *)

MODULE PasswordFont;

IMPORT Font, Palette, RTAllocator, ScreenType, ScrnFont;
(**** Version 4.0 *****************
IMPORT RT0, RTHeap, RTMisc, RTType;
***********************************)

TYPE
  PasswdFont = Palette.FontClosure OBJECT
    next    : PasswdFont;
    base    : Font.T;
    derived : Font.T;
    ch      : CHAR;
  OVERRIDES
    apply := Apply
  END;

VAR
  mu    : MUTEX      := NEW (MUTEX);
  fonts : PasswdFont := NIL;

PROCEDURE New (base: Font.T;  ch: CHAR): Font.T =
  (* Return a font where all characters are painted as if they were
     character "ch" in font "base". *)
  VAR x: PasswdFont;
  BEGIN
    LOCK mu DO
      x := fonts;
      WHILE (x # NIL) DO
        IF (x.base = base) AND (x.ch = ch) THEN RETURN x.derived; END;
        x := x.next;
      END;
      x := NEW (PasswdFont, base := base, ch := ch, next := fonts);
      x.derived := Palette.FromFontClosure (x);
      fonts := x;
      RETURN x.derived;
    END;
  END New;

PROCEDURE Apply (pf: PasswdFont;  st: ScreenType.T): ScrnFont.T =
  <*FATAL RTAllocator.OutOfMemory*>
  VAR
    base := Palette.ResolveFont (st, pf.base);
    xx   : ScrnFont.T := RTAllocator.Clone (base);
  BEGIN
    (* now, patch up the new font so it only paints one character *)
    IF (base.metrics # NIL) THEN
      xx.metrics := RTAllocator.Clone (base.metrics);
      WITH m = xx.metrics DO
        m.family := "password";
        IF (m.charMetrics # NIL) THEN
          VAR save := m.charMetrics [ORD ('*') - m.firstChar];  BEGIN
            m.charMetrics := NEW (ScrnFont.CharMetrics, 1);
            m.charMetrics[0] := save;
          END;
        END;
        m.defaultChar := ORD ('*');
        m.firstChar := ORD ('*');
        m.lastChar := ORD ('*');
      END;
    END;
    RETURN xx;
  END Apply;

(*---------------------------------------------------------------- UNSAFE ---*)
(**** Reactor version 4.0 ********************************
PROCEDURE Clone (ref: REFANY): REFANY =
  VAR x: REFANY;  defn: RT0.TypeDefn;  tc, len: INTEGER;  src, dest: ADDRESS;
  BEGIN
    IF (ref = NIL) THEN RETURN NIL; END;

    tc := TYPECODE (ref);
    defn := RTType.Get (tc);

    IF defn.nDimensions = 0 THEN
      (* REF or OBJECT *)
      x := RTAllocator.NewTraced (tc);
    ELSE (* open array *)
      VAR shape : ARRAY [0..31] OF INTEGER;
      BEGIN
        RTHeap.GetArrayShape (ref, shape);
        x := RTAllocator.NewTracedArray (tc,
                SUBARRAY (shape, 0, defn.nDimensions));
      END;
    END;

    len  := RTHeap.GetDataSize (ref);
    src  := RTHeap.GetDataAdr (ref);
    dest := RTHeap.GetDataAdr (x);
    RTMisc.Copy (src, dest, len);

    RETURN x;
  END Clone;
******************************************************************)

BEGIN
END PasswordFont.
