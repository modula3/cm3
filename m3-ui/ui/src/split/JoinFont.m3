(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jan 31 09:41:03 PST 1995 by kalsow  *)
(*      modified on Thu Dec 10 18:35:50 PST 1992 by msm     *)
<*PRAGMA LL*>

MODULE JoinFont;

IMPORT ScrnFont, JoinScreen, Font, Rect, Palette, PlttFrnds, VBTRep;

PROCEDURE New(st: JoinScreen.T): Oracle =
  BEGIN
    RETURN NEW(Oracle, st := st)
  END New;

PROCEDURE Resolve (st: JoinScreen.T; fnt: Font.T) =
  VAR i: INTEGER; t := st.succ(NIL, i);
  BEGIN
    WHILE t # NIL DO EVAL Palette.ResolveFont(t, fnt); t := st.succ(t, i) END
  END Resolve;

PROCEDURE Apply (           st : JoinScreen.T;
                 <*UNUSED*> cl : Palette.FontClosure;
                            fnt: Font.T               ): ScrnFont.T =
  VAR res := st.fonts[fnt.fnt];
  BEGIN
    Resolve(st, fnt);
    IF res = NIL OR res = PlttFrnds.noFont THEN
      res := NEW(T, id := 2 * fnt.fnt + 1, st := st)
    END;
    VAR
      i: INTEGER;
      t          := st.succ(NIL, i);
    BEGIN
      IF t = NIL THEN
        res.metrics := EmptyMetrics
      ELSE
        res.metrics := Palette.ResolveFont(t, fnt).metrics
      END
    END;
    RETURN res
  END Apply;

REVEAL
  Oracle = ScrnFont.Oracle BRANDED OBJECT
             st: JoinScreen.T;
           (*
           OVERRIDES
             match   := Match;
             list    := List;
             lookup  := Lookup;
             builtIn := BuiltIn
           *)
           END;

TYPE
  T = ScrnFont.T OBJECT
        st: JoinScreen.T;
      (*
      OVERRIDES
        localize := Localize;
        unload   := Unload
      *)
      END;

(*
PROCEDURE FontMatch(orc: FontOracle; 
                    family: TEXT;
                    pointSize: INTEGER;
                    slant: ScrnFont.Slant;
                    maxResults: CARDINAL;
                    weightName: TEXT;
                    version: TEXT;
                    foundry: TEXT;
                    width: TEXT;
                    pixelsize: INTEGER;
                    hres, vres: INTEGER;
                    spacing: ScrnFont.Spacing;
                    averageWidth: INTEGER;
                    charsetRegistry: TEXT;
                    charsetEncoding: TEXT)
                    : REF ARRAY OF TEXT =
  BEGIN
    IF orc.st.sts = NIL THEN Crash(); END;
    RETURN orc.st.sts[0].font.match(family, pointSize, slant, maxResults,
                             weightName, version, foundry, width,
                             pixelsize, hres, vres, spacing,
                             averageWidth, charsetRegistry, charsetEncoding);
  END FontMatch;
                    
PROCEDURE FontBuiltIn(orc: FontOracle; id: Font.Predefined): ScrnFont.T =
  BEGIN
    IF orc.st.sts = NIL THEN Crash(); END;
    RETURN orc.st.sts[0].font.builtIn(id);
  END FontBuiltIn;

PROCEDURE FontList(orc: FontOracle;
                   pat: TEXT;
                   maxResults: INTEGER) : REF ARRAY OF TEXT =
  BEGIN
    IF orc.st.sts = NIL THEN Crash(); END;
    RETURN orc.st.sts[0].font.list(pat, maxResults);
  END FontList;

PROCEDURE FontLookup(orc: FontOracle; name: TEXT): ScrnFont.T =
  BEGIN
    IF orc.st.sts = NIL THEN Crash(); END;
    RETURN orc.st.sts[0].font.lookup(name);
  END FontLookup;

EXCEPTION FatalError;

PROCEDURE Crash () =
  <* FATAL FatalError *>
  BEGIN
    RAISE FatalError;
  END Crash;
*)

VAR
  EmptyMetrics := NEW(NullMetrics,
                      minBounds := ScrnFont.CharMetric{0, Rect.Empty},
                      maxBounds := ScrnFont.CharMetric{0, Rect.Empty},
                      firstChar := 0, lastChar := 0, selfClearing := TRUE,
                      charMetrics := NIL);

TYPE
  NullMetrics = ScrnFont.Metrics OBJECT
                OVERRIDES
                  intProp  := NullIntProp;
                  textProp := NullTextProp
                END;

PROCEDURE NullIntProp (<*UNUSED*> self: NullMetrics;
                       <*UNUSED*> name: TEXT;
                       <*UNUSED*> ch  : INTEGER       := -1): INTEGER
  RAISES {ScrnFont.Failure} =
  BEGIN
    RAISE ScrnFont.Failure
  END NullIntProp;

PROCEDURE NullTextProp (<*UNUSED*> self: NullMetrics;
                        <*UNUSED*> name: TEXT;
                        <*UNUSED*> ch  : INTEGER       := -1): TEXT
  RAISES {ScrnFont.Failure} =
  BEGIN
    RAISE ScrnFont.Failure
  END NullTextProp;

BEGIN END JoinFont.

