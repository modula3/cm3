(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jul 30 11:40:24 PDT 1993 by steveg *)
(*      modified on Sun Dec 13 21:17:57 PST 1992 by mhb    *)
(*      modified on Fri Jul  3 22:04:08 PDT 1992 by meehan *)
(*      modified on Tue Jun 16 21:55:35 PDT 1992 by muller *)

MODULE XParam;

IMPORT Fmt, Point, Rect, Text, Trestle, TrestleComm;

PROCEDURE ParseDisplay (t: TEXT): Display RAISES {Error} =
  VAR
    n                           := Text.Length (t);
    i, start: CARDINAL          := 0;
    z       : Display;
    buf     : REF ARRAY OF CHAR;
  PROCEDURE Err (spec: TEXT; index: CARDINAL) RAISES {Error} =
    BEGIN
      RAISE Error (NEW (DisplayInfo, spec := spec, index := index))
    END Err;
  PROCEDURE scan () RAISES {Error} =
    BEGIN
      IF n = 0 THEN Err (t, 0) END;
      buf := NEW (REF ARRAY OF CHAR, n);
      Text.SetChars (buf^, t);
      WHILE buf [i] # ':' DO INC (i); IF i = n THEN Err (t, 0) END END;
      IF i = n THEN Err (t, 0) END;
      (* IF buf [i] # ':' THEN Err (t, i) END; *)
      IF i # 0 THEN
        z.hostname := Text.FromChars (SUBARRAY (buf^, 0, i))
      END;
      INC (i);
      IF i = n THEN Err (t, i - 1) END;
      IF buf [i] = ':' THEN
        z.DECnet := TRUE;
        INC (i);
        IF i = n THEN Err (t, i - 1) END
      END;
      start := i;
      z.display := num (i, n, buf);
      IF i = start THEN Err (t, start) END;
      IF i = n THEN RETURN END;
      IF buf [i] # '.' THEN Err (t, i) END;
      INC (i);
      IF i = n THEN Err (t, i - 1) END;
      start := i;
      z.screen := num (i, n, buf);
      IF i = start THEN Err (t, start) END;
      IF i # n THEN Err (t, i) END;
    END scan;
  BEGIN
    scan ();
    RETURN z
  END ParseDisplay;

PROCEDURE UnparseDisplay (READONLY d: Display): TEXT =
  CONST colons = ARRAY BOOLEAN OF TEXT {":", "::"};
  BEGIN
    RETURN Fmt.F ("%s%s%s.%s", d.hostname, colons [d.DECnet],
                  Fmt.Int (d.display), Fmt.Int (d.screen))
  END UnparseDisplay;
  
PROCEDURE ParseGeometry (t: TEXT): Geometry RAISES {Error} =
  CONST
    VertexMap = ARRAY BOOLEAN, BOOLEAN OF
                  Rect.Vertex{
                  ARRAY BOOLEAN OF
                    Rect.Vertex{Rect.Vertex.SE, Rect.Vertex.NE},
                  ARRAY BOOLEAN OF
                    Rect.Vertex{Rect.Vertex.SW, Rect.Vertex.NW}};
  VAR
    width, height: INTEGER;
    x, y                             := 0;
    i, start     : CARDINAL          := 0;
    n                                := Text.Length(t);
    buf          : REF ARRAY OF CHAR;
    xplus, yplus                     := TRUE;
  PROCEDURE Err (spec: TEXT; index: CARDINAL) RAISES {Error} =
    BEGIN
      RAISE
        Error(NEW(GeometryInfo, spec := spec, index := index))
    END Err;
  PROCEDURE scan () RAISES {Error} =
    BEGIN
      IF n = 0 THEN Err(t, 0) END;
      buf := NEW(REF ARRAY OF CHAR, n);
      Text.SetChars(buf^, t);
      width := num(i, n, buf);
      IF i = 0 THEN width := Missing.h END;
      IF i = n THEN RETURN END;
      IF buf[i] = 'x' THEN
        INC(i);
        start := i;
        height := num(i, n, buf);
        IF start = i THEN Err(t, start) END
      ELSE
        height := Missing.v
      END;
      IF i = n THEN RETURN END;
      IF buf[i] # '+' AND buf[i] # '-' THEN Err(t, i) END;
      INC(i);
      start := i;
      x := num(i, n, buf);
      IF i = start THEN Err(t, start) END;
      IF buf[start - 1] = '-' THEN xplus := FALSE END;
      IF i = n THEN RETURN END;
      IF buf[i] # '+' AND buf[i] # '-' THEN Err(t, i) END;
      INC(i);
      start := i;
      y := num(i, n, buf);
      IF i = start OR i # n THEN Err(t, start) END;
      IF buf[start - 1] = '-' THEN yplus := FALSE END;
    END scan;
  BEGIN
    scan();
    RETURN Geometry{VertexMap[xplus, yplus], Point.T{x, y},
                    Point.T{width, height}}
  END ParseGeometry;

PROCEDURE UnparseGeometry (READONLY g: Geometry): TEXT =
  CONST
    xplus = ARRAY Rect.Vertex OF TEXT {"+", "-", "+", "-"};
    yplus = ARRAY Rect.Vertex OF TEXT {"+", "+", "-", "-"};
  BEGIN
    RETURN Fmt.Int (g.size.h) & "x" & Fmt.Int (g.size.v) & xplus [g.vertex]
             & Fmt.Int (ABS (g.dp.h)) & yplus [g.vertex]
             & Fmt.Int (ABS (g.dp.v))
  END UnparseGeometry;
  
PROCEDURE num (VAR i: CARDINAL; n: CARDINAL; buf: REF ARRAY OF CHAR):
  CARDINAL =
  CONST DIGITS = SET OF CHAR {'0'.. '9'};
  VAR v: CARDINAL := 0;
  BEGIN
    LOOP
      IF i = n OR NOT buf [i] IN DIGITS THEN RETURN v END;
      v := 10 * v + ORD (buf [i]) - ORD ('0');
      INC (i)
    END
  END num;

PROCEDURE Position (         trsl: Trestle.T;
                             id  : Trestle.ScreenID;
                    READONLY g   : Geometry          ): Point.T
  RAISES {TrestleComm.Failure} =
  BEGIN
    WITH array = Trestle.GetScreens(trsl) DO
      IF array = NIL THEN RAISE TrestleComm.Failure END;
      FOR i := FIRST(array^) TO LAST(array^) DO
        IF id = Trestle.NoScreen OR array[i].id = id THEN
          WITH s = array[i].dom DO
            CASE g.vertex OF
            | Rect.Vertex.NW =>
                RETURN Point.T{v := s.north + g.dp.v, 
                               h := s.west + g.dp.h}
            | Rect.Vertex.SW =>
                RETURN Point.T{v := s.south - g.size.v - g.dp.v,
                               h := s.west + g.dp.h}
            | Rect.Vertex.NE =>
                RETURN Point.T{v := s.north + g.dp.v, 
                               h := s.east - g.size.h - g.dp.h}
            | Rect.Vertex.SE =>
                RETURN Point.T{v := s.south - g.size.v - g.dp.v,
                               h := s.east - g.size.h - g.dp.h}
            END;
          END
        END
      END
    END;
    RAISE TrestleComm.Failure
  END Position;

BEGIN
END XParam.
