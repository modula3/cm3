(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Created by stolfi on Wed Apr 19 01:48:27 1989               *)
(* Last modified on Sun May 30 10:41:23 PDT 1993 by meehan     *)
(*      modified on Tue Nov 24 22:15:03 PST 1992 by mhb        *)
(*      modified on Wed Jun 17 12:00:14 PDT 1992 by stolfi     *)

(*      modified on Tue Feb 11 21:39:48 PST 1992 by muller     *)
(*      modified on Mon Nov 11 16:26:22 PST 1991 by steveg     *)


MODULE ColorName EXPORTS ColorName, ColorNameF;

IMPORT Color, ISOChar, Text, TextF, TextList, TextListSort, TextIntTbl,
       TextRefTbl;

FROM ColorNameTable IMPORT Basic;

TYPE 
  FrEntry = RECORD name: TEXT; val: REAL END;   
  (* Entry of fraction table *)

CONST Fraction = ARRAY OF FrEntry{
  
    (* Fraction prefixes: *)
    (* Note: if A is a prefix of B, then A must come after B. *)
    (* Also the last prefix must be "" *)

    FrEntry {name := "VeryVeryVery",     val := 15.0 / 16.0},
    FrEntry {name := "VeryVerySlightly", val := 1.0 / 16.0 },
    FrEntry {name := "VeryVery",         val := 7.0 / 8.0  },
    FrEntry {name := "VerySlightly",     val := 1.0 / 8.0  },
    FrEntry {name := "Very",             val := 3.0 / 4.0  },
    FrEntry {name := "Slightly",         val := 1.0 / 4.0  },
    FrEntry {name := "Somewhat",         val := 3.0 / 8.0  },
    FrEntry {name := "Quite",            val := 5.0 / 8.0  },
    FrEntry {name := "Rather",           val := 1.0 / 2.0  },
    FrEntry {name := "",                 val := 1.0 / 3.0  }
  };

PROCEDURE IsPrefix (a, b: TEXT; VAR (*OUT*) rest: TEXT): BOOLEAN =
  (*
    If /a/ is a prefix of /b/ (ignoring case), return TRUE and put rest
    of /b/ in /rest/.  Else return false and leave /rest/alone.  *)
  BEGIN
    <* ASSERT a # NIL *>
    <* ASSERT b # NIL *>
    WITH aa = a^,
         bb = b^  DO
      IF NUMBER (aa) <= 1 THEN
        rest := b;
        RETURN TRUE
      ELSIF NUMBER (bb) < NUMBER (aa) THEN
        RETURN FALSE
      ELSE
        FOR i := 0 TO LAST (aa) - 1 DO
          IF ISOChar.Lower [aa [i]] # ISOChar.Lower [bb [i]] THEN RETURN FALSE END
        END;
        rest := Text.FromChars (
                  SUBARRAY (bb, NUMBER (aa) - 1, NUMBER (bb) - NUMBER (aa)));
        RETURN TRUE
      END
    END
  END IsPrefix;

PROCEDURE NormalizeName (a: TEXT): TEXT =
  (* Deletes all whitespace in /a/ and converts to lower case *)
  VAR
    b := NEW (REF ARRAY OF CHAR, Text.Length (a));
    j := 0;
  BEGIN
    IF NUMBER (b^) > 0 THEN
      FOR i := 0 TO LAST (a^) - 1 DO
        IF NOT a [i] IN ISOChar.Spaces THEN
          b [j] := ISOChar.Lower [a [i]];
          INC (j)
        END
      END
    END;
    RETURN Text.FromChars (SUBARRAY (b^, 0, j))
  END NormalizeName;

PROCEDURE ToRGB (name: TEXT): Color.T RAISES {NotFound} =
  VAR
    value : REFANY;
    rgb   : Color.T;
    rgbRef: REF Color.T;
  PROCEDURE fail (<* UNUSED *> name: TEXT): Color.T RAISES {NotFound} =
    BEGIN
      RAISE NotFound
    END fail;
  BEGIN
    LOCK nameCache DO
      IF nameCache.table.get (name, value) THEN
        RETURN NARROW (value, REF Color.T)^
      END
    END;
    WITH normalized = NormalizeName (name) DO
      rgb := LowerCaseToRGB (normalized, fail)
    END;
    LOCK nameCache DO
      rgbRef := NEW (REF Color.T);
      rgbRef^ := rgb;
      EVAL nameCache.table.put (name, rgbRef);
    END;
    RETURN rgb
  END ToRGB;

PROCEDURE LowerCaseToRGB (name: TEXT; p: NotInTable): Color.T
  RAISES {NotFound} =
  VAR
    f         : CARDINAL;
    index     : INTEGER;
    y, frac   : REAL;
    rgb       : Color.T;
    hsv       : Color.HSV;
    bare, rest: TEXT;
  BEGIN
    (* Strips fraction modifier: *)
    f := 0;
    WHILE NOT IsPrefix (Fraction [f].name, name, rest) DO INC (f) END;
    frac := Fraction [f].val;
    bare := rest;
    (* Strips color modifier: *)
    IF IsPrefix ("dark", bare, rest) OR IsPrefix ("dim", bare, rest) THEN
      rgb := LowerCaseToRGB (rest, p);
      RETURN Mix (Color.Black, frac, rgb, 1.0 - frac)

    ELSIF IsPrefix ("pale", bare, rest) OR IsPrefix ("light", bare, rest) THEN
      rgb := LowerCaseToRGB (rest, p);
      RETURN Mix (Color.White, frac, rgb, 1.0 - frac)

    ELSIF IsPrefix ("medium", bare, rest) THEN
      (* There must be no fraction modifier: *)
      IF NOT Text.Equal (bare, name) THEN RAISE NotFound END;
      frac := 0.25;
      rgb := LowerCaseToRGB (rest, p);
      RETURN Mix (Color.Black, frac, rgb, 1.0 - frac)

    ELSIF IsPrefix ("reddish", bare, rest) THEN
      rgb := LowerCaseToRGB (rest, p);
      RETURN Mix (Color.Red, frac, rgb, 1.0 - frac)

    ELSIF IsPrefix ("greenish", bare, rest) THEN
      rgb := LowerCaseToRGB (rest, p);
      RETURN Mix (Color.Green, frac, rgb, 1.0 - frac)

    ELSIF IsPrefix ("bluish", bare, rest) THEN
      rgb := LowerCaseToRGB (rest, p);
      RETURN Mix (Color.Blue, frac, rgb, 1.0 - frac)

    ELSIF IsPrefix ("yellowish", bare, rest) THEN
      rgb := LowerCaseToRGB (rest, p);
      RETURN Mix (Color.Yellow, frac, rgb, 1.0 - frac)

    ELSIF IsPrefix ("drab", bare, rest) OR IsPrefix ("dull", bare, rest)
            OR IsPrefix ("weak", bare, rest) THEN
      rgb := LowerCaseToRGB (rest, p);
      y := Color.Brightness (rgb);
      RETURN Mix (Color.T {y, y, y}, frac, rgb, 1.0 - frac)

    ELSIF IsPrefix ("strong", bare, rest) OR IsPrefix ("vivid", bare, rest)
            OR IsPrefix ("bright", bare, rest) THEN
      rgb := LowerCaseToRGB (rest, p);
      hsv := Color.HSV {Color.ToHSV (rgb).h, 1.0, 1.0};
      RETURN Mix (Color.FromHSV (hsv), frac, rgb, 1.0 - frac)

    ELSE
      (* No color modifier -- there must be no fraction modifier: *)
      IF NOT Text.Equal (bare, name) THEN RAISE NotFound END;
      IF NOT table.get (name, index) THEN
        RETURN p (name)
      ELSE
        RETURN Basic [index].rgb
      END
    END
  END LowerCaseToRGB;


PROCEDURE Mix (READONLY a    : Color.T;
                        alpha: REAL;
               READONLY b    : Color.T;
                        beta : REAL   ): Color.T =
  BEGIN
    RETURN Color.T{r := alpha * a.r + beta * b.r, g :=
                 alpha * a.g + beta * b.g, b :=
                 alpha * a.b + beta * b.b}
  END Mix;

PROCEDURE NameList (): TextList.T =
  VAR list: TextList.T := NIL;
  BEGIN
    FOR i := FIRST (Basic) TO LAST (Basic) DO
      list := TextList.Cons (Basic [i].name, list);
    END;
    RETURN TextListSort.SortD (list)
  END NameList;

PROCEDURE Init () =
  BEGIN
    nameCache := NEW (Cache, table := NEW (TextRefTbl.Default).init (16));
    table := NEW (TextIntTbl.Default).init (NUMBER (Basic));
    FOR i := 0 TO LAST (Basic) DO
      IF table.put (NormalizeName (Basic [i].name), i) THEN
        (* ignore duplicates (case-variants) *)
      END;
    END
  END Init;

BEGIN
  Init ();
  <* ASSERT Text.Empty (Fraction [LAST (Fraction)].name) *>
END ColorName.

