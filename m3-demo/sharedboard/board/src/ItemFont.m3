(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

MODULE ItemFont;

IMPORT Font, Text, Scan, Fmt,
       FontCache;

REVEAL T = BRANDED "ItemFont" REF RECORD
    preSize: TEXT;
    size: REAL;
    postSize: TEXT;
  END;


PROCEDURE FromName (name: TEXT): T =
  VAR preSize, size, postSize: TEXT;
  BEGIN
    SplitName (name, preSize, size, postSize);
    RETURN (NEW (T, preSize := preSize,
                    size := FLOAT (Scan.Int (size)) / 10.0,
                    postSize := postSize));
  END FromName;

<*INLINE*> PROCEDURE Size (if: T): REAL =
  BEGIN
    RETURN if.size;
  END Size;

PROCEDURE Scale (if: T; scale: REAL): T =
  BEGIN
    RETURN (NEW (T, preSize := if.preSize,
                    size := if.size / scale,
                    postSize := if.postSize));
  END Scale;

PROCEDURE ToFont (if: T; scale: REAL): Font.T 
    RAISES {Invisible, TooSmall, TooBig} =
  VAR size := TRUNC (if.size * scale);
  BEGIN
    IF size < 1 THEN 
      RAISE Invisible;
    ELSIF size < 6 THEN
      RAISE TooSmall;
    ELSE 
      (* Limit precision: get rid of all but the first n bits. *)
      size := Truncate (size);
      RETURN (FontCache.Get (if.preSize & 
                             Fmt.Int (size) & "0" & 
                             if.postSize));
    END;
  END ToFont;

PROCEDURE SplitName (full: TEXT; 
                     VAR preSize, size, postSize: TEXT) =
  VAR lastHyphen := 0;     (* to be set to the eigth hyphen *)
      nextHyphen: INTEGER; (* to be set to the ninth hyphen *)
  BEGIN
    FOR i := 2 TO 8 DO
      lastHyphen := Text.FindChar (full, '-', lastHyphen+1);
    END;
    preSize := Text.Sub (full, 0, lastHyphen+1);
    nextHyphen := Text.FindChar (full, '-', lastHyphen+1);
    size := Text.Sub (full, lastHyphen+1, nextHyphen-lastHyphen-1);
    postSize := Text.Sub (full, nextHyphen, 1000);
  END SplitName;

PROCEDURE Truncate (n: INTEGER): INTEGER =
  (* retains the first 4 bits of "n", filling the rest (of n) with zeroes,
     and returns it. *)
  VAR p := 1; (* to be set to the power of 2 just >= n *)
      m := n;
  BEGIN
    IF n >= 64 THEN RETURN 64 END;
    WHILE (m > 0) DO
      m := m DIV 2;
      p := p * 2;
    END;
    IF p <= 16 THEN 
      RETURN n;
    ELSE
      RETURN (n - (n MOD (p DIV 16)));
    END;
  END Truncate;


BEGIN
END ItemFont.
