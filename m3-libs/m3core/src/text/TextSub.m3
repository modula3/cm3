(* Copyright 1997, Critical Mass, Inc.  All rights reserved. *)

UNSAFE MODULE TextSub EXPORTS Text, TextSub;

IMPORT TextClass, Text8, Text16, TextLiteral;

REVEAL
  TT = Public BRANDED "TextSub.T" OBJECT OVERRIDES
    get_info       := MyGetInfo;
    get_char       := MyGetChar;
    get_wide_char  := MyGetWideChar;
    get_chars      := MyGetChars;
    get_wide_chars := MyGetWideChars;
  END;

PROCEDURE New (t: TEXT; start, length: CARDINAL): TEXT =
  BEGIN
    RETURN Sub (t, start, length);
  END New;

(* Text.Sub *)
PROCEDURE Sub (t: TEXT; start, length: CARDINAL): TEXT =
  VAR info: TextClass.Info;  new_len: INTEGER;
  BEGIN
    t.get_info (info);
    new_len := MIN (info.length - start, length);
    IF (new_len <= 0)           THEN RETURN ""; END;
    IF (new_len = info.length)  THEN RETURN t;  END;
    IF (new_len = 1) THEN RETURN FromWideChar (t.get_wide_char (start)); END;

    (* Avoid building subtexts of subtexts... *)
    TYPECASE t OF
    | TT(tt) =>  t := tt.base;  INC (start, tt.start);
    ELSE         (* skip *)
    END;
    
    IF  (info.length >= 256)          (* It's big *)
    AND (new_len * 4 <= info.length)  (* It's shrinking substantially *)
    AND (new_len <= 16384) THEN       (* It's not huge *)
      VAR tc := TYPECODE (t); BEGIN
        (* don't bother flattening literals, they're not in the heap anyway! *)
        IF (tc # TYPECODE (TextLiteral.T)) THEN
          IF info.wide THEN
            VAR r := Text16.Create (new_len); BEGIN
              t.get_wide_chars (SUBARRAY (r.contents^, 0, new_len),  start);
              RETURN r;
            END;
          ELSE
            VAR r := Text8.Create (new_len); BEGIN
              t.get_chars (SUBARRAY (r.contents^, 0, new_len),  start);
              RETURN r;
            END;
          END;
        END;
      END;
    END;

    RETURN NEW (TT, base := t, start := start, len := new_len);
  END Sub;

PROCEDURE MyGetInfo (t: TT;  VAR info: TextClass.Info) =
  BEGIN
    t.base.get_info (info);
    info.length := t.len;
    IF (info.start # NIL) THEN
      IF info.wide
        THEN INC (info.start, t.start * ADRSIZE (WIDECHAR));
        ELSE INC (info.start, t.start * ADRSIZE (CHAR));
      END;
    END;
  END MyGetInfo;

PROCEDURE MyGetChar (t: TT;  i: CARDINAL): CHAR =
  BEGIN
    RETURN t.base.get_char (i + t.start);
  END MyGetChar;

PROCEDURE MyGetWideChar (t: TT;  i: CARDINAL): WIDECHAR =
  BEGIN
    RETURN t.base.get_wide_char (i + t.start);
  END MyGetWideChar;

PROCEDURE MyGetChars (t: TT;  VAR a: ARRAY OF CHAR;  start: CARDINAL) =
  BEGIN
    t.base.get_chars (a, start + t.start);
  END MyGetChars;

PROCEDURE MyGetWideChars (t: TT;  VAR a: ARRAY OF WIDECHAR;  start: CARDINAL) =
  BEGIN
    t.base.get_wide_chars (a, start + t.start);
  END MyGetWideChars;

BEGIN
END TextSub.
