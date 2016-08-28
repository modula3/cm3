(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE TextSub EXPORTS Text, TextSub;

IMPORT TextCat, TextClass, Text8, Text8Short, Text16, Text16Short, TextLiteral;

(*47 IMPORT TextStats; 74*)
(*47 FROM TextStats IMPORT Op; 74*)

REVEAL
  TT = Public BRANDED "TextSub.T" OBJECT OVERRIDES
    get_info       := SubGetInfo;
    get_char       := SubGetChar;
    get_wide_char  := SubGetWideChar;
    get_chars      := SubGetChars;
    get_wide_chars := SubGetWideChars;
  END;

PROCEDURE New (t: TEXT; start, length: CARDINAL): TEXT =
  BEGIN
    RETURN Sub (t, start, length);
  END New;

(* EXPORTED *) 
(* Text.Sub *)
PROCEDURE Sub (t: TEXT; start, length: CARDINAL): TEXT =
  VAR info: TextClass.Info;  new_len: INTEGER;  root: TEXT;
  VAR c: CHAR;
  VAR wc: WIDECHAR; 
  BEGIN
    (*47 TextStats.NoteGround (Op.Sub); 74*) 
    t.get_info (info);
    new_len := MIN (info.length - start, length);
    IF (new_len <= 0)           THEN RETURN ""; END;
    IF (new_len = info.length)  THEN RETURN t;  END;
    IF (new_len = 1) THEN 
      IF info.wide THEN 
        (*47 TextStats.NoteGround (Op.get_wide_char); 74*) 
        wc := t.get_wide_char (start);
        (*47 TextStats.NoteFinished (Op.get_wide_char); 74*) 
        IF ORD (wc) <= ORD (LAST (CHAR)) THEN 
          RETURN FromChar (VAL (ORD (wc), CHAR));
        ELSE 
          RETURN FromWideChar (wc);
        END; 
      ELSE 
        (*47 TextStats.NoteGround (Op.get_char); 74*) 
        c := t.get_char (start); 
        (*47 TextStats.NoteFinished (Op.get_char); 74*) 
        RETURN FromChar (c); 
      END;
    END;

    (* Descend as far as possible through all subtexts and 
       concatenations that the result lies entirely within one
       side of. *)
    root := t;
    LOOP
      TYPECASE t OF
      | TT(tt) =>  t := tt.base;  INC (start, tt.start);
      | TextCat.T(tc) =>
          IF start + new_len <= tc.a_len THEN
            t := tc.a;
          ELSIF start >= tc.a_len THEN
            t := tc.b;
            DEC (start, tc.a_len);
          ELSE
            EXIT;
          END;
      ELSE
        EXIT;
      END;
    END;
    IF t # root THEN
      t.get_info (info);
      IF start = 0 AND new_len = info.length THEN
        RETURN t;
      END;
    END;
    
    IF  (info.length >= 256)          (* The base is big *)
    AND (new_len * 4 <= info.length)  (* It's shrinking substantially *)
    AND (new_len <= 16384) THEN       (* The result is not huge *)
      VAR tc := TYPECODE (t); BEGIN
        (* don't bother flattening literals, they're not in the heap anyway! *)
        IF (tc # TYPECODE (TextLiteral.T)) THEN
          IF info.wide THEN
            IF new_len <= Text16Short.MaxLength THEN 
              (* Flatten into a Text16Short. *) 
              VAR r := NEW (Text16Short.T); BEGIN
                r.len := new_len;
                (*47 TextStats.NoteGround (Op.get_wide_chars); 74*) 
                t.get_wide_chars (SUBARRAY (r.contents, 0, new_len),  start);
                (*47 TextStats.NoteFinished (Op.get_wide_chars); 74*) 
                r.contents[new_len] := VAL (0,WIDECHAR);
                (*47 TextStats.NoteAllocText16Short(r); 74*)
                RETURN r;
              END (* Block *);
            ELSE (* Flatten into a Text16. *) 
              VAR r := Text16.Create (new_len); BEGIN
                (*47 TextStats.NoteGround (Op.get_wide_chars); 74*) 
                t.get_wide_chars (SUBARRAY (r.contents^, 0, new_len),  start);
                (*47 TextStats.NoteFinished (Op.get_wide_chars); 74*) 
                RETURN r;
              END (* Block *);
            END;
          ELSE (* No wide chars. *) 
            IF new_len <= Text8Short.MaxLength THEN
              (* Flatten into a Text8Short. *) 
              VAR r := NEW (Text8Short.T); BEGIN
                r.len := new_len;
                (*47 TextStats.NoteGround (Op.get_chars); 74*) 
                t.get_chars (SUBARRAY (r.contents, 0, new_len),  start);
                (*47 TextStats.NoteFinished (Op.get_chars); 74*) 
                r.contents[new_len] := '\000';
                (*47 TextStats.NoteAllocText8Short(r); 74*)
                RETURN r;
              END (* Block *);
            ELSE (* Flatten into a Text8. *) 
              VAR r := Text8.Create (new_len); BEGIN
                (*47 TextStats.NoteGround (Op.get_chars); 74*) 
                t.get_chars (SUBARRAY (r.contents^, 0, new_len),  start);
                (*47 TextStats.NoteFinished (Op.get_chars); 74*) 
                RETURN r;
              END (* Block *);
            END;
          END;
        END;
      END (* Block *);
    END;
   
    VAR r := NEW (TT, base := t, start := start, len := new_len); BEGIN 
      (*47 TextStats.NoteAllocTextSub(r); 74*)
      (*47 TextStats.NoteFinished (Op.Sub); 74*) 
      RETURN r;
    END (* Block *)
  END Sub;

PROCEDURE SubGetInfo (t: TT;  VAR info: TextClass.Info) =
  BEGIN
    t.base.get_info (info);
    info.length := t.len;
    IF (info.start # NIL) THEN
      IF info.wide
        THEN INC (info.start, t.start * ADRSIZE (WIDECHAR));
        ELSE INC (info.start, t.start * ADRSIZE (CHAR));
      END;
    END;
  END SubGetInfo;

PROCEDURE SubGetChar (t: TT;  i: CARDINAL): CHAR =
  BEGIN
    (*47 TextStats.NoteRecurse (Op.get_char); 74*) 
    RETURN t.base.get_char (i + t.start);
  END SubGetChar;

PROCEDURE SubGetWideChar (t: TT;  i: CARDINAL): WIDECHAR =
  BEGIN
    (*47 TextStats.NoteRecurse (Op.get_wide_char); 74*) 
    RETURN t.base.get_wide_char (i + t.start);
  END SubGetWideChar;

PROCEDURE SubGetChars (t: TT;  VAR a: ARRAY OF CHAR;  start: CARDINAL) =
  BEGIN
    (*47 TextStats.NoteRecurse (Op.get_chars); 74*) 
    t.base.get_chars (a, start + t.start);
  END SubGetChars;

PROCEDURE SubGetWideChars (t: TT;  VAR a: ARRAY OF WIDECHAR;  start: CARDINAL) =
  BEGIN
    (*47 TextStats.NoteRecurse (Op.get_wide_chars); 74*) 
    t.base.get_wide_chars (a, start + t.start);
  END SubGetWideChars;

BEGIN
END TextSub.
