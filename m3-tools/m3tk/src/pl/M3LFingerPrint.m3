(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)
(**)
(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)


UNSAFE MODULE M3LFingerPrint;

IMPORT Text, TextExtras, TextF, ASCII;
IMPORT M3LTypeToText, FingerPrint;


TYPE
  Handle = RECORD
    texts: REF ARRAY OF TEXT;
    components: REF ARRAY OF INTEGER;
    count: CARDINAL;
  END;


PROCEDURE Incremental(
    VAR handle: Handle;
    text: Text.T;
    VAR f: T)
    RAISES {}=
  VAR
    prev: CARDINAL;
    length := Text.Length(text);
    pos: CARDINAL := 0;
  BEGIN
    REPEAT
      prev := pos;
      WITH endOfText =
          NOT TextExtras.FindCharSet(text, 
                LOOPHOLE(M3LTypeToText.TypeIndexChars, ASCII.Set), pos) DO
        WITH sectionLength = pos - prev DO
          IF sectionLength # 0 THEN
            FingerPrint.DataIncremental(
                ADR(text[prev]), sectionLength, f.f1, f.f2);
            INC(handle.count, sectionLength);
          END;
        END;
        IF endOfText THEN EXIT END;
      END;
      VAR
        limit: CARDINAL;
        index: CARDINAL := 0;
      BEGIN
        CASE text[pos] OF <*NOWARN*>
        | M3LTypeToText.TypeIndexOneCh => limit := pos + 1;
        | M3LTypeToText.TypeIndexTwoCh => limit := pos + 2;
        | M3LTypeToText.TypeIndexThreeCh => limit := pos + 3;
        | M3LTypeToText.TypeIndexManyCh => limit := LAST(CARDINAL);
        END;
        LOOP
          INC(pos);
          IF pos > limit THEN EXIT END;
          WITH ch = text[pos] DO
            IF ch = M3LTypeToText.TypeIndexManyCh THEN INC(pos); EXIT END;
            index := index * M3LTypeToText.TypeIndexBase +
                (ORD(text[pos]) - ORD(M3LTypeToText.TypeIndexFirstDigitCh));
          END;
        END;
        WITH component = handle.components[index] DO
          IF component = -1 THEN
            component := index (* was "handle.count" *);
            Incremental(handle, handle.texts[index], f);
          ELSE
            FingerPrint.Incremental(
                ORD(M3LTypeToText.TypeIndexOneCh), f.f1, f.f2);
            FingerPrint.DataIncremental(
                ADR(component), BYTESIZE(component), f.f1, f.f2);
          END;
        END;
      END;
    UNTIL pos >= length;
  END Incremental;


PROCEDURE Generate(texts: REF ARRAY OF TEXT): REF ARRAY OF T RAISES {}=
  VAR
    handle: Handle;
    res := NEW(REF ARRAY OF T, NUMBER(texts^));
  BEGIN
    handle.texts := texts;
    handle.components := NEW(REF ARRAY OF INTEGER, NUMBER(texts^));
    FOR i := 0 TO LAST(texts^) DO
      handle.count := 0;
      FOR i := 0 TO LAST(handle.components^) DO
        handle.components[i] := -1;
      END;
      Incremental(handle, texts[i], res[i]);
    END; (* for *)
    RETURN res;
  END Generate;


BEGIN

END M3LFingerPrint.
