(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Nov  9 16:58:50 PST 1994 by isard      *)

MODULE M3LoaderAccess;

IMPORT Word, Text, Wr, Stdio;
IMPORT M3LoaderUnsafe;

VAR
  textbufsize := 256;
  textbuffer  := NEW(REF ARRAY OF CHAR, textbufsize);

CONST
  sign_test = ARRAY [1 .. 4] OF Word.T
                { 16_80, 16_8000, 16_800000, 0 };
  sign_ext = ARRAY [1 .. 4] OF Word.T
                { 16_FFFFFF00, 16_FFFF0000, 16_FF000000, 0 };

PROCEDURE to_int (buf: Buffer; offset: INTEGER; size: [1 .. 4]): INTEGER =
  VAR int := 0;
  BEGIN
    FOR i := size-1 TO 0 BY -1 DO
      int := Word.Shift(int, 8) + buf[offset+i];
    END;

    IF Word.And(int, sign_test[size]) # 0 THEN
      int := Word.Or(int, sign_ext[size]);
    END;

    RETURN int;
  END to_int;

PROCEDURE from_int (buf: Buffer; offset, size, val: INTEGER) =
  BEGIN
    FOR i := 0 TO size-1 DO
      buf[offset+i] := Word.And(val, 16_FF);
      val := Word.Shift(val, -8);
    END
  END from_int;

PROCEDURE to_char (buf: Buffer; offset: INTEGER): CHAR =
  BEGIN
    RETURN VAL(buf[offset], CHAR);
  END to_char;

PROCEDURE to_text (buf: Buffer; offset, max_size: INTEGER): TEXT =
  VAR i := 0;
  BEGIN
    IF max_size > textbufsize THEN
      INC(textbufsize, max_size);
      textbuffer := NEW(REF ARRAY OF CHAR, textbufsize);
    END;

    WHILE buf[offset+i] # 0 AND i < max_size DO
      textbuffer[i] := VAL(buf[offset+i], CHAR);
      INC(i);
    END;

    RETURN Text.FromChars(SUBARRAY(textbuffer^, 0, i));
  END to_text;

PROCEDURE ascii_to_int (buf: Buffer; offset, max_size: INTEGER): INTEGER =
  VAR i, int := 0;
  BEGIN
    WHILE buf[offset+i] >= ORD('0') AND buf[offset+i] <= ORD('9') AND
          i < max_size DO
      int := int * 10 + (buf[offset+i] - ORD('0'));
      INC(i);
    END;

    RETURN int;
  END ascii_to_int;

PROCEDURE do_reloc (buf: Segment;
                    offset, val, type: INTEGER; load: BOOLEAN) =
  <* FATAL ANY *>
  VAR
    old_val: INTEGER;
    sign   := 1;
  BEGIN
    IF NOT load THEN
      sign := -1;
    END;

    CASE type OF
      1 =>  old_val := M3LoaderUnsafe.adr_to_int(buf, offset, 2);
            INC(old_val, sign * val);
            M3LoaderUnsafe.adr_from_int(buf, offset, 2, old_val);
    | 2 =>  old_val := M3LoaderUnsafe.adr_to_int(buf, offset, 2);
            INC(old_val, sign * (val - 2 - (buf.address + offset)));
            M3LoaderUnsafe.adr_from_int(buf, offset, 2, old_val);
    | 6 =>  old_val := M3LoaderUnsafe.adr_to_int(buf, offset, 4);
            INC(old_val, sign * val);
            M3LoaderUnsafe.adr_from_int(buf, offset, 4, old_val);
    | 7 =>  old_val := M3LoaderUnsafe.adr_to_int(buf, offset, 4);
            INC(old_val, sign *val);
            M3LoaderUnsafe.adr_from_int(buf, offset, 4, old_val);
    | 20 => old_val := M3LoaderUnsafe.adr_to_int(buf, offset, 4);
            INC(old_val, sign * (val - 4 - (buf.address + offset)));
            M3LoaderUnsafe.adr_from_int(buf, offset, 4, old_val);
    ELSE
      Wr.PutText(Stdio.stderr, "Unknown relocation slipped past obj reader\n");
      <* ASSERT FALSE *>
    END
  END do_reloc;

BEGIN
END M3LoaderAccess.
