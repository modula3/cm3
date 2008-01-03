(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Nov  9 18:12:12 PST 1994 by isard      *)

UNSAFE MODULE M3LoaderUnsafe;

IMPORT File, RTMisc, Word;
IMPORT M3LoaderAccess;

FROM M3LoaderAccess IMPORT Buffer, Segment;

TYPE
  Proc = PROCEDURE ();

PROCEDURE call (address: INTEGER) =
  VAR
    fake_proc := LOOPHOLE(address, Proc);
  BEGIN
    fake_proc();
  END call;

CONST
  sign_test = ARRAY [1 .. 4] OF Word.T
                { 16_80, 16_8000, 16_800000, 0 };
  sign_ext = ARRAY [1 .. 4] OF Word.T
                { 16_FFFFFF00, 16_FFFF0000, 16_FF000000, 0 };

PROCEDURE adr_to_int (buf: Segment; offset: INTEGER; size: [1 .. 4]): INTEGER =
  VAR
    int := 0;
    ptr : UNTRACED REF File.Byte;
  BEGIN
    ptr := LOOPHOLE(buf.address + offset + size-1, ADDRESS);

    FOR i := size-1 TO 0 BY -1 DO
      int := Word.Shift(int, 8) + (ptr)^;
      DEC(ptr);
    END;

    IF Word.And(int, sign_test[size]) # 0 THEN
      int := Word.Or(int, sign_ext[size]);
    END;

    RETURN int;
  END adr_to_int;

PROCEDURE adr_from_int (buf: Segment; offset, size, val: INTEGER) =
  VAR
    ptr : UNTRACED REF File.Byte;
  BEGIN
    ptr := LOOPHOLE(buf.address + offset, ADDRESS);

    FOR i := 0 TO size-1 DO
      ptr^ := Word.And(val, 16_FF);
      val := Word.Shift(val, -8);
      INC(ptr);
    END
  END adr_from_int;

PROCEDURE copy_buf_to_seg (src: Buffer;
                           srcoffs, dest, destoffs, size: INTEGER) =
  VAR
    srcadr,
    destadr : ADDRESS;
  BEGIN
    srcadr := LOOPHOLE(ADR(src[0])+srcoffs, ADDRESS);
    destadr := LOOPHOLE(dest+destoffs, ADDRESS);
    RTMisc.Copy(srcadr, destadr, size);
  END copy_buf_to_seg;

PROCEDURE copy_data (src, srcoffs, dest, destoffs, size: INTEGER) =
  VAR
    srcptr,
    destptr : ADDRESS;
  BEGIN
    srcptr := LOOPHOLE(src + srcoffs, ADDRESS);
    destptr := LOOPHOLE(dest + destoffs, ADDRESS);

    RTMisc.Copy(srcptr, destptr, size);
  END copy_data;

PROCEDURE zero_segment (buf: Segment) =
  BEGIN
    RTMisc.Zero(LOOPHOLE(buf.address, ADDRESS), buf.size);
  END zero_segment; 

PROCEDURE zero_data (place, length: INTEGER) =
  BEGIN
    RTMisc.Zero(LOOPHOLE(place, ADDRESS), length);
  END zero_data; 

BEGIN
END M3LoaderUnsafe.
