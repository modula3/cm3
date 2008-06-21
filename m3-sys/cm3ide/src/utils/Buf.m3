(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: M3File.m3                                             *)
(* Last modified on Tue Mar  7 11:10:02 PST 1995 by kalsow     *)

UNSAFE MODULE Buf;

IMPORT FS, File, OSError, Text;

CONST
  N = 16_1000000; (* == 2^24 == 16MBytes *)

TYPE
  Ptr = UNTRACED REF ARRAY [0..N-1] OF File.Byte;

PROCEDURE FromFile (path: TEXT;  pad: CARDINAL): T =
  VAR f: File.T;  len, next, got: INTEGER;  t: T;  p: Ptr;
  BEGIN
    TRY
      f := FS.OpenFileReadonly (path);
      len := f.status().size;
      t := NEW (T, len + pad);

      next := 0;
      WHILE (len > N) DO
        p := LOOPHOLE (ADR (t[next]), Ptr);
        got := f.read (p^, mayBlock := TRUE);
        INC (next, got);
        DEC (len, got);
      END;

      WHILE (len > 0) DO
        p := LOOPHOLE (ADR (t[next]), Ptr);
        got := f.read (SUBARRAY (p^, 0, len), mayBlock := TRUE);
        INC (next, got);
        DEC (len, got);
      END;

      f.close ();
      RETURN t;
    EXCEPT OSError.E =>
      RETURN NIL;
    END;
  END FromFile;

PROCEDURE FromText (txt: TEXT): T =
  VAR t := NEW (T, Text.Length (txt));
  BEGIN
    Text.SetChars (t^, txt);
    RETURN t;
  END FromText;

BEGIN
  <*ASSERT BYTESIZE (File.Byte) = BYTESIZE (CHAR) *>
END Buf.

