(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

UNSAFE MODULE Mpz;

IMPORT MpzRep;

IMPORT MpzP AS P;
IMPORT WeakRef;
IMPORT M3toC;
IMPORT Word;

PROCEDURE New() : T =
  VAR
    res := NEW(T);
  BEGIN
    P.c_init(LOOPHOLE(ADR(res.val), P.MpzPtrT));
    EVAL WeakRef.FromRef(res, CleanUp);
    set_ui(res, 0);
    RETURN res
  END New;

PROCEDURE CleanUp(<*UNUSED*>READONLY w : WeakRef.T; r : REFANY) =
  BEGIN
    WITH this = NARROW(r, T) DO
      P.c_clear(ADR(this.val))
    END
  END CleanUp;

PROCEDURE Import(t : T; READONLY data : ARRAY OF Word.T) =
  BEGIN
    P.import(ADR(t.val), NUMBER(data), -1, BYTESIZE(Word.T), 0, 0, ADR(data[0]))
  END Import;

PROCEDURE Export(VAR data : ARRAY OF Word.T; t : T) =
  CONST
    numb = BITSIZE(Word.T);
  BEGIN
    FOR i := FIRST(data) TO LAST(data) DO
      data[i] := 0
    END;
    
    WITH count = (P.c_sizeinbase(ADR(t.val), 2) + numb - 1) DIV numb DO
      IF count > NUMBER(data) THEN
        (* number is too big, truncate it *)
        VAR
          ndata := NEW(REF ARRAY OF Word.T, count);
        BEGIN
          P.export(ADR(ndata[0]), NIL, -1, BYTESIZE(Word.T), 0, 0, ADR(t.val));
          data := SUBARRAY(ndata^, 0, NUMBER(data));
        END
      ELSE
        P.export(ADR(data[0]), NIL, -1, BYTESIZE(Word.T), 0, 0, ADR(t.val))
      END
    END
  END Export;

PROCEDURE FormatDecimal(t : T) : TEXT =
  VAR
    cs := P.format_decimal(ADR(t.val));
  BEGIN
    TRY
      RETURN M3toC.CopyStoT(cs)
    FINALLY
      P.free_formatted(cs)
    END
  END FormatDecimal;

PROCEDURE FormatHexadecimal(t : T) : TEXT =
  VAR
    cs := P.format_hexadecimal(ADR(t.val));
  BEGIN
    TRY
      RETURN M3toC.CopyStoT(cs)
    FINALLY
      P.free_formatted(cs)
    END
  END FormatHexadecimal;

PROCEDURE FormatOctal(t : T) : TEXT =
  VAR
    cs := P.format_octal(ADR(t.val));
  BEGIN
    TRY
      RETURN M3toC.CopyStoT(cs)
    FINALLY
      P.free_formatted(cs)
    END
  END FormatOctal;

PROCEDURE FormatBased(t : T; base : [-2..62]) : TEXT =
  VAR
    cs := P.format_based(ADR(t.val), base);
  BEGIN
    TRY
      RETURN M3toC.CopyStoT(cs)
    FINALLY
      P.free_formatted(cs)
    END
  END FormatBased;

BEGIN END Mpz.
