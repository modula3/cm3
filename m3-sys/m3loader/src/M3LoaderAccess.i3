(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Nov  9 16:49:01 PST 1994 by isard      *)

INTERFACE M3LoaderAccess;

IMPORT File;

TYPE
  Segment = RECORD
    address : INTEGER;
    type    : SegType;
    size    : INTEGER;
  END;

TYPE
  Buffer = REF ARRAY OF File.Byte;

TYPE SegType = { Text, Data, Bss };

PROCEDURE to_int (buf: Buffer; offset: INTEGER; size: [1 .. 4]): INTEGER;
PROCEDURE from_int (buf: Buffer; offset, size, val: INTEGER);
PROCEDURE to_char (buf: Buffer; offset: INTEGER): CHAR;
PROCEDURE to_text (buf: Buffer; offset, max_size: INTEGER): TEXT;
PROCEDURE ascii_to_int (buf: Buffer; offset, max_size: INTEGER): INTEGER;

PROCEDURE do_reloc (buf: Segment;
                    offset, val, type: INTEGER; load: BOOLEAN);

(* List of legal values for relocation types *)
CONST RelocTable = ARRAY OF INTEGER { 1, 2, 6, 7, 20 };

END M3LoaderAccess.
