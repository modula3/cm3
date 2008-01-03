(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Nov  9 18:10:02 PST 1994 by isard      *)

INTERFACE M3LoaderUnsafe;

IMPORT M3LoaderAccess;

PROCEDURE copy_data (src, srcoffs, dest, destoffs, size: INTEGER);
PROCEDURE copy_buf_to_seg (src: M3LoaderAccess.Buffer;
                           srcoffs, dest, destoffs, size: INTEGER);
PROCEDURE adr_to_int (buf: M3LoaderAccess.Segment; offset: INTEGER;
                      size: [1 .. 4]): INTEGER;
PROCEDURE adr_from_int (buf: M3LoaderAccess.Segment;
                        offset, size, val: INTEGER);
PROCEDURE zero_segment (buf: M3LoaderAccess.Segment);
PROCEDURE zero_data (place, length: INTEGER);
PROCEDURE call (address: INTEGER);

END M3LoaderUnsafe.
