(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: BuiltinOps.m3                                         *)
(* Last Modified On Tue Apr 16 01:18:12 1991 By kalsow         *)
(*      Modified On Thu Jul 27 17:30:12 1989 By muller         *)

MODULE BuiltinOps;

IMPORT Abs, Adr, AdrSize, BitSize;
IMPORT ByteSize, Ceiling, Dec, Dispose, First, Floatt, Floor;
IMPORT Inc, IsType, Last, Loophole, Max, Min, Narrow;
IMPORT New, Number, Ord, Round, Subarray, Trunc, Typecode, Val;


PROCEDURE Initialize () =
  BEGIN
    (* NOTE: this list is ordered! *)
    (* builtin procedures *)
    Abs.Initialize ();
    Adr.Initialize ();
    AdrSize.Initialize ();
    BitSize.Initialize ();
    ByteSize.Initialize ();
    Ceiling.Initialize ();
    Dec.Initialize ();
    Dispose.Initialize ();
    First.Initialize ();
    Floatt.Initialize ();
    Floor.Initialize ();
    Inc.Initialize ();
    IsType.Initialize ();
    Last.Initialize ();
    Loophole.Initialize ();
    Max.Initialize ();
    Min.Initialize ();
    Narrow.Initialize ();
    New.Initialize ();
    Number.Initialize ();
    Ord.Initialize ();
    Round.Initialize ();
    Subarray.Initialize ();
    Trunc.Initialize ();
    Typecode.Initialize ();
    Val.Initialize ();
  END Initialize;

BEGIN
END BuiltinOps.
