(* Copyright (C) 1994, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(* Last modified on Fri Jun 23 09:47:47 PDT 1995 by mcjones *)

MODULE OCR_Stub EXPORTS OCR;

IMPORT Rd, Wr;

PROCEDURE IsFromPBMImplemented(): BOOLEAN =
  BEGIN RETURN FALSE END IsFromPBMImplemented;

PROCEDURE FromPBM(
    <*UNUSED*> rd: Rd.T;
    <*UNUSED*> wr: Wr.T;
    VAR (*OUT*) nWords, nWordsBytes: CARDINAL;
    <*UNUSED*> reject: CHAR := '#';
    <*UNUSED*> resolution: CARDINAL := 300) =
  BEGIN
    nWords := 0;
    nWordsBytes := 0
  END FromPBM;

BEGIN
END OCR_Stub.
