(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Mon Oct 31 11:47:24 PST 1994 by mcjones    *)

(* The "OCR" interface provides access to an optical character
   recognition algorithm, for recognizing ``words'' (sequences of
   non-whitespace ISO-Latin1 characters in a scanned image of a page
   of text).  The current implementation uses the DECimage Character
   Recognition Software library. *)

INTERFACE OCR;

IMPORT AtomList, Rd, Thread, Wr;

EXCEPTION Error(AtomList.T);

PROCEDURE IsFromPBMImplemented(): BOOLEAN;
(* Return "TRUE" if and only if "FromPBM" is implemented on this
   platform. *)

PROCEDURE FromPBM(
    rd: Rd.T;
    wr: Wr.T;
    VAR (*OUT*) nWords, nWordsBytes: CARDINAL;
    reject: CHAR := '#'; 
    resolution: CARDINAL := 300)
  RAISES {Rd.Failure, Wr.Failure, Thread.Alerted, Error};
(* Read from "rd" an image in portable bitmap format, apply an OCR
   algorithm to it, write to "wr" a list of the recognized words
   (substituting "reject" for each unrecognizable character) followed
   by a list of the corresponding bounding boxes, set "nWords" to the
   number of words, set "nWordsBytes" to the offset of the first
   bounding box relative to the initial position of "wr", and return
   without closing "rd" or "wr". *)

(* The input image may be in either plain or raw format, as described 
   in the pbm(5) manpage.

   The list of words is written to "wr" in the form:

|  {w \n}

   where "w" is a sequence of non-whitespace characters, as delivered
   by the OCR engine.  Each word contains at least one letter or
   digit.

   The list of bounding boxes written to "wr" in the form:

|  {w s e n}

   where each of "w", "s", "e", and "n" is a four-byte cardinal stored
   in little-endian order.  The origin for these coordinates is at the
   top left corner of the page, with positive coordinates representing
   positions below and to the right.  A bounding box includes every
   pixel position "(x, y)" such that "w <= x < e" and "n <= y < s".
   It is possible for "w = e" and/or "n = s", in which case the
   bounding box is empty (contains no pixels).

*)

END OCR.
