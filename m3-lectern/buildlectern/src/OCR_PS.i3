(* Copyright 1994 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Thu Oct 20 17:13:19 PDT 1994 by mcjones *)

(* Recover OCR data from output of "ocr.ps". *)

INTERFACE OCR_PS;

IMPORT BBoxSeq, Rd, TextSeq;

TYPE
  T <: Public;
  Public = OBJECT METHODS
    init(rd: Rd.T; resolution: CARDINAL): T;
    nextPage(words: TextSeq.T; bBoxes: BBoxSeq.T): BOOLEAN
  END;

(* A "OCR_PS.T" is stream that delivers a sequence of OCR data (words
   and bounding boxes) recovered from the output from a special
   PostScript prolog (ocr.ps).

   The call "t.init(rd)" initializes "t" to a stream derived from
   "rd", which should be the standard output of a PostScript job run
   with the prolog ocr.ps and with output of "resolution" dots/inch.

   The call "t.nextPage(w, b)" appends to "w" and "b" the words and
   bounding boxes of the next page and returns "TRUE", or returns
   "FALSE" without changing "w" or "b" if the end of the document is
   reached.

*)

END OCR_PS.
