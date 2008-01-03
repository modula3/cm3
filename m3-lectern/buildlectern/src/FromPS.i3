(* Copyright 1994 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Thu Dec  8 11:12:31 PST 1994 by mcjones *)

(* Render PostScript to images and OCR data. *)

INTERFACE FromPS;

IMPORT File, Rd, Thread, Wr;

TYPE
  T <: Public;
  Public = OBJECT METHODS
    init(
      file: File.T;
      imageType := ImageType.PBMRAW;
      orientation := Orientation.Portrait;
      resolution := 300;
      ocr := TRUE;
      gs: TEXT := NIL;
      verbose := FALSE
      ): T RAISES {Error, Thread.Alerted};
    nextImage(VAR (*OUT*) rd: Rd.T): BOOLEAN RAISES {Error, Thread.Alerted};
    nextOCR(
      wr: Wr.T;
      height: CARDINAL;
      VAR (*OUT*) nWords, nWordsBytes: CARDINAL
      ) RAISES {Thread.Alerted, Wr.Failure}
  END;
  ImageType = {PBMRAW, PGMRAW, PPMRAW};
  Orientation = {Portrait, Landscape, LandscapeOther};

EXCEPTION Error(TEXT);

(* A "FromPS.T" is stream that delivers a sequence of images and, if
   "OCR=TRUE", corresponding OCR data (words and bounding boxes) by
   interpreting a PostScript file.  

   The call "t.init(f, i, o, r)" initializes "t" to a stream interpreting
   the PostScript file "f" with the specified image type, orientation,
   and resolution.  "f" is not closed.

   The call "t.nextImage(rd)" sets "rd" to a reader delivering an
   image of the next page and returns "TRUE", or returns "FALSE" if no
   more pages remain.  "rd" should {\em not} be closed.

   The call "t.nextOCR(wr, h, w, nW, nWB)" writes to "wr" a list of
   the recognized words followed by a list of the corresponding
   bounding boxes, sets "nW" to the number of words, sets "nWB" to the
   offset of the first bounding box relative to the initial position
   of "wr", and returns without closing "wr". The OCR data is
   delivered in the same format as by "OCR.FromPBM".  "nextOCR" should
   only be called {\em after} after a call of "t.nextImage(rd)" has
   returned "TRUE" {\em and} the corresponding image has been read
   from "rd".  "h" must be the height of the image (in scanlines) just
   read from "rd".

*)

END FromPS.
