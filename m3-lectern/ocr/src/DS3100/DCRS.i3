(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Thu Mar 24 16:03:21 PST 1994 by mcjones    *)

(* "DCRS" is a Modula-3 interface to DECimage Character Recognition
   Software.  The character recognition process involves several
   steps.  First you acquire an image of a page (e.g., via a scanner
   or PostScript renderer) and use it to create an object of type
   "DCRS.T".  Then you ``segment'' the image, or subdivide it into one
   or more disjoint rectangular segments, each containing text or
   graphics.  At this point you can optionally delete one or more of
   the segments.  Then you perform the actual recognition.  Finally,
   you recover the text and, optionally, font and position
   information, in any of several formats. *)

INTERFACE DCRS;

IMPORT AtomList, Rd, Rect, TextList, Thread;

EXCEPTION Error(AtomList.T);

TYPE
  State = {New, Segmented, Recognized, Closed};
  Resolution = {DPI200, DPI300, DPI400};

TYPE T <: REFANY;

(* A "DCRS.T", say "i", has these components:
| image(i)  `a rectangular array of pixels, each black or white`
| res(i)    `a value of type "Resolution"`
| state(i)  `a value of type "State"`

   and, if "state(i) >= State.Segmented":

| segs(i)   `a sequence of disjoint rectangular regions of the image`

   and, if "state(i) = State.Recognized":

| text(i) `a map from "segs(i)" to sequences of characters and layout
           information` *)

PROCEDURE FromPBM(
    rd: Rd.T;
    r := Resolution.DPI300): T
  RAISES {Rd.Failure, Thread.Alerted, Error};
(* Return a new "i" with these components:
| image(i)  `contents of portable bitmap file read from "rd"`
| res(i)    r
| state(i)  State.New
*)

PROCEDURE FromDDIF(
    pathname: TEXT;
    r := Resolution.DPI300)
  : T RAISES {Error};
(* Return a new "i" with these components:
| image(t)  `contents of DDIF image in file with pathname "p"`
| res(t)    r
| state(t)  State.New
*)

PROCEDURE Segment(
    i: T;
    READONLY regionOfInterest: Rect.T := Rect.Full;
    bypassSegmentation := FALSE)
  RAISES {Error};
(* If "state(t)=State.New", set "segs(i)" to a set of nonoverlapping
   rectangular segments and set "state(i) := State.Segmented".  If "r
   # NIL", it specifies a rectangular region to which the segmentation
   process is restricted.  If "bypass" is "TRUE", the entire image, or
   entire region of interest if "r # NIL", is considered to be one
   large segment. It's a checked runtime error if "Segment" is called
   with "state(i) > State.New". *)

TYPE SegmentFilter = OBJECT METHODS
    keep(
      segmentNumber: CARDINAL;
      blockNumber: CARDINAL;
      READONLY r: Rect.T;
      text: BOOLEAN): BOOLEAN
  END;

PROCEDURE FilterSegments(i: T; f: SegmentFilter);
(* Call "f", which is an instance of a client-defined subtype of
   "SegmentFilter", on each block of each segment of "i".  Each call
   "f.keep(n, b, r, t)" specifies a block "b" within a segment "n" as
   well as the extent "r" of the block and the type "t" ("TRUE" means
   text; "FALSE" means graphics).  "FilterSegments" deletes each
   segment for which "f.keep" returns "FALSE". *)

PROCEDURE Recognize(
    i: T;
    c: CharSet := NIL;
    d: DictionaryContext := NIL;
    forceMonospace := FALSE)
  RAISES {Error};
(* If "state(t) = State.New", first perform "Segment(i)".  Then, if
   "state(t) < State.Recognized", run the recognition algorithm on "i"
   and set "state(i) := State.Recognized".  It's a checked runtime
   error if "Recognize" is called with "state(i)>State.Segmented". *)

TYPE
  Language = {ISOLatin1, CanadianFrench, Danish, Dutch,
    English, Finnish, French, German, Icelandic, Italian,
    Norwegian, Spanish, Swedish, Numeric};
  CharSet <: OBJECT METHODS
    init(language := Language.ISOLatin1): CharSet;
    custom(READONLY charset: ARRAY [0..255] OF BOOLEAN)
      : CharSet
  END;
  DictionaryContext <: OBJECT METHODS
    init(
        primary := Language.English;
        userDictPathnames: TextList.T := NIL)
      : DictionaryContext
  END;

PROCEDURE GetWords(
    i: T; ws: WordSink; reject: CHAR; seg: CARDINAL := 0)
  RAISES ANY;
(* If "state(t) < State.Recognized", first perform
   "Recognized(i)". Call "w", which is an instance of a client-defined
   subtype of "WordSink", on each word or layout item of every
   segment, if "seg=0", or of segment number "seg", if "seg#0".  "w"'s
   "word" method is called with the text (with each unrecognizable
   character replaced by "rej"), extent, font, and font style of each
   word in order.  "w"'s "fontChange" method is called each time there
   is a change in the font style or font size of the text.  "w"'s
   "newLine" method is called each time there is a newline in the
   text.  The only exceptions raised by "GetWords" are those raised by
   calls on "w"'s methods. *)

TYPE WordSink = OBJECT METHODS
    word(
      segmentNumber, blockNumber: CARDINAL;
      w: TEXT;
      READONLY r: Rect.T) RAISES ANY;
    fontChange(
      segmentNumber, blockNumber: CARDINAL;
      f: Font;
      italic, bold, underline: BOOLEAN;
      pointSize: CARDINAL) RAISES ANY;
    newLine(segmentNumber, blockNumber: CARDINAL) RAISES ANY
  END;
  Font = {Courier, Helvetica, Times, Unknown};

PROCEDURE ExportText(
    i: T;
    reject: CHAR;
    wysiwyg: BOOLEAN := FALSE)
  : TEXT RAISES {Error};
(* If "state(t) < State.Recognized", first perform
   "Recognized(i)". Return the recognized text from "i".  Each
   unrecognizable character is replaced by "rej".  If "wysiwyg" is
   "FALSE", the result is ``decolumnized'' into a single column;
   otherwise, the result matches the page layout of the original
   document as closely as possible. *)

PROCEDURE ExportDDIF(
    i: T; reject: CHAR; inclImages: BOOLEAN; p: TEXT)
  RAISES{Error};
(* If "state(t) < State.Recognized", first perform
   "Recognized(i)". Write the recognized text and layout information
   as a DDIF file with pathname "p".  Each unrecognizable character is
   replaced by "rej".  If "inclImages" is "TRUE", the non-text
   segments of "i" are included in the file as graphical items. *)

PROCEDURE ExportPostScript(
    i: T;
    reject: CHAR;
    drawBorders, inclImages, relPositions: BOOLEAN)
  : TEXT RAISES {Error};
(* If "state(t) < State.Recognized", first perform "Recognized(i)".
   Return a PostScript program to recreate the recognized text and
   layout of "i".  Each unrecognizable character is replaced by "rej".
   If "drawBoarders" is "TRUE", the boundaries of the text and graphic
   segments will be outlined.  If "inclImages" is "TRUE", the non-text
   segments of "i" are included in the file as graphical items.  If
   "rel" is "TRUE", the generated PostScript attempts to minimize
   relative positioning errors rather than absolute positioning
   errors. *)

PROCEDURE Close(i: T);
(* Set "state(i) := State.Closed" and release all the resources used
   by "i". It is a checked runtime error to call any procedure in this
   interface when "state(i) = State.Closed". *)

END DCRS.

(* ***** TO DO:

   Remove "blockNumber" argument to "SegmentFilter.keep"?

   Should the recognition dictionary be a global variable?

   Should the reject character be a "Recognize" parameter? or a global
   variable?

*)
