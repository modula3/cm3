(* Copyright 1990 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Lectern: interface for lectern document format *)

(* Last modified on Fri Apr  7 15:49:57 PDT 1995 by birrell   *)

INTERFACE LecternDoc;

IMPORT Rd, Thread, Wr;

(* This interface describes the format of a Lectern document.  The document
   consists of a fixed 4-byte header, followed by a sequence of components
   and a directory (in any order), followed by a 4-byte trailer that specifies
   the position of the start of the directory.  The directory specifies the
   start and length of the other components. *)


(* *)
(* Types *)
(* *)

TYPE Component = RECORD start, length: INTEGER := 0 END;
  (* A record describing one component of the document: the component
     consists of the bytes [start..start+length) from the document.  If
     start=0, the component is absent from the document.  The component
     itself may be in any of a variety of formats, including PostScript or
     one of the image file formats such as PBM, PGM, PPM, LGM or TIFF.  The
     actual format is determined from context or by examining the component,
     typically its first two bytes, e.g. "%!" for PostScript or "P6" for PPM.
     *)

TYPE Class = { Thumbnail, Small, Normal, Large, Print,
               OCRWords, OCRRects, Hypertext, Annotations };
  (* The categories of component for a page.  Their intended usages are:
           Thumbnail:   very small reduced image (about 1/48th scale)
           Small:       image for reduced screen viewing (75 DPI)
           Normal:      default image for displaying on screen (100 DPI)
           Large:       image for magnified screen viewing (150 DPI)
           Print:       image for printing (300 DPI)
           OCRWords:    words in the page as delivered by OCR.i3
           OCRRects:    bounding rectangles corresponding to OCRWords; 300 DPI
           Hypertext:   hypertext links and anchors, format t.b.d.
           Annotations: annotation data, format t.b.d.
     *)

TYPE Scales = ARRAY Class OF INTEGER;
  (* Factor by which each class of component is down-scaled from Class.Print *)

CONST DefaultScales = Scales{ 48, 4, 3, 2, 1, 1, 1, 1, 1 };
  (* Default scale factors *)

TYPE DirPage = ARRAY Class OF Component;
  (* The data for a single page *)

TYPE DirPages = REF ARRAY OF DirPage;
  (* The data for all the pages of the document, numbered from 0. *)

TYPE Attribute = RECORD key, value: TEXT END;
  (* A general mechanism for including key-value attributes in the document.
     The keys are centrally registered with the Lectern developer(s), and
     are generally defined within this interface.  The values are arbitrary.
     *)

TYPE Attributes = REF ARRAY OF Attribute;

TYPE Gammas = REF ARRAY OF REAL;
  (* The gamma adjustments that were applied when reducing each page, indexed
     by image number starting from 0. *)

TYPE Dir = RECORD
  (* The directory for a lectern document, stored within the document. *)
    origin: INTEGER := 0;    (* The index in DirPages for logical page 0.
                                Might be negative if first page of DirPages
                                has logical page number 1 or greater. *)
    contents: INTEGER := 0;  (* The index in DirPages for first contents page.
                                Might be negative if there is no contents
                                page within DirPages. *)
    index: INTEGER := 0;     (* The index in DirPages for first index page.
                                Might be negative if there is no index page
                                within DirPages. *)
    outline: Component;      (* Outline links, if there are any *)
    original: Component;     (* Original PostScript, if available *)
    pages: DirPages;         (* Data for each page *)
    attributes: Attributes;  (* Arbitrary attributes of the document *)
    gammas: Gammas;          (* Gamma correction history *)
  END;


(* *)
(* Writing a document *)
(* *)

PROCEDURE WriteHeader(wr: Wr.T) RAISES {Wr.Failure, Thread.Alerted};
  (* Appends to "wr" the fixed 8-byte lectern document header. *)

PROCEDURE WriteDir(wr: Wr.T; READONLY dir: Dir) RAISES {Wr.Failure,
                                                        Thread.Alerted};
  (* Appends to "wr" a representation of "dir" *)

PROCEDURE WriteTrailer(wr: Wr.T; dirPos: CARDINAL) RAISES {Wr.Failure,
                                                           Thread.Alerted};
  (* Appends to "wr" a trailer specifying "dirPos" as the start of the
     lectern document's directory. *)


(* *)
(* Reading a document *)
(* *)

EXCEPTION NotLectern;
  (* Raised if the reader apparently is not for a lectern document *)

PROCEDURE ReadDir(rd: Rd.T; VAR dir: Dir) RAISES { Rd.Failure, Thread.Alerted,
                                                   NotLectern };
  (* Reads the directory from the lectern document stored in "rd".  The
     document's header must be at the reader's position 0, and the document's
     trailer must end at the reader's end-of-file.  The reader must be
     seekable.  Raises "NotLectern" on any syntax error or unexpected
     end-of-file. On return, dir.pages, dir.attributes and dir.gammas are
     non-nil (though they mught be zero length, of course). *)


(* *)
(* I/O subroutines, for other uses *)
(* *)

CONST
  IntBias = 32000;
  RealFactor = 1024.0;

PROCEDURE PutInt4(wr: Wr.T; n: INTEGER)
                  RAISES { Wr.Failure, Thread.Alerted };
  (* Writes an integer onto wr.  Requires n >= IntBias and < 2^31-IntBias.
     Occupies 4 bytes. *)

PROCEDURE PutReal(wr: Wr.T; r: REAL)
                  RAISES { Wr.Failure, Thread.Alerted };
  (* Write "r" onto wr, encoded by writing the integer ROUND(r*RealFactor).
     This restricts the accuracy, and requires that r >= -IntBias/RealFactor.
     Occupies 4 bytes. *)

PROCEDURE PutText(wr: Wr.T; t: TEXT) RAISES { Wr.Failure, Thread.Alerted };
  (* Write "t" onto wr.  "t" can have arbitrary length and content.
     Occupies 4+Text.Length(t) bytes. *)

PROCEDURE ReadInt4(rd: Rd.T): INTEGER RAISES { Rd.Failure, Thread.Alerted,
                                               Rd.EndOfFile };
  (* Read an integer as written bu PutInt4 *)

PROCEDURE ReadReal(rd: Rd.T): REAL RAISES { Rd.Failure, Thread.Alerted,
                                               Rd.EndOfFile };
  (* Read a real as written by PutInt4 *)

PROCEDURE ReadText(rd: Rd.T): TEXT RAISES { Rd.Failure, Thread.Alerted,
                                            Rd.EndOfFile, NotLectern };
  (* Read a text as written by PutText.  Raises NotLectern if the length
     is illegal (negative) or excessive (100000). *)

PROCEDURE CopyRd(rd: Rd.T; length: INTEGER; wr: Wr.T): Component
                RAISES { Rd.Failure, Wr.Failure, Thread.Alerted };
  (* Copy "length" bytes from current position of "rd" into "wr". Result
     is a "component" describing the resulting content of "wr". *)

PROCEDURE CopyComponent(rd: Rd.T; from: Component; wr: Wr.T): Component
                        RAISES { Rd.Failure, Wr.Failure, Thread.Alerted };
  (* Copy the component "from" from "rd" into "wr". Result is a "component"
     describing the resulting contents of "wr".  A component with .start=0
     causes no bytes to be copied and returns the empty component. *)

END LecternDoc.
