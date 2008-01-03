(* Copyright 1990 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Lectern: a user interface for viewing documents stored as images *)
(* Converting files to Lectern's own lgm format *)

(* Last modified on Mon Feb  6 14:06:48 PST 1995 by birrell   *)
(*      modified on Thu Oct 20 16:54:59 PDT 1994 by mcjones   *)

INTERFACE LGM;

IMPORT Images, RegularFile, Thread, Wr;

EXCEPTION Error(TEXT);

TYPE T <: REFANY;

PROCEDURE ReadImage(file: RegularFile.T; start, length: INTEGER; VAR t: T)
                   RAISES { Error };
  (* Reads an image in a suitable format and assigns it to "t", for
     subequent uses in calls of "GetFormat" and "Reduce".  Accepts any image
     format accepted by ImageRd.T.  If "t" is initially non-NIL, it will be
     re-used if possible (to avoid memory allocation costs).  Raises "Error"
     if "file" doesn't contain a valid image.  The file should be seekable. 
     This implementation never closes the file, which should remain readable
     and immutable until the last call of "Reduce". *)

TYPE Format = { PBM, PGM, PPM, LGM };

PROCEDURE GetFormat(t: T): Format;
  (* Returns the format of the image file in the reader. *)

PROCEDURE GetHeight(t: T): CARDINAL;
  (* Returns the height in scan lines of the image file in the reader. *)

PROCEDURE Reduce(t: T; scale: INTEGER; gamma: REAL; levels: INTEGER;
                 diffuse: BOOLEAN;
                 verbose: BOOLEAN): Images.Contents
                RAISES { Error, Wr.Failure, Thread.Alerted};
  (* Returns an image based on "t", down-scaled by "scale", adjusted by
     "gamma" ( <1.0 darkens), and reduced to at most "levels" levels in each
     color channel.  Iff "diffuse", color reduction uses an error diffusion
     algorithm; otherwise it just posterizes.
     NOTE: "gamma" is replaced by 1.0 if scale=1.
     NOTE: multiple calls of "Reduce" for a single call of
           "Read" will likely perform well. *)

PROCEDURE Compress(contents: Images.Contents; scale: INTEGER;
                   wr: Wr.T; verbose: BOOLEAN)
                RAISES { Wr.Failure, Thread.Alerted };
  (* Write a compressed form of "contents" on "wr". The parameter "scale"
     is recorded in the output file, but has no other effect.
     The output format is acceptable to ImageRd.T.  *)

END LGM.
