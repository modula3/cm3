(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Mon Sep 27 14:35:11 PDT 1993 by mcjones    *)
(*      modified on Thu May 20 15:21:48 PDT 1993 by swart      *)
(*      modified on Tue Jan 28 12:09:43 PST 1992 by kalsow     *)
(*      modified on Thu Nov  2 18:13:15 1989 by muller         *)

(* A "TextRd.T", or text reader, is a reader that delivers the
   characters of a "TEXT" supplied when the reader was created.  Text
   readers are seekable, non-intermittent, and never raise "Failure"
   or "Alerted". *)

INTERFACE TextRd;

IMPORT Rd;

TYPE
  T <: Public;
  Public = Rd.T OBJECT METHODS init(t: TEXT): T END;

(* The call "rd.init(t)" initializes "rd" as a seekable,
   non-intermittent reader with:
| len(rd) = Text.Length(t)
| src(rd) = `characters of t`
| cur(rd) = 0

   It is a checked runtime error if "t = NIL". *)

PROCEDURE New(t: TEXT): T;
(* Equivalent to "NEW(T).init(t)". *)

END TextRd.
