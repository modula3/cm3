(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Mon Sep 27 14:35:19 PDT 1993 by mcjones    *)
(*      modified on Thu May 20 15:21:47 PDT 1993 by swart      *)
(*      modified on Mon Mar 23 10:28:06 PST 1992 by kalsow     *)
(*      modified on Thu Nov  2 18:13:34 1989 by muller         *)


(* A "TextWr.T", or text writer, is a writer the contents of whose
   internal buffer can be retrieved as a "TEXT".  Retrieving the
   buffer resets the target to be empty.  Text writers are buffered,
   seekable, and never raise "Failure" or "Alerted".  The fact that
   they are buffered is essentially unobservable, since there is no
   way for the client to access the target except through the text
   writer. *)

INTERFACE TextWr;

IMPORT Wr;

TYPE
  T <: Public;
  Public = Wr.T OBJECT METHODS init(): T END;

(* The call "wr.init()" initializes "wr" to be a seekable writer with
   "c(wr)" set to the empty sequence and "cur(wr)" to 0.  The
   writer has no upper bound on its length.*)

PROCEDURE New(): T;
(* Equivalent to "NEW(T).init()". *)

PROCEDURE ToText(wr: T): TEXT;
(* Return "c(wr)", resetting "c(wr)" to the empty sequence and
   "cur(wr)" to 0. *)

END TextWr.

