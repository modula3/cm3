(*
   FBEPostScript.i3
   A Postscript backend for Formatter.i3.
   David Nichols, Xerox PARC
   July, 1991

   $Id: FBEPostScript.i3,v 1.1.1.1 2001-01-13 14:47:03 wagner Exp $
*)
(* Copyright (c) 1991 Xerox Corporation.  All rights reserved.

   Use and copying of this software and preparation of derivative works based
   upon this software are permitted.  Any distribution of this software or
   derivative works must comply with all applicable United States export
   control laws.  This software is made available AS IS, and Xerox Corporation
   makes no warranty about the software, its performance or its conformity to
   any specification. *)

INTERFACE FBEPostScript;

IMPORT FBE, Wr;

(* Returns an FBE.T that writes PostScript to the underlying Wr.T.  You need to
   allocate all your fonts before writing output so that font definitions can
   go in the prolog.  The two strings passed in the comment array are printed
   in the upper left-hand corner of each page.  m3pp uses them for the
   modification date and time of the source file. *)
PROCEDURE New (wr       : Wr.T;
               title    : TEXT;
               comment           := ARRAY [0 .. 1] OF TEXT{"", ""};
               landscape         := TRUE                            ): FBE.T
  RAISES {FBE.Failed};

END FBEPostScript.
