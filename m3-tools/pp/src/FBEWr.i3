(*
   FBEWr.i3
   Traditional, fixed-width, ASCII backend for Formatter.
   David Nichols, Xerox PARC
   July, 1991

   $Id$
*)
(* Copyright (c) 1991 Xerox Corporation.  All rights reserved.

   Use and copying of this software and preparation of derivative works based
   upon this software are permitted.  Any distribution of this software or
   derivative works must comply with all applicable United States export
   control laws.  This software is made available AS IS, and Xerox Corporation
   makes no warranty about the software, its performance or its conformity to
   any specification. *)

INTERFACE FBEWr;

IMPORT FBE, Wr;

(* Returns a fixed-width FBE.T that writes to the underlying Wr.T.  It does not
   understand tab characters; it treats them as being one position wide, just
   like other chars. *)
PROCEDURE New (wr: Wr.T; width: INTEGER): FBE.T;

END FBEWr.
