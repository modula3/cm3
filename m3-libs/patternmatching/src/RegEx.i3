(*

  RegEx.i3 - regex(3) style regular expressions

  This module implements regex(3) style regular expressions safe for
  multiple threads.  See "man regex" for a description of a regex
  expression.

  Edit History:
   Jan 30 1992		Schilit		Created.
   Feb 9  1993		Schilit		Update for release.

*)

(* Copyright (c) 1991, 1992 Xerox Corporation.  All rights reserved.

   Use and copying of this software and preparation of derivative works
   based upon this software are permitted.  Any distribution of this
   software or derivative works must comply with all applicable United
   States export control laws.  This software is made available AS IS, and
   Xerox Corporation makes no warranty about the software, its performance
   or its conformity to any specification. *)

INTERFACE RegEx;

EXCEPTION Error(TEXT);

TYPE Pattern <: ROOT;

CONST MEM_SIZE = 9;

TYPE
  Memory = ARRAY [1 .. MEM_SIZE] OF
             RECORD
               start: INTEGER;
               stop : INTEGER;
             END; (* record *)

(* The Memory array stores the positions where \(pat\) pairs occured or -1
   if none. *)

PROCEDURE Compile (pat: TEXT): Pattern RAISES {Error};
(* Compile the regex(3) style regular expression in pat and returns a
   Pattern for later use. *)

PROCEDURE Decompile (READONLY pat: Pattern): TEXT;
(* Decompile the Pattern 'pat' into a regular expression TEXT and
   return. *)

PROCEDURE Dump (READONLY pat: Pattern): TEXT;
(* Dump the Pattern 'pat' into raw bytes formatted as a TEXT (for
   debugging). *)

PROCEDURE Execute (READONLY pat  : Pattern;
                            data : TEXT;
                            start: CARDINAL := 0;
                            len  : CARDINAL := LAST(CARDINAL);
                            mem  : REF Memory := NIL           ): INTEGER;

(* Compare the regular expression 'pat' against the text data returning the
   starting position in 'data' if there was a match, -1 otherwise.  If
   optional parameter 'start' and 'len' are specified then consider only
   Text.Sub(data,start,len) portion of 'data.' If optional parameter mem is
   specified then return indices for \(..\) sequences. *)

END RegEx.
