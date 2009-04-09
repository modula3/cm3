(* Copyright 1996-2003 John D. Polstra.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgment:
 *      This product includes software developed by John D. Polstra.
 * 4. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * $Id: RCSDelta.i3,v 1.1.1.1 2009-04-09 17:01:56 jkrell Exp $ *)

(* The "RCSDelta" interface supports operations on the individual deltas
   of an RCS file. *)

INTERFACE RCSDelta;

IMPORT RCSDate, RCSError, RCSPhrases, RCSRevNum, RCSString;

CONST
  Brand = "RCSDelta";

TYPE
  T <: Public;

  Public = OBJECT
    revision: RCSRevNum.T := NIL;
    date: RCSDate.T := NIL;
    author: TEXT := NIL;
    state: TEXT := NIL;
  END;

  Iterator = OBJECT METHODS
    next(VAR delta: T): BOOLEAN;
  END;

(* An "RCSDelta.T", or delta, represents a single revision of an RCS
   file.  Do not create a delta explicitly; use "RCSFile.AddDelta"
   instead.

   An "Iterator" is an object for iterating over a set of deltas. *)

PROCEDURE Compare(d1, d2: T): [-1..1];
(* Compare two deltas according to their revision numbers. *)

PROCEDURE Dead(delta: T; inAttic: BOOLEAN): BOOLEAN;
(* Return an indication of whether the given delta is dead, according to
   CVS rules. *)

PROCEDURE Equal(d1, d2: T): BOOLEAN;
(* Test whether two deltas have identical revision numbers. *)

PROCEDURE GetBranch(delta: T; revNum: RCSRevNum.T): T
  RAISES {RCSError.E};
(* Gets the first delta of the given branch.  Raises the exception if
   the branch does not exist. *)

PROCEDURE GetLog(delta: T): RCSString.T
  RAISES {RCSError.E};
(* Return a string containing the log entry of the given revision. *)

PROCEDURE GetText(delta: T;
                  diffBase: T := NIL): RCSString.Iterator
  RAISES {RCSError.E};
(* Return a string iterator for retrieving the revision's text, in full,
   or as a diff relative to "diffBase". *)

(* With the default "diffBase" of "NIL", "GetText" retrieves the full
   text for "delta".  Otherwise, "GetText" attempts to calculate
   the diffs required to transform the "diffBase" delta into "delta".
   It is not possible to do this in all cases, although in practice,
   the important cases are covered.  "GetText" is able to reverse
   an existing set of diffs, but it is not able to combine a
   succession of diffs into one.  Therefore, if "delta" was originally
   based upon "diffBase", or vice-versa, then the operation will
   succeed. *)

PROCEDURE IterateBranches(delta: T): Iterator;
(* Return an iterator over the branches of the given delta, in their
   order of appearance in the RCS file. *)

PROCEDURE IterateBranchesReversed(delta: T): Iterator;
(* Return an iterator over the branches of the given delta, in reverse
   order from their sequence in the RCS file. *)

PROCEDURE IterateTextPhrases(delta: T): RCSPhrases.Iterator;
(* Iterate over all the "newphrases" in the text section of the delta,
   in their order of appearance. *)

PROCEDURE IterateTreePhrases(delta: T): RCSPhrases.Iterator;
(* Iterate over all the "newphrases" in the tree section of the delta,
   in their order of appearance. *)

PROCEDURE GetPrev(delta: T): T;
(* Returns the previous delta from the given one, i.e., moves toward
   the head delta. *)

PROCEDURE Predecessor(delta: T): T;
(* Return the delta on which "delta" was based.  This moves toward
   lower revision numbers. *)

PROCEDURE ToText(delta : T) : TEXT;
(* Produce a readable representation for debugging purposes. *)

END RCSDelta.
