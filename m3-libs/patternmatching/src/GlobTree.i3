(* Copyright 1996-1998 John D. Polstra.
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
 * $Id: GlobTree.i3,v 1.1 2001-12-01 16:07:31 wagner Exp $ *)

(* The "GlobTree" interface allows one to construct arbitrarily complex
   boolean expressions for evaluating whether to accept or reject a
   filename. *)

INTERFACE GlobTree;

IMPORT Glob, RegEx;

TYPE
  T = OBJECT METHODS
    test(filename: TEXT): BOOLEAN RAISES {RegEx.Error};
  END;

VAR
  False, True: T;  (* CONST *)

(* A "GlobTree.T" represents a boolean expression for filtering
   filenames.  Its "test" method may be called with a filename to be
   tested.  It returns "TRUE" or "FALSE" according to whether the name
   is accepted or rejected by the expression.

   Expressions are trees constructed from nodes representing either
   primitive matching operations (primaries) or operators that are
   applied to their subexpressions.  The simplest primitives are
   "False", which matches nothing, and "True", which matches everything.

   A more useful primitive is the matching operation.  It is
   constructed with a glob-style pattern.  Its "test" method returns
   an indication of whether the given filename matches the pattern.

   Expressions can be combined with the boolean operators AND, OR, and
   NOT, to form more complex expressions. *)

PROCEDURE Match(pattern: TEXT; options := Glob.MatchOptions{}): T;
(* Return a "match" primitive for the given pattern.  Matching is
   performed by "Glob.Match", using the specified options. *)

PROCEDURE And(left, right: T): T;
(* Return an AND expression. *)

PROCEDURE Or(left, right: T): T;
(* Return an OR expression. *)

PROCEDURE Not(child: T): T;
(* Return an expression that evaluates the logical negation of its
   subexpression. *)

END GlobTree.
