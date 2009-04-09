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
 * $Id: RCSString.i3,v 1.1.1.1 2009-04-09 17:01:58 jkrell Exp $ *)

(* The "RCSString" interface provides an abstract interface to the text
   strings associated with RCS files.  Behind the abstract interface are
   hidden a number of different implementations, each appropriate for a
   different situation.  The goal of this approach is efficiency.

   For example, most strings that were present in the original RCS file
   are implemented as pointers into the memory-mapped image of the file
   itself.  Thus, those strings can contain huge numbers of characters
   without consuming any memory resources. *)

INTERFACE RCSString;

CONST Brand = "RCSString";

TYPE
  T = OBJECT METHODS
    toText(): TEXT := NIL;
    numLines(): CARDINAL := NIL;
    iterate(): Iterator := NIL;
  END;

  Iterator = OBJECT METHODS
    next(VAR line: T): BOOLEAN := NIL;
  END;

(* The "toText" method returns a "TEXT" representation of the entire
   string.  It should be avoided for very large strings.  Instead, the
   "iterate" method should be used to access large strings one line at a
   time.

   The "numLines" method returns the number of lines in the string.  If
   the last line does not have a terminating newline character, it still
   counts as a line.

   The "iterate" method constructs an iterator that accesses the
   individual lines of the string in sequence.  Each call to the
   "Iterator"'s "next" method sets the "VAR" parameter to the next
   line of the string, and returns "TRUE".  If there are no more
   lines, "next" returns "FALSE".

   Each returned line includes its terminating newline, if any.  The
   last line won't necessarily end with a newline. *)

PROCEDURE FromText(text: TEXT): T;
(* Construct an "RCSString.T" from the given "TEXT". *)

END RCSString.
