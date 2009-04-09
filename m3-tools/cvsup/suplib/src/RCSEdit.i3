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
 * $Id$ *)

INTERFACE RCSEdit;

IMPORT RCSString;

CONST
  Brand = "RCSEdit";

TYPE
  T = REF RECORD
    offset: INTEGER;  (* Line number offset: input minus output. *)
    text: REF ARRAY OF RCSString.T;  (* Added lines of text, or NIL. *)
  END;

(* An "RCSEdit.T" represents a single edit of a file.  It consists of a
   deletion, addition, or replacement (deletion plus addition) of one or
   more consecutive lines of text.  Within an RCS file, each delta
   except the head delta is represented by a sequence of such edits.

   An edit is applied to an input text, and produces an output text.
   The "offset" member is the difference between the input file's line
   number and the output file's line number at the point where the edit
   is applied.  It represents the accumulated effects of the additions
   and deletions effected by the prior edits.  The "text" member, if
   non-"NIL", contains one or more lines of text to be added at the
   point of the edit.  It is "NIL" for a plain deletion.  *)

END RCSEdit.
