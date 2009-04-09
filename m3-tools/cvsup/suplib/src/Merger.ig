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

(* "Merger" is a generic interface defining an iterator for merging two
   ordered sequences. *)

GENERIC INTERFACE Merger(Elem);
(* Where "Elem.T" is a reference type and "Elem" contains

| PROCEDURE Compare(a, b: Elem.T): [-1..1];

   "Compare" must be a total order.  It may be declared with a
   parameter mode of either "VALUE" or "READONLY", but not "VAR".
*)

TYPE
  T <: Public;

  Public = OBJECT METHODS
    next(VAR a, b: Elem.T): BOOLEAN RAISES ANY;

    compare(READONLY a, b: Elem.T): [-1..1];

    getA(): Elem.T RAISES ANY := NIL;
    getB(): Elem.T RAISES ANY := NIL;
  END;

(* A "Merger.T(Elem)", or merger, is an iterator which yields successive
   elements from a merge of two sorted input sequences.  The merger
   gets elements from the two input sequences by calling its methods
   "m.getA()" and "m.getB()".  These methods are undefined by
   default, and must be appropriately overridden through subtyping.
   Each method must return the next element of its input sequence,
   or "NIL" if the sequence is exhausted.  The elements so returned
   must be ordered as defined by "m.compare(a, b)".

   The default implementation of "m.compare(a, b)" calls
   "Elem.Compare(a, b)".  It may be overridden if desired.

   The elements of the merged sequence are obtained by calls to
   "m.next(a, b)".  This method returns "FALSE" when the merged
   sequence is exhausted; otherwise, it returns "TRUE".  The "VAR"
   parameters "a" and/or "b" are set to the next element of the merged
   sequence, depending on which input sequence it came from.  If the
   elements from the two input sequences are equal according to
   "m.compare(a, b)", then both "a" and "b" are set.  Otherwise,
   one of them is set to "NIL".

   The "RAISES" set for the "next" method is the union of the "RAISES"
   sets of "getA" and "getB".

*)

END Merger.
