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
 * $Id: RCSRevNum.i3,v 1.1.1.1 2009-04-09 17:01:58 jkrell Exp $ *)

INTERFACE RCSRevNum;

IMPORT Word;

CONST
  Brand = "RCSRevNum";

TYPE
  T = TEXT;

(* An "RCSRevNum.T" represents an RCS revision number, in text form. *)

PROCEDURE Cat(r1, r2: T): T;
(* Returns the concatenation of the two revision numbers. *)

PROCEDURE Compare(r1, r2: T): [-1..1];
(* The comparison is lexicographic. *)

PROCEDURE Equal(r1, r2: T): BOOLEAN;
(* Returns "Text.Equal(r1, r2)". *)

PROCEDURE Hash(r: T): Word.T;
(* Return a hash value suitable for use with a hash table. *)

PROCEDURE Last(r: T): T;
(* Returns the last component. *)

PROCEDURE NumParts(r: T): CARDINAL;
(* Returns the number of numeric parts in the revision number.  I.e.,
   returns the number of periods + 1. *)

PROCEDURE IsTrunk(r: T): BOOLEAN;
(* Returns the equivalent of "NumParts(r) = 2". *)

PROCEDURE Prefix(r: T): T;
(* Returns everything except the last component and its preceding dot. *)

PROCEDURE IsCVSBranch(r: T): BOOLEAN;
(* <=> n > 2 AND n MOD 2 = 1 OR 
       n > 2 AND n MOD 2 = 0 AND Last(Prefix(r)) = 0
       for n = NumParts(r) *)
END RCSRevNum.
