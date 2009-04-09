(* Copyright 1999-2003 John D. Polstra.
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
 * $Id: DevT.i3,v 1.1.1.1 2009-04-09 17:01:52 jkrell Exp $ *)

(* The "DevT" interface abstracts out some operations involving
   the Unix "dev_t" type.  This is intended to solve some portability
   problems. *)

INTERFACE DevT;

IMPORT
  OSError, TokScan, Utypes, Word;

TYPE
  T = Utypes.dev_t;

PROCEDURE Decode(t: TEXT): T RAISES {TokScan.Error};
(* Encode a "dev_t" into a text string. *)

PROCEDURE Encode(READONLY dev: T): TEXT;
(* Decode a text string into a "dev_t". *)

PROCEDURE Equal(READONLY a, b: T): BOOLEAN;
(* Test whether two "dev_t" values are equal. *)

PROCEDURE Hash(READONLY dev: T): Word.T;
(* Compute a hash value from a "dev_t". *)

PROCEDURE Mknod(path: TEXT;
                mode: Utypes.mode_t;
		READONLY dev: T) RAISES {OSError.E};
(* Execute a Unix "mknod" call with the given arguments. *)

END DevT.
