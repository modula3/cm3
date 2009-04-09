(* Copyright 1997-2003 John D. Polstra.
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

(* The "MD5Wr" interface provides a subclass of "Wr.T" which computes
   the MD5 signature of everything it writes. *)

INTERFACE MD5Wr;

IMPORT MD5, Wr;

TYPE
  T <: Public;

  Public = Wr.T OBJECT METHODS
    init(wr: Wr.T;
	 md5: MD5.T := NIL;
	 closeChild: BOOLEAN := TRUE): T;
    getSignature(): TEXT;
  END;

(* If "closeChild" is "TRUE" (the default), then closing the escaped
   writer will also cause the child writer to be closed.  If it is "FALSE",
   then the child writer will be left open.

   By default, the signature is computed over exactly the bytes
   that have been written to the MD5 writer.  If the "md5" parameter
   is specified, then it can be used to accumulate bytes onto an
   existing partially calculated signature.

   The "getSignature" method may only be called after the "MD5Wr.T" has
   been closed.  It returns the text form of the signature. *)

END MD5Wr.
