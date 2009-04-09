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

(* The "MD5" interface provides facilities for calculating MD5 checksums. *)

INTERFACE MD5;

IMPORT MD5Digest, OSError, Pathname;

TYPE
  T = OBJECT METHODS
    clone(): T;
    update(READONLY data: ARRAY OF CHAR);
    updateRaw(data: UNTRACED REF CHAR; count: CARDINAL);
    updateText(text: TEXT);
    finish(): TEXT;
    finishRaw(VAR rslt: MD5Digest.T);
  END;

(* An "MD5.T" calculates the MD5 checksum of some data.  After it is
   initialized by the "init" method, it may be supplied with data
   in arbitrary chunks, via calls to "update" and/or "updateText".
   After all data has been processed, either "finish" or "finishRaw"
   should be called to retrieve the checksum and free all the
   resources associated with the "MD5.T".  The former returns the
   checksum in text form, as a 32-character string.  The latter
   returns the checksum in raw (binary) form.

   The "clone" method returns a new "MD5.T" with an exact copy of the
   state of the existing one. *)

PROCEDURE New(): T;
(* Creates and initializes a checksum generator, and returns it. *)

PROCEDURE NewRCS(): T;
(* Creates and initializes a special checksum generator for RCS files.
   It canonicalizes the checksum in such a way as to disregard
   irrelevant differences in white space. *)

PROCEDURE FileSignature(name: Pathname.T): TEXT
  RAISES {OSError.E};
(* Return the MD5 signature, in text form, of an entire file.  WARNING: The
   MD5 library allocates a buffer of BUFSIZ bytes (from <stdio.h>) on the
   stack, to perform this function.  Take care that the calling thread's
   stack has enough space to allow this. *)

END MD5.
