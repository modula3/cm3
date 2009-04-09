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

(* The "GzipWr" interface defines a subclass of "StreamWr.T" which writes to
   a compressed stream, compressing the data written in the process. *)

INTERFACE GzipWr;

IMPORT OSError, StreamWr, Wr;

TYPE
  T <: Public;

  Public = StreamWr.T OBJECT METHODS
    init(wr: Wr.T;
	 level: CompLevel := -1;
	 closeChild: BOOLEAN := TRUE): T RAISES {OSError.E};
  END;

  CompLevel = [-1 .. 9];  (* default, none, fastest .. best *)

(* The "init" method is supplied with a so-called child writer, which
   must write bytes directly to the compressed stream.  The optional
   parameter "level" specifies the desired tradoff between compression
   efficiency and speed.

   If "closeChild" is "TRUE" (the default), then closing the Gzip writer
   will also cause the child writer to be closed.  If it is "FALSE", then
   the child writer will be left open. *)

PROCEDURE Cleanup(wr: T);
(* Frees all resources used by "wr", without doing anything to the child. *)

(* "Cleanup" is a no-op if the resources have already been freed, e.g.,
   by a call to "Wr.Close". *)

END GzipWr.
