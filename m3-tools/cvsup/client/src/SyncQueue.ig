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

GENERIC INTERFACE SyncQueue(Elem);
(* Where "Elem.T" is a type that is not an open array type, and "Elem"
   contains

| CONST Brand = <text-constant>;

*)

IMPORT Thread;

EXCEPTION EndOfFile;

TYPE
  T <: Public;

  Public = Private OBJECT METHODS
    init(sizeHint: CARDINAL := 5): T;

    put(READONLY e: Elem.T);
    close();

    get(): Elem.T RAISES {EndOfFile, Thread.Alerted};
    EOF(): BOOLEAN RAISES {Thread.Alerted};
    size(): CARDINAL;
  END;

  Private <: ROOT;

(* A SyncQueue.T, or synchronized queue, is a unidirectional pipeline
   that can be used for inter-thread communication.

   The "put" method appends a new element to the end of the queue.  The
   queue is allowed to grow without bound, so the "put" operation never
   blocks the calling thread.

   The "get" method removes and returns the element at the head of the
   queue.  If the queue is empty, "get" blocks the calling thread, until
   data is again available to be gotten.

   Synchronized queues support the notion of an end-of-file marker.  On
   the writing side, the "close" method (conceptually) appends an EOF
   mark to the end of the queue.  It is a checked runtime error to call
   "put" after "close" has been called.  If a "get" operation encounters
   the EOF marker, it raises the "EndOfFile" exception.  The presence of
   the end-of-file may also be tested explicitly, by the "EOF" method.
   Note that, if the queue is empty but it has not been closed, the "EOF"
   method will block the calling thread.

   The "size" method can be used to implement non-blocking gets.  It
   returns the number of elements that could be gotten without blocking.
   For the purposes of "size", the end-of-file marker counts as one
   element. *)

END SyncQueue.
