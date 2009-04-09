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
 * $Id: TreeComp.i3,v 1.1.1.1 2009-04-09 17:01:51 jkrell Exp $ *)

INTERFACE TreeComp;

IMPORT ClientClass, CVProto, Logger, Reaper, StreamRd, StreamWr, 
       SupFileRecSeq, Thread;


TYPE
  T <: Public;

  Public = Thread.Closure OBJECT
    METHODS
      init(proto: CVProto.T;
           rd: StreamRd.T;
           wr: StreamWr.T;
           collections: SupFileRecSeq.T;
           clientClass: ClientClass.T;
           compLevel: [-1..9] := -1;
           reaper: Reaper.T := NIL;
	   logger: Logger.T := NIL): T;
    END;

(* A "TreeComp.T", or tree differ, represents the thread that compares
   the client and server file trees, and determines which files are
   missing, extra, or possibly out-of-date on the client.

   "proto" describes the version of the protocol being used.

   "rd" is the input stream from the client's Tree Lister.

   "wr" is the output stream to the client's Detailer.

   "collections" is a sequence of supfile records, specifying which
   collections and releases are to be transferred.

   "compLevel" is the compression level to be used, for collections that
   have compression enabled.

   If a "reaper" is given, it will be notified just before the thread
   terminates. *)

VAR traceLevel := 0;

END TreeComp.
