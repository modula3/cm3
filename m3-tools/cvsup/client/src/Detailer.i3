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

INTERFACE Detailer;

IMPORT
  CVProto, Logger, Reaper, StreamRd, StreamWr, SupFileRecSeq,
  SyncFixupQueue, Thread;

TYPE
  T <: Public;

  Public = Thread.Closure OBJECT
  METHODS
    init(proto: CVProto.T;
         rd: StreamRd.T;
         wr: StreamWr.T;
         collections: SupFileRecSeq.T;
         fixups: SyncFixupQueue.T;
         compLevel: [-1..9] := -1;
         reaper: Reaper.T := NIL;
         stats: Stats := NIL;
         logger: Logger.T := NIL): T;
  END;

  Stats <: StatsPublic;

  StatsPublic = MUTEX OBJECT
    numRequests: CARDINAL := 0;
    bytesIn, bytesOut := 0.0d0;
    wireBytesIn, wireBytesOut := 0.0d0;
  METHODS
    init(): Stats;
    start();
    update();
    finish();
  END;

(* A "Detailer.T" is the thread that examines files which may need updating,
   and sends relevant details to the server.

   The "init" method takes many arguments, although most of them
   have reasonable default values.  The only mandatory arguments
   are "proto", the protocol descriptor, "rd", the input stream
   from the server's Tree Differ, "wr", the output stream to the
   server's File Differ, and "fixups", the queue of fixup requests
   from the Updater, for repairing failed edits of RCS files.

  "compLevel" specifies the compression level to use.

   If a "reaper" is specified, the detailer will notify it just before it
   terminates.

   If "stats" is specified, the detailer will maintain various
   statistics in the given object.  It will call the "start" method
   initially, then the "update" method after each update of the
   statistics, and finally, the "finish" method just before terminating.

   Messages are sent to the "logger", if one is specified. *)

END Detailer.
