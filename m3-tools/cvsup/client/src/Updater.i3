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

INTERFACE Updater;

IMPORT
  CVProto, FileUpdater, Logger, Reaper, StreamRd, SupFileRecSeq,
  SyncFixupQueue, Thread;

TYPE
  T <: Public;

  Public = Thread.Closure OBJECT
  METHODS
    init(proto: CVProto.T;
         rd: StreamRd.T;
         collections: SupFileRecSeq.T;
         fixups: SyncFixupQueue.T;
	 deleteLimit := -1;
         reaper: Reaper.T := NIL;
         destDir: TEXT := NIL;
         stats: Stats := NIL;
         trace: Logger.T := NIL): T;
  END;

  Stats <: StatsPublic;

  StatsPublic = MUTEX OBJECT
    updateInfo: ARRAY FileUpdater.UpdateType OF UpdateInfo;
    totals: UpdateInfo;
  METHODS
    init(): Stats;
    start();
    update(type: FileUpdater.UpdateType);
    finish();
  END;

  UpdateInfo = RECORD
    fileCount: CARDINAL := 0;	(* Number of files updated *)
    fileBytes := 0.0d0;		(* Final size of updated file *)
    wireBytes := 0.0d0;		(* Bytes from wire before decompression *)
    commBytes := 0.0d0;		(* Bytes in after decompression *)
  END;

(* An "Updater.T" is the thread that receives edit requests from the
   server's File Differ, and applies the edits to the client's files.

   "proto" describes the version of the protocol being used.

   "rd" is the input stream from the File Differ.

   If any RCS file updates fail to produce the proper checksums, Fixup
   records will be communicated back to the Detailer via the "fixups"
   queue.

   If a "deleteLimit" is specified, the updater will exit with a fatal
   error if an attempt is made to delete more than that number of files.
   The default is no limit.

   If a "reaper" is given, it will be notified just before the thread
   terminates.

   "destDir" is an optional directory where updated files will be
   placed.  The default is to update files in their original locations.

   If "stats" is specified, the updater will maintain various
   statistics in the given object.  It will call the "start" method
   initially, then the "update" method after each update of the
   statistics, and finally, the "finish" method just before terminating. 

   Debugging information is written to the "trace" stream, if specified. *)

END Updater.
