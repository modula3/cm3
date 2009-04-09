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
 * $Id: FileUpdater.i3,v 1.1.1.1 2009-04-09 17:01:37 jkrell Exp $ *)

INTERFACE FileUpdater;

IMPORT
  CVProto, FileStatus, Logger, Pathname, Rd, SupFileRec, Thread,
  TokScan, Wr;

EXCEPTION
  Error(TEXT);
  FixupNeeded(TEXT);

TYPE
  T <: Public;

  Public = OBJECT METHODS
    update(sfr: SupFileRec.T;
	   name: Pathname.T;
	   toAttic: BOOLEAN;
	   proto: CVProto.T;
	   trace: Logger.T;
	   protoRd: Rd.T;
           wr: Wr.T;
	   VAR (*OUT*) status: Status)
      RAISES {Error, FixupNeeded, Rd.EndOfFile, Rd.Failure, Thread.Alerted,
	      TokScan.Error, Wr.Failure};
    isRCS(): BOOLEAN;
  END;

(* The base type defaults isRCS() to a method that returns FALSE.  Subtypes
   may override this. *)

  Status = RECORD
    updateType: UpdateType;
    fs: FileStatus.T;
    fromAttic: BOOLEAN;
    modified: BOOLEAN;
    wantSum: TEXT := "*";  (* Reference MD5 checksum from the server. *)
  END;

(* The default value for "wantSum" never matches.  If an individual
   subtype wants to disable checksum comparisons, it should set
   "wantSum" to "NIL". *)

  UpdateType = {
    Create,		(* Create a new file *)
    Checkout,		(* Check a file out from RCS *)
    Delete,		(* Delete an existing file *)
    Replace,		(* Replace an existing file *)
    Append,		(* Append to an existing file *)
    Touch,              (* Update an existing file's modification time *)
    Edit,		(* Edit an RCS file or a checked-out file *)
    Rsync,              (* Edit a regular file with the rsync algorithm *)
    Fixup,		(* Replace an RCS file that couldn't be edited *)
    Other		(* Everything else. *)
  };

END FileUpdater.
