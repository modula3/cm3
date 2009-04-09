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
 * $Id: FileStatus.i3,v 1.1.1.1 2009-04-09 17:01:54 jkrell Exp $ *)

INTERFACE FileStatus;

IMPORT FileAttr, Pathname, RCSDate, RCSRevNum, Rd, Thread, Time, Wr;

EXCEPTION Error(TEXT);

TYPE
  T <: Public;

  Public = OBJECT
    type: Type;
    name: Pathname.T := NIL;
    tag: TEXT := NIL;
    date: RCSDate.T := NIL;
    revNum: RCSRevNum.T := NIL;
    revDate: RCSDate.T := NIL;
    clientAttr: FileAttr.T := NIL;
    serverAttr: FileAttr.T := NIL;
  END;

(* "clientAttr" contains the attributes of the client's file if there
   is one. "serverAttr" contains the attributes of the corresponding
   file on the server.  In CVS mode, these are identical.  But in
   checkout mode, "clientAttr" represents the checked-out file while
   "serverAttr" represents the corresponding RCS file on the server. *)

  Type = { DirDown, CheckoutLive, CheckoutDead, FileLive, FileDead, DirUp };

  Reader <: RdPublic;

  RdPublic = OBJECT METHODS
    version(): CARDINAL;
    scanTime(): Time.T;
    status(): CHAR;
    origin(): TEXT;
    timeDelta(): Time.T;
    get(): T RAISES {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
    prune(): T RAISES {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
    close() RAISES {Rd.Failure, Thread.Alerted};
  END;

  Writer <: WrPublic;

  WrPublic = OBJECT METHODS
    version(): CARDINAL;
    put(fs: T) RAISES {Error, Thread.Alerted, Wr.Failure};
    close() RAISES {Thread.Alerted, Wr.Failure};
  END;

PROCEDURE Compare(a, b: T): [-1..1];
(* Compares the names lexicographically, except that a "FileStatus.T"
   whose type is "Type.DirUp" is greater than anything of which it is
   a proper prefix. *)

PROCEDURE Equal(a, b: T): BOOLEAN;
(* Equivalent to "Compare(a, b) = 0". *)

PROCEDURE FromRd(rd: Rd.T): Reader
  RAISES {Error, Rd.Failure, Thread.Alerted};

PROCEDURE FromNull(): Reader;

PROCEDURE ToWr(wr: Wr.T; scanTime: Time.T): Writer
  RAISES {Thread.Alerted, Wr.Failure};

END FileStatus.
