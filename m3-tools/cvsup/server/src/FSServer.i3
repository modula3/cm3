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
 * $Id: FSServer.i3,v 1.1.1.1 2009-04-09 17:01:48 jkrell Exp $ *)

INTERFACE FSServer;

IMPORT IP, Logger, Pathname, Thread;

EXCEPTION Error(TEXT);

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(config: Configuration): T
      RAISES {Error};
    run() RAISES {Error, Thread.Alerted};
  END;

  Configuration = OBJECT
    localEndpoint             := IP.NullEndPoint;
    serverBase: Pathname.T    := NIL;
    serverCollDirs: TEXT      := NIL;
    serverScanDir: Pathname.T := NIL;
    loDataPort                := IP.NullPort;
    hiDataPort                := IP.NullPort;
    logger: Logger.T          := NIL;
    maxChildren               := -1;
    compLevel: [-1..9]        := -1;
    detailAllRCSFiles         := FALSE;
  END;

(* An "FSServer.T" represents the CVSup server, as a whole.  It receives
   connections on the given "localEndpoint", and forks off subthreads as
   required to service them.

   "serverBase" is the pathname of the base directory for the files
   that describe the collections to be served.  If unspecified, a
   suitable default is used.

   "serverCollDirs" is a colon-separated list of directories where
   the collection information can be found.  Non-absolute paths are
   interpreted relative to "serverBase".  If unspecified, a suitable
   default is used.

   "serverScanDir" is a directory under which the scan files are to be
   found.  If unspecified, then no scan files are used and the server
   does a full tree walk for each collection served.

   "loDataPort" and "hiDataPort" specify the range of TCP ports that the
   server will use to establish data connections in passive mode.

   If "logger" is NIL, no logging will be done.

   "maxChildren" governs the number of simultaneous connections that will
   be served.  Friends listed in the configuration file are exempt from
   this limit.  If "maxChildren" is less than 0, the server will serve
   one client without forking, and then exit.

   "compLevel" specifies the compression level to be used.  A value
   of 0 gives no compression, while 9 gives the maximum.  The
   default of -1 gives a reasonable default compression level. *)

END FSServer.
