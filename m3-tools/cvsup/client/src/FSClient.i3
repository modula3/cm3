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
 * $Id: FSClient.i3,v 1.1.1.1 2009-04-09 17:01:37 jkrell Exp $ *)

INTERFACE FSClient;

IMPORT
  Detailer, IP, Logger, SupFileRec, SupFileRecSeq, SupMisc, Thread,
  TreeList, Updater;

TYPE
  T <: Public;
  Public = Thread.Closure OBJECT METHODS
    init(config: Configuration): T;
  END;

  Configuration = OBJECT
    localEndpoint                 := IP.NullEndPoint;
    port                          := SupMisc.Port;
    collections: SupFileRecSeq.T  := NIL;
    override: SupFileRec.T        := NIL;
    overrideMask                  := SupFileRec.Options{};
    connectMode                   := ConnectMode.Default;
    loDataPort                    := IP.NullPort;
    hiDataPort                    := IP.NullPort;
    destDir: TEXT                 := NIL;
    lockFile: TEXT                := NIL;
    deleteLimit: INTEGER          := -1;
    authRequired                  := FALSE;
    listerStats: TreeList.Stats   := NIL;
    detailerStats: Detailer.Stats := NIL;
    updaterStats: Updater.Stats   := NIL;
    trace: Logger.T               := NIL;
    listerTrace: Logger.T         := NIL;
    detailerTrace: Logger.T       := NIL;
    updaterTrace: Logger.T        := NIL;
  END;

  ConnectMode = { Default, Active, Passive, Socks, Mux };

(* An "FSClient.T" represents the entire CVSup client.  It is a subtype
   of "Thread.T", so it can be run as a separate thread if desired.

   The "init" method takes a "Configuration" object which specifies
   the details of the update(s) to be performed.

   In the "Configuration" object, "port" specifies the server port
   to which the client will attempt to connect.  "collections"
   describes the collections to be received.

   "connectMode" specifies the mode for establishing the connection.

   In active mode, the client does a listen, and the server connects
   to the client to establish the data connection.  "loDataPort"
   and "hiDataPort" can be used to specify a range of client ports.
   The listening socket will be bound to an available port in the
   specified range.  The default values let the operating system
   pick an arbitrary port.

   For use behind firewalls, a passive mode is also supported.  In
   passive mode, it is the server that listens for the secondary
   connection, and the client who initiates that connection.

   "destDir" is a directory under which all modified files will be put.
   By default, the original files are modified in place.  When "destDir"
   is specified, none of the original files are modified.  It is useful
   for testing and for dry runs.

   "lockFile" is a pathname that will be created and locked.  If it is
   already locked, the client will exit immediately with failure status.
   This can be used to prevent collisions between multiple runs spawned
   periodically by cron.

   "deleteLimit" is the maximum number of files that may be deleted in
   a single run.  An attempt to exceed the limit results in a fatal
   error.  The default is no limit.

   "listerStats", "detailerStats", and "updaterStats" can be used to
   pass statistics gathering objects to the client.  "trace",
   "listerTrace", "detailerTrace", and "updaterTrace" likewise can be
   used to pass message loggers. *)

END FSClient.
