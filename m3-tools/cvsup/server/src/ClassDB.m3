(* Copyright 2000-2001 Olaf Wagner.
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
 *      This product includes software developed by Olaf Wagner.
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
 * $Id: ClassDB.m3,v 1.1.1.1 2009-04-09 17:01:46 jkrell Exp $ *)

MODULE ClassDB;

IMPORT
  ClientClass, ErrMsg, FileAttr, Logger, OSError, OSErrorPosix,
  Pathname, Rd, Text, Thread, Time, Uerror;

PROCEDURE Init(fn: Pathname.T; logger: Logger.T := NIL)
  RAISES {Rd.Failure, Thread.Alerted} =
  BEGIN
    LOCK mu DO
      RealInit(fn, logger);
    END;
  END Init;

PROCEDURE RealInit(fn: Pathname.T; logger: Logger.T := NIL)
  RAISES {Rd.Failure, Thread.Alerted} =
  BEGIN
    IF db = NIL THEN
      db := NEW(ClientClass.DB);
      dbPath := fn;
      dbLogger := logger;
      dbModTime := GetModTime(fn, logger);
      dbRefreshTime := Time.Now();
    END;
    EVAL db.init(fn);
  END RealInit;

PROCEDURE GetClass(name: TEXT) : ClientClass.T =
  BEGIN
    LOCK mu DO
      RETURN db.getClass(name);
    END;
  END GetClass;

PROCEDURE Get(path: Pathname.T;
              maxAge: Time.T;
              logger: Logger.T := NIL): ClientClass.DB
  RAISES {Rd.Failure, Thread.Alerted} =
  VAR
    now := Time.Now();
    fileTime := GetModTime(path, logger);
  BEGIN
    LOCK mu DO
      IF fileTime < 0.0d0 THEN
        RETURN ClientClass.FreeAccessDB();
      END;
      IF Text.Equal(path, dbPath) AND
        dbModTime = fileTime AND
        dbRefreshTime >= now - maxAge THEN
        RETURN db;
      END;
      db := NIL;
      RealInit(path, logger);
      RETURN db;
    END;
  END Get;

PROCEDURE GetModTime(fn: Pathname.T; logger: Logger.T) : Time.T =
(* Returns the modification time of "fn", or -1.0d0 if that information
   cannot be gotten.  In the latter case, a warning is logged unless the
   file simply doesn't exist. *)
  VAR
    attr: FileAttr.T;
    fileTime: Time.T;
  BEGIN
    TRY
      attr := FileAttr.FromPathname(fn, follow := TRUE);
      fileTime := FileAttr.GetModTime(attr);
    EXCEPT OSError.E(list) =>
      IF list.head # EnoentAtom THEN
        Log(logger, Logger.Priority.Warning,
          "Cannot get attributes for \"" & fn & "\": " &
          ErrMsg.StrError(list));
      END;
      fileTime := -1.0d0;
    END;
    RETURN fileTime;
  END GetModTime;

PROCEDURE Log(logger: Logger.T;
              priority: Logger.Priority;
              msg: TEXT) =
  BEGIN
    IF logger # NIL THEN
      Logger.Put(logger, priority, msg);
    END;
  END Log;

VAR (* CONST *)
  EnoentAtom := OSErrorPosix.ErrnoAtom(Uerror.ENOENT);
VAR
  mu:= NEW(MUTEX);
  db: ClientClass.DB;
  dbPath: Pathname.T;
  dbLogger: Logger.T;
  dbModTime: Time.T;
  dbRefreshTime: Time.T;
BEGIN
  db := NIL;
  dbPath := "";
  dbLogger := NIL;
  dbModTime := -1.0d0;
  dbRefreshTime := -1.0d0;
END ClassDB.
