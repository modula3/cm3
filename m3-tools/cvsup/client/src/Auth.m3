(* Copyright 1999-2003 John D. Polstra.
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
 * $Id: Auth.m3,v 1.1.1.1 2009-04-09 17:01:33 jkrell Exp $ *)

MODULE Auth;

IMPORT ErrMsg, FileRd, Fmt, OSError, Pathname, Rd, Text, Thread, TokScan;

REVEAL
  DB = BRANDED OBJECT
    path: Pathname.T;
    rd: Rd.T;
    lineNum := 0;
  END;

PROCEDURE Close(db: DB)
  RAISES {Error, Thread.Alerted} =
  BEGIN
    TRY
      Rd.Close(db.rd);
    EXCEPT Rd.Failure(l) =>
      RAISE Error("Cannot close \"" & db.path & "\": " & ErrMsg.StrError(l));
    END;
  END Close;

PROCEDURE Err(db: DB; msg: TEXT)
  RAISES {Error} =
  BEGIN
    RAISE Error(db.path & ":" & Fmt.Int(db.lineNum) & ": " & msg);
  END Err;

PROCEDURE GetRec(db: DB): TokScan.T
  RAISES {Error, Rd.EndOfFile, Thread.Alerted} =
  VAR
    line: TEXT;
  BEGIN
    TRY
      REPEAT
	INC(db.lineNum);
	line := TokScan.Trim(Rd.GetLine(db.rd));
      UNTIL Text.Length(line) > 0 AND Text.GetChar(line, 0) # '#';
    EXCEPT Rd.Failure(l) =>
      Err(db, "read error: " & ErrMsg.StrError(l));
    END;
    RETURN TokScan.New(line, SET OF CHAR{':'}, emptyTokens := TRUE);
  END GetRec;

PROCEDURE Lookup(db: DB;
                 server: TEXT;
		 VAR (*OUT*) auth: T): BOOLEAN
  RAISES {Error, Thread.Alerted} =
  VAR
    startingPoint: CARDINAL;
    ts: TokScan.T;
    found: BOOLEAN;
    ent: T;
  BEGIN
    TRY
      startingPoint := Rd.Index(db.rd);
      REPEAT
	TRY
	  ts := GetRec(db);
	  ent.server := ts.getToken("server name");
	  ent.client := ts.getToken("client name");
	  ent.password := ts.getToken("password");
	  ent.comment := ts.getToken("comment");
	  found := TokScan.EqualFolded(ent.server, server);
	EXCEPT Rd.EndOfFile =>
	  Rd.Seek(db.rd, 0);
	  db.lineNum := 0;
	END;
      UNTIL found OR Rd.Index(db.rd) = startingPoint;
      IF NOT found THEN RETURN FALSE END;
      auth := ent;
    EXCEPT
    | Rd.Failure(l) =>
	RAISE Error("Cannot rewind \"" & db.path & "\": " &
	  ErrMsg.StrError(l));
    | TokScan.Error(msg) =>
	Err(db, msg);
    END;
    RETURN TRUE;
  END Lookup;

PROCEDURE Open(path: Pathname.T): DB
  RAISES {Error} =
  BEGIN
    TRY
      RETURN NEW(DB, path := path, rd := FileRd.Open(path));
    EXCEPT OSError.E(l) =>
      RAISE Error("Cannot open \"" & path & "\": " & ErrMsg.StrError(l));
    END;
  END Open;

BEGIN
END Auth.
