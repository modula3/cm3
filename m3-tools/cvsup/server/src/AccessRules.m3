(* Copyright 1998-2003 John D. Polstra.
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
 * $Id: AccessRules.m3,v 1.1.1.1 2009-04-09 17:01:46 jkrell Exp $ *)

MODULE AccessRules;

IMPORT
  ErrMsg, File, FileRd, Fmt, FS, IP, Logger, OSError, OSErrorPosix,
  Pathname, Rd, RefList, SupMisc, Text, Thread, Time, Uerror,
  UnixMisc, Word;

REVEAL
  T = BRANDED OBJECT
    path: Pathname.T := "";
    logger: Logger.T := NIL;
    fileTime: Time.T := 0.0d0;
    refreshTime: Time.T := 0.0d0;
    rules: RefList.T := NIL;	(* List of Rule. *)
  END;

TYPE
  RuleType = { Permit, PermitWithAuth, Deny };

  Rule = OBJECT
    addr: IP.Address;
    matchBits: CARDINAL;
    countBits: CARDINAL;
    type: RuleType;
    limit: CARDINAL;
  END;

CONST
  Digits = SET OF CHAR{'0'..'9'};

VAR (* CONST *)
  EnoentAtom := OSErrorPosix.ErrnoAtom(Uerror.ENOENT);

VAR
  theT := NEW(T);

EXCEPTION Error(TEXT);

PROCEDURE Check(rules: T;
                addr: IP.Address;
		READONLY clients: ARRAY OF IP.Address): CheckResult =
  VAR
    cur: RefList.T;
    rule: Rule;
    count: CARDINAL;
  BEGIN
    cur := rules.rules;
    WHILE cur # NIL DO
      rule := cur.head;
      IF Match(addr, rule.addr, rule.matchBits) THEN
	count := 0;
	FOR i := FIRST(clients) TO LAST(clients) DO
	  IF clients[i] # IP.NullAddress
	  AND Match(addr, clients[i], rule.countBits) THEN
	    INC(count);
	  END;
	END;
	IF count > rule.limit THEN  (* Failed the rule. *)
	  IF rule.type = RuleType.Deny THEN
	    IF rule.limit = 0 THEN
	      RETURN CheckResult.Denied;
	    ELSE
	      RETURN CheckResult.TooMany;
	    END;
	  END;
	ELSE  (* Passed the rule. *)
	  IF rule.type = RuleType.Permit THEN
	    RETURN CheckResult.OK;
	  ELSIF rule.type = RuleType.PermitWithAuth THEN
	    RETURN CheckResult.AuthRequired;
	  END;
	END;
      END;
      cur := cur.tail;
    END;
    (* If we fall off the end, we accept.  This is used only when there
       is no readable access file.  If the access file exists and is
       readable, we automatically append a universal deny rule to the
       end. *)
    RETURN CheckResult.OK;
  END Check;

PROCEDURE Get(path: Pathname.T;
              maxAge: Time.T;
	      logger: Logger.T := NIL): T
  RAISES {Rd.Failure, Thread.Alerted} =
  CONST
    HostStop = SET OF CHAR{' ', '\t', '/', '\n'};
    WS = SET OF CHAR{' ', '\t'};
  VAR
    now := Time.Now();
    file: File.T;
    rd: Rd.T;
    fileTime: Time.T;
    line: TEXT;
    lineNum: CARDINAL;
    len: CARDINAL;
    scanPos: CARDINAL;
    chPos: CARDINAL;
    start: CARDINAL;
    ch: CHAR;
    type: RuleType;
    host: TEXT;
    matchBits: CARDINAL;
    countBits: CARDINAL;
    limit: CARDINAL;
    lastRule: RefList.T;
  PROCEDURE AddRule(addr: IP.Address;
		    matchBits: CARDINAL;
		    countBits: CARDINAL;
		    type: RuleType;
		    limit: CARDINAL) =
    VAR
      rule := NEW(Rule,
	addr := addr,
	matchBits := matchBits,
	countBits := countBits,
	type := type,
	limit := limit);
      elem := RefList.List1(rule);
    BEGIN
      IF lastRule = NIL THEN
	theT.rules := elem;
      ELSE
	lastRule.tail := elem;
      END;
      lastRule := elem;
    END AddRule;
  PROCEDURE NextCh() =
    BEGIN
      chPos := scanPos;
      IF scanPos < len THEN
	ch := Text.GetChar(line, chPos);
	INC(scanPos);
      ELSE
	ch := '\n';
      END;
    END NextCh;
  PROCEDURE SkipWS() =
    BEGIN
      WHILE ch IN WS DO
	NextCh();
      END;
    END SkipWS;
  BEGIN
    (* We go ahead and open the file even though we might not need to
       parse it.  That way we avoid races between a stat() and the
       subsequent open. *)
    TRY
      file := FS.OpenFileReadonly(path);
      rd := NEW(FileRd.T).init(file);
    EXCEPT OSError.E(list) =>
      IF list.head # EnoentAtom THEN
	Log(logger, Logger.Priority.Warning,
	  "Cannot open \"" & path & "\": " & ErrMsg.StrError(list));
      END;
      theT.path := "";
      theT.logger := logger;
      theT.fileTime := 0.0d0;
      theT.refreshTime := 0.0d0;
      theT.rules := NIL;
      RETURN theT;
    END;
    TRY
      TRY
	fileTime := file.status().modificationTime;
      EXCEPT OSError.E(list) =>  (* No way! *)
	Log(logger, Logger.Priority.Warning,
	  "fstat failed on \"" & path & "\": " & ErrMsg.StrError(list));
	fileTime := 0.0d0;
      END;

      (* No need to reparse the file (with potential DNS delays) if what
	 we have already is up to date. *)
      IF Text.Equal(path, theT.path)
      AND theT.fileTime = fileTime
      AND theT.refreshTime >= now - maxAge THEN
	RETURN theT;
      END;

      theT.path := path;
      theT.logger := logger;
      theT.fileTime := fileTime;
      theT.refreshTime := now;
      theT.rules := NIL;
      lastRule := NIL;

      lineNum := 0;
      LOOP
	TRY line := Rd.GetLine(rd) EXCEPT Rd.EndOfFile => EXIT END;
	INC(lineNum);
	WITH commentStart = Text.FindChar(line, '#') DO
	  IF commentStart >= 0 THEN
	    line := Text.Sub(line, 0, commentStart);
	  END;
	END;
	len := Text.Length(line);
	scanPos := 0;
	NextCh();
	SkipWS();
	IF ch # '\n' THEN  (* Line has something on it. *)
	  TRY
	    (* Scan the rule type. *)
	    CASE ch OF
	    | '+' => type := RuleType.Permit;  limit := LAST(CARDINAL);
	    | '*' => type := RuleType.PermitWithAuth;  limit := LAST(CARDINAL);
	    | '-' => type := RuleType.Deny;  limit := 0;
	    ELSE
	      RAISE Error("\"+\" or \"-\" expected");
	    END;
	    NextCh();
	    SkipWS();

	    (* Scan the host name or IP address. *)
	    start := chPos;
	    WHILE NOT ch IN HostStop DO NextCh() END;
	    IF chPos = start THEN
	      RAISE Error("Host name or IP address expected");
	    END;
	    host := Text.Sub(line, start, chPos - start);
	    SkipWS();

	    (* Scan the match bits, if any.  The default is 32. *)
	    matchBits := 32;
	    IF ch = '/' THEN  (* Match bits *)
	      NextCh();
	      SkipWS();
	      IF NOT ch IN Digits THEN
		RAISE Error("Invalid matchbits");
	      END;
	      matchBits := 0;
	      REPEAT
		matchBits := 10*matchBits + ORD(ch) - ORD('0');
		NextCh();
	      UNTIL NOT ch IN Digits;
	      IF matchBits > 32 THEN
		RAISE Error("Matchbits (" & Fmt.Int(matchBits)
		  & ") out of range");
	      END;
	      SkipWS();
	    END;

	    (* Scan the count bits, if any.  The default is the match bits. *)
	    countBits := matchBits;
	    IF ch = '/' THEN  (* Count bits *)
	      NextCh();
	      SkipWS();
	      IF NOT ch IN Digits THEN
		RAISE Error("Invalid countbits");
	      END;
	      countBits := 0;
	      REPEAT
		countBits := 10*countBits + ORD(ch) - ORD('0');
		NextCh();
	      UNTIL NOT ch IN Digits;
	      IF countBits > 32 THEN
		RAISE Error("Countbits (" & Fmt.Int(countBits)
		  & ") out of range");
	      END;
	      SkipWS();
	    END;

	    (* Scan the connection limit, if any.  The default was set
	       above, based on the rule type. *)
	    IF ch # '\n' THEN
	      IF NOT ch IN Digits THEN
		RAISE Error("Invalid connection limit");
	      END;
	      limit := 0;
	      REPEAT
		limit := 10*limit + ORD(ch) - ORD('0');
		NextCh();
	      UNTIL NOT ch IN Digits;
	      SkipWS();
	    END;

	    IF ch # '\n' THEN RAISE Error("End of line expected") END;

	    TRY
	      AddRule(SupMisc.ParseIPAddress(host, netOK := TRUE),
		matchBits, countBits, type, limit);
	    EXCEPT SupMisc.BadAddress =>  (* Try it as a host name. *)
	      WITH addrs = UnixMisc.GetHostAddrs(host) DO
		IF addrs = NIL OR NUMBER(addrs^) = 0 THEN
		  RAISE Error("Cannot resolve host name \"" & host & "\"");
		END;
		FOR i := FIRST(addrs^) TO LAST(addrs^) DO
		  AddRule(addrs[i], matchBits, countBits, type, limit);
		END;
	      END;
	    END;
	  EXCEPT Error(msg) =>
	    Log(logger, Logger.Priority.Warning,
	      path & ":" & Fmt.Int(lineNum) & ": " & msg);
	  END;
	END;
      END;

      (* Append a universal PermitWithAuth rule at the end. *)
      AddRule(IP.NullAddress, 0, 0, RuleType.PermitWithAuth, LAST(CARDINAL));
    FINALLY
      Rd.Close(rd);
    END;

    RETURN theT;
  END Get;

PROCEDURE Log(logger: Logger.T;
              priority: Logger.Priority;
	      msg: TEXT) =
  BEGIN
    IF logger # NIL THEN
      Logger.Put(logger, priority, msg);
    END;
  END Log;

PROCEDURE Match(addr1, addr2: IP.Address; maskBits: CARDINAL): BOOLEAN =
  CONST
    Masks = ARRAY [1..7] OF INTEGER{
      16_80, 16_c0, 16_e0, 16_f0, 16_f8, 16_fc, 16_fe
    };
  VAR
    i := 0;
  BEGIN
    WHILE maskBits >= 8 DO
      IF addr1.a[i] # addr2.a[i] THEN RETURN FALSE END;
      INC(i);
      DEC(maskBits, 8);
    END;
    IF maskBits > 0 THEN
      WITH m = Masks[maskBits] DO
	IF Word.And(addr1.a[i], m) # Word.And(addr2.a[i], m) THEN
	  RETURN FALSE;
	END;
      END;
    END;
    RETURN TRUE;
  END Match;

(*****************************************************************************)
(* The remaining procedures are used only for debugging.                     *)
(*****************************************************************************)

PROCEDURE FmtIP(addr: IP.Address): TEXT =
(* Used only for debugging. *)
  VAR
    t := Fmt.Int(addr.a[0]);
  BEGIN
    FOR i := 1 TO LAST(addr.a) DO
      t := t & "." & Fmt.Int(addr.a[i]);
    END;
    RETURN t;
  END FmtIP;

<*UNUSED*>
PROCEDURE FmtRule(rule: Rule): TEXT =
(* Used only for debugging. *)
  VAR
    t: TEXT;
  BEGIN
    CASE rule.type OF
    | RuleType.Permit         => t := "+";
    | RuleType.PermitWithAuth => t := "*";
    | RuleType.Deny           => t := "-";
    END;
    t := t & FmtIP(rule.addr);
    t := t & "/" & Fmt.Int(rule.matchBits);
    t := t & "/" & Fmt.Int(rule.countBits);
    t := t & " " & Fmt.Int(rule.limit);
    RETURN t;
  END FmtRule;

BEGIN
END AccessRules.
