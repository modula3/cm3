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
 * $Id: SysLogger.m3,v 1.1.1.1 2009-04-09 17:02:00 jkrell Exp $ *)

UNSAFE MODULE SysLogger;

IMPORT
  ASCII, CText, Ctypes, Logger, LoggerClass, M3toC, MySyslog, Text,
  TextIntTbl, Word;

REVEAL
  T = Public BRANDED OBJECT
  OVERRIDES
    init := Init;
    put := Put;
    close := Close;
  END;

CONST
  PrioMap = ARRAY Logger.Priority OF Ctypes.int{
    MySyslog.LOG_EMERG,
    MySyslog.LOG_ALERT,
    MySyslog.LOG_CRIT,
    MySyslog.LOG_ERR,
    MySyslog.LOG_WARNING,
    MySyslog.LOG_NOTICE,
    MySyslog.LOG_INFO,
    MySyslog.LOG_DEBUG
  };

  FacilityNames = ARRAY Facility OF TEXT{
    "Kern",
    "User",
    "Mail",
    "Daemon",
    "Auth",
    "Syslog",
    "Lpr",
    "News",
    "Uucp",
    "Cron",
    "Authpriv",
    "Ftp",
    "Local0",
    "Local1",
    "Local2",
    "Local3",
    "Local4",
    "Local5",
    "Local6",
    "Local7"
  };

VAR
  FacilityTable: TextIntTbl.T;	(* CONST *)

PROCEDURE Init(self: T;
	       ident: TEXT;
               facility: Facility;
	       options := Options{};
	       level: Logger.Priority := Logger.Priority.Debug): T =
  VAR
    uident: Ctypes.char_star;
    ufacility: Ctypes.int;
    ulogopt: Ctypes.int;
  BEGIN
    EVAL Logger.T.init(self, level);
    uident := M3toC.CopyTtoS(ident);
    CASE facility OF
    | Facility.Kern     => ufacility := MySyslog.LOG_KERN;
    | Facility.User     => ufacility := MySyslog.LOG_USER;
    | Facility.Mail     => ufacility := MySyslog.LOG_MAIL;
    | Facility.Daemon   => ufacility := MySyslog.LOG_DAEMON;
    | Facility.Auth     => ufacility := MySyslog.LOG_AUTH;
    | Facility.Syslog   => ufacility := MySyslog.LOG_SYSLOG;
    | Facility.Lpr      => ufacility := MySyslog.LOG_LPR;
    | Facility.News     => ufacility := MySyslog.LOG_NEWS;
    | Facility.Uucp     => ufacility := MySyslog.LOG_UUCP;
    | Facility.Cron     => ufacility := MySyslog.LOG_CRON;
    | Facility.Authpriv => ufacility := MySyslog.LOG_AUTHPRIV;
    | Facility.Ftp      => ufacility := MySyslog.LOG_FTP;
    | Facility.Local0   => ufacility := MySyslog.LOG_LOCAL0;
    | Facility.Local1   => ufacility := MySyslog.LOG_LOCAL1;
    | Facility.Local2   => ufacility := MySyslog.LOG_LOCAL2;
    | Facility.Local3   => ufacility := MySyslog.LOG_LOCAL3;
    | Facility.Local4   => ufacility := MySyslog.LOG_LOCAL4;
    | Facility.Local5   => ufacility := MySyslog.LOG_LOCAL5;
    | Facility.Local6   => ufacility := MySyslog.LOG_LOCAL6;
    | Facility.Local7   => ufacility := MySyslog.LOG_LOCAL7;
    END;
    ulogopt := 0;
    IF Option.Pid IN options THEN
      ulogopt := Word.Or(ulogopt, MySyslog.LOG_PID);
    END;
    IF Option.Cons IN options THEN
      ulogopt := Word.Or(ulogopt, MySyslog.LOG_CONS);
    END;
    IF Option.Ndelay IN options THEN
      ulogopt := Word.Or(ulogopt, MySyslog.LOG_NDELAY);
    END;
    IF Option.Nowait IN options THEN
      ulogopt := Word.Or(ulogopt, MySyslog.LOG_NOWAIT);
    END;
    IF Option.Perror IN options THEN
      ulogopt := Word.Or(ulogopt, MySyslog.LOG_PERROR);
    END;
    MySyslog.openlog(uident, ulogopt, ufacility);
    RETURN self;
  END Init;

PROCEDURE Put(<*UNUSED*> self: T;
                         priority: Logger.Priority;
	                 msg: TEXT) =
  VAR
    msgStr := CText.SharedTtoS(msg);
  BEGIN
    MySyslog.syslog(PrioMap[priority], msgStr);
    CText.FreeSharedS(msg, msgStr);
  END Put;

PROCEDURE Close(<*UNUSED*> self: T) =
  BEGIN
    MySyslog.closelog();
  END Close;

PROCEDURE DecodeFacility(name: TEXT): Facility
  RAISES {Error} =
  VAR
    ord: INTEGER;
  BEGIN
    IF NOT FacilityTable.get(ToLower(name), ord) THEN
      RAISE Error("Invalid syslog facility \"" & name & "\"");
    END;
    RETURN VAL(ord, Facility);
  END DecodeFacility;

PROCEDURE EncodeFacility(facility: Facility): TEXT =
  BEGIN
    RETURN FacilityNames[facility];
  END EncodeFacility;

PROCEDURE ToLower(t: TEXT): TEXT =
  VAR
    len := Text.Length(t);
    chars := NEW(REF ARRAY OF CHAR, len);
  BEGIN
    Text.SetChars(chars^, t);
    FOR i := 0 TO len-1 DO
      chars[i] := ASCII.Lower[chars[i]];
    END;
    RETURN Text.FromChars(chars^);
  END ToLower;

BEGIN
  FacilityTable := NEW(TextIntTbl.Default).init(NUMBER(FacilityNames));

  FOR facility := FIRST(FacilityNames) TO LAST(FacilityNames) DO
    EVAL FacilityTable.put(ToLower(FacilityNames[facility]), ORD(facility));
  END;
END SysLogger.
