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
 * $Id: Logger.m3,v 1.1.1.1 2009-04-09 17:01:55 jkrell Exp $ *)

MODULE Logger;

IMPORT LoggerClass, Text;

REVEAL
  T = LoggerClass.Public BRANDED OBJECT
    level: Priority;
    prefix := "";
    counts := ARRAY Priority OF CARDINAL{0, ..};
    closed := FALSE;
  OVERRIDES
    init := Init;
    put := DefaultPut;
    close := DefaultClose;
  END;

  LoggerClass.Private = MUTEX BRANDED OBJECT END;

PROCEDURE Init(logger: T;
               level: Priority := Priority.Debug): T =
  BEGIN
    logger.level := level;
    RETURN logger;
  END Init;

PROCEDURE Put(logger: T;
              priority: Priority;
	      msg: TEXT) =
  BEGIN
    LOCK logger DO
      IF priority <= logger.level THEN
	logger.put(priority, logger.prefix & msg);
      END;
      INC(logger.counts[priority]);
    END;
  END Put;

PROCEDURE Indent(logger: T) =
  BEGIN
    LOCK logger DO
      logger.prefix := " " & logger.prefix;
    END;
  END Indent;

PROCEDURE Exdent(logger: T) =
  BEGIN
    LOCK logger DO
      logger.prefix := Text.Sub(logger.prefix, 1);
    END;
  END Exdent;

PROCEDURE Count(logger: T; priority: Priority): CARDINAL =
  VAR
    n: CARDINAL := 0;
  BEGIN
    LOCK logger DO
      FOR p := FIRST(Priority) TO priority DO
	INC(n, logger.counts[p]);
      END;
    END;
    RETURN n;
  END Count;

PROCEDURE CountEqual(logger: T; priority: Priority): CARDINAL =
  BEGIN
    LOCK logger DO
      RETURN logger.counts[priority];
    END;
  END CountEqual;

PROCEDURE Close(logger: T) =
  BEGIN
    LOCK logger DO
      IF NOT logger.closed THEN
	logger.close();
	logger.closed := TRUE;
      END;
    END;
  END Close;

PROCEDURE DefaultPut(<*UNUSED*> logger: T;
                     <*UNUSED*> priority: Priority;
		     <*UNUSED*> msg: TEXT) =
  BEGIN
  END DefaultPut;

PROCEDURE DefaultClose(<*UNUSED*> logger: T) =
  BEGIN
  END DefaultClose;

PROCEDURE Emerg(logger: T; msg: TEXT) =
  BEGIN
    Put(logger, Priority.Emerg, msg);
  END Emerg;

PROCEDURE Alert(logger: T; msg: TEXT) =
  BEGIN
    Put(logger, Priority.Alert, msg);
  END Alert;

PROCEDURE Crit(logger: T; msg: TEXT) =
  BEGIN
    Put(logger, Priority.Crit, msg);
  END Crit;

PROCEDURE Err(logger: T; msg: TEXT) =
  BEGIN
    Put(logger, Priority.Err, msg);
  END Err;

PROCEDURE Warning(logger: T; msg: TEXT) =
  BEGIN
    Put(logger, Priority.Warning, msg);
  END Warning;

PROCEDURE Notice(logger: T; msg: TEXT) =
  BEGIN
    Put(logger, Priority.Notice, msg);
  END Notice;

PROCEDURE Info(logger: T; msg: TEXT) =
  BEGIN
    Put(logger, Priority.Info, msg);
  END Info;

PROCEDURE Debug(logger: T; msg: TEXT) =
  BEGIN
    Put(logger, Priority.Debug, msg);
  END Debug;

BEGIN
END Logger.
